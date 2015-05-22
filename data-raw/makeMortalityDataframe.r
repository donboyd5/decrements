# 5/22/2015

# SOA RP-2000 tables are at:
# http://www.soa.org/ccm/ content/research-publications/experience-studies-tools/the-rp-2000-mortality-tables/
# Resources more generally are at:
# http://mort.soa.org/  -- many tables
# http://www.soa.org/professional-interests/pension/resources/pen-mortality-resources.aspx
# http://www.soa.org/research/experience-study/pension/research-mortality-improve-bb.aspx

# create a data frame with multiple mortality tables
# each one to have the following fields
#   tablename, character
#   series, character:  rp2000,
#   usage, character: employee, annuitant, hybrid
#   sex, character: male, female, unisex, female75


# packages I always want loaded
library(plyr) # always load BEFORE loading dplyr
library(dplyr)
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library(foreign) # various import and export routines - e.g., for reading Stata files
library(gdata) # for reading spreadsheets
library(knitr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

library(xlsx)

library(devtools)


# load my packages last
# devtools::install_github("donboyd5/apitools")
# devtools::install_github("donboyd5/bdata")
# devtools::install_github("donboyd5/btools")
# devtools::install_github("donboyd5/pdata")
library(apitools)
library(bdata)
library(btools)
# library(pdata)

draw <- "./data-raw/"
dmort <- paste0(draw, "mortality/")


#****************************************************************************************************
#                    RUN ONCE: Download mortality tables from the Society of Actuaries    ####
#                    Only repeat when getting new data
#****************************************************************************************************
# http://mort.soa.org/
# go to soa site and look at the tables

# Key mortality tables appear to be:
# Mortality: (a) projected to 2000, (b) separate males and females, and (c) separate employees and annuitants
# 1594, 1595 are male employees, annuitants to 2000
# 1596 RP-2000 Mortality Table - Male Aggregate - Disabled Retiree
# 1597 1598 are female employees, annuitants to 2000
# 1599 RP-2000 Mortality Table - Female Aggregate - Disabled Retiree


# make a list of SOA tables that we want
tlist.s <- "tid, series, sex, memtype, collar, year
818, gam1971, male, hybrid, allcollars, 1971
1594, rp2000, male, employee, allcollars, 2000
1595, rp2000, male, annuitant, allcollars, 2000
1596, rp2000, male, disabled, allcollars, 2000
1597, rp2000, female, employee, allcollars, 2000
1598, rp2000, female, annuitant, allcollars, 2000
1599, rp2000, female, disabled, allcollars, 2000
"
tlist <- read_csv(tlist.s) %>% mutate_each(funs(str_trim))
names(tlist) <- str_trim(names(tlist))
tlist <- tlist %>% mutate(fname=paste(tid, series, sex, memtype, collar, year, sep="_"),
                          fname=paste0(fname, ".xlsx"),
                          year=as.integer(year),
                          tid=as.integer(tid))
tlist

for(tbl in tlist$tid) {
  url <- paste0("http://mort.soa.org/Export.aspx?Type=xlsx&TableIdentity=", tbl)
  tmp <- filter(tlist, tid==tbl)
  download.file(url, paste0(dmort, tmp$fname), mode="wb")
}


# alternatively, modify this code to download a single table
# 818	1971 GAM - Male
# 1971 Group Annuity Mortality (GAM) Table – Male. Minimum Age: 5 Maximum Age: 110
# this is the Winkelvoss table
download.file("http://mort.soa.org/Export.aspx?Type=xlsx&TableIdentity=818", paste0(dmort, "818_gam1971_male_hybrid_allcollars_1971.xlsx"), mode="wb")




#****************************************************************************************************
#                    Read and combine mortality tables ####
#****************************************************************************************************
# note that tlist from above is needed
tlist

# make a big data frame
gettab <- function(fname) {
  df <- read.xlsx(paste0(dmort, fname), sheetIndex = 1)
  names(df)[1:2] <- c("age", "value")
  df2 <- df %>% mutate_each(funs(cton)) %>%
    select(age, value) %>%
    filter(!is.na(age) | (is.na(age) & value==1)) %>%
    mutate(age=as.integer(age)) %>%
    unique
  return(df2)
}

dfall <- tlist %>% group_by(tid, series, sex, memtype, collar, year) %>%
  do(gettab(.$fname))

glimpse(dfall)
count(dfall, tid)


#****************************************************************************************************
#                    Construct any needed hybrid tables ####
#****************************************************************************************************
# make sure hybrid tables run from age 20 to 120


# create hybrid rp2000 table from 1594, 1595, 1597, 1598 into a
# first combine annuitants / employees within sex
# then combine sex
rp2k <- dfall %>% filter(tid %in% c(1594, 1595, 1597, 1598), !is.na(age)) %>%
  ungroup %>%
  select(year, sex, age, memtype, value) %>%
  # average annuitant and employee member types
  spread(memtype, value) %>%
  mutate(value=ifelse(is.na(annuitant), employee, ifelse(is.na(employee), annuitant, (employee + annuitant) / 2))) %>%
  # now average male and female
  select(year, sex, age, value) %>%
  spread(sex, value) %>%
  mutate(tablename="rp2000.hybrid",
         series="rp2000",
         memtype="hybrid",
         collar="allcollars",
         sex="unisex",
         qx.m=ifelse(is.na(male), female, ifelse(is.na(female), male, (male + female) / 2))) %>%
  select(-male, -female)
rp2k %>% data.frame


# get gam1971
gam1971base <- dfall %>% filter(tid %in% 818, !is.na(age)) %>%
  mutate(tablename="gam1971.hybrid") %>%
  rename(qx.m=value)
# add ages 110-120, with qx.m=1
onerec <- gam1971base %>% filter(age==110) %>% mutate(qx.m=1) %>% select(-age)
stackrecs <- function(x) return(onerec)
gam1971addon <- data.frame(age=111:120) %>% group_by(age) %>% do(stackrecs(.$age))
gam1971 <- bind_rows(gam1971base, gam1971addon)



#****************************************************************************************************
#                    Select desired tables and save mortality data frame ####
#****************************************************************************************************
# Here's what we want in the final data frame
# \item{tablename}{Unique name identifying the mortality table - ordinarily will select on this, character}
# \tid{SOA unique table number if this table is on the SOA site (rather than constructed), integer}
# \item{series}{Level of government: 1, 2, or 3, for State-local, State, or local, numeric}
# \item{memtype}{Purpose of the table: c("employee", "annuitant", "disabled", "hybrid")}
# \item{collar}{c("allcollars", "blue", "white")}
# \item{sex}{Male or female, c("male", "female", "unisex", "female75"), character}
#       unisex is 50% male, 50% female
#       female75 is 75% female, 25% male
# \item{age}{Attained age, integer}
# \item{qx.m}{Rate of mortality at age x, numeric}


glimpse(gam1971)
glimpse(rp2k)
mortality <- bind_rows(gam1971, rp2k) %>%
  select(tablename, tid, series, memtype, collar, sex, year, age, qx.m) %>%
  arrange(tablename, tid, series, memtype, collar, sex, year, age)

glimpse(mortality)
count(mortality, tablename, tid, series, memtype, collar, sex, year)
mortality %>% select(tablename, age, qx.m) %>% spread(tablename, qx.m) %>% data.frame

use_data(mortality, overwrite=TRUE)

