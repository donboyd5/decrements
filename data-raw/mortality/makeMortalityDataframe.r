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
#
#                    Mortality tables from the Society of Actuaries    ####
#
#****************************************************************************************************
# http://mort.soa.org/
# go to soa site and look at the tables

# Key tables appear to be:
# Mortality: (a) projected to 2000, (b) separate males and females, and (c) separate employees and annuitants
# 1594, 1595 are male employees, annuitants to 2000
# 1596 RP-2000 Mortality Table - Male Aggregate - Disabled Retiree
# 1597 1598 are female employees, annuitants to 2000
# 1599 RP-2000 Mortality Table - Female Aggregate - Disabled Retiree

# Projection scales:
# 923 1994 Mortality Improvement Projection Scale AA - Female
# 924 1994 Mortality Improvement Projection Scale AA - Male
# 1511 Interim Mortality Improvement Scale BB - Male
# 1512 Interim Mortality Improvement Scale BB - Female



# make a list of SOA tables that we want
tlist.s <- "tid, series, sex, memtype, collar, year
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
                          fname=paste0(fname, ".xlsx"))
tlist

for(tbl in tlist$tid) {
  url <- paste0("http://mort.soa.org/Export.aspx?Type=xlsx&TableIdentity=", tbl)
  tmp <- filter(tlist, tid==tbl)
  download.file(url, paste0(dmort, tmp$fname), mode="wb")
}

# now make a big data frame
gettab <- function(fname) {
  df <- read.xlsx(paste0(dmort, fname), sheetIndex = 1)
  names(df)[1:2] <- c("age", "value")
  df2 <- df %>% mutate_each(funs(cton)) %>%
    select(age, value) %>%
    filter(!is.na(age) | (is.na(age) & value==1)) %>%
    unique
  return(df2)
}


dfall <- tlist %>% group_by(tid, series, sex, memtype, collar, year) %>%
  do(gettab(.$fname))

mortality <- dfall
use_data(mortality, overwrite=TRUE)

# combine 1594, 1595, 1597, 1598 into a hybrid rp2000 table
# first combine annuitants / employees within sex
# then meld sex
rp2k <- dfall %>% filter(tid %in% c(1594, 1595, 1597, 1598)) %>%
  ungroup %>%
  select(sex, age, memtype, value) %>%
  # average member types
  spread(memtype, value) %>%
  mutate(value=ifelse(is.na(annuitant), employee, ifelse(is.na(employee), annuitant, (employee + annuitant) / 2))) %>%
  # now average male/female
  select(sex, age, value) %>%
  spread(sex, value) %>%
  mutate()


#****************************************************************************************************
#                    Download mortality tables from the Society of Actuaries    ####
#****************************************************************************************************
# 818	1971 GAM - Male
# 1971 Group Annuity Mortality (GAM) Table – Male. Minimum Age: 5 Maximum Age: 110
# this is the Winkelvoss table
download.file("http://mort.soa.org/Export.aspx?Type=xlsx&TableIdentity=818", paste0(ddir, "gam1971_male.xlsx"), mode="wb")

# 985	RP-2000 - 1992 Base Mortality Table – Male Aggregate - Employee
# Retirement Plan (RP) - 2000 Mortality Table - Male Aggregate - Employee, 1992 Base Year Rates (Table 3-1). Minimum Age: 1. Maximum Age: 70


# 1971 Group Annuity Mortality (GAM) Table – Male. Minimum Age: 5 Maximum Age: 110
download.file("http://mort.soa.org/Export.aspx?Type=xlsx&TableIdentity=818", paste0(ddir, "gam1971_male.xlsx"), mode="wb")


employee	annuitant (healthy)	disabled retiree






# get an actuarial table from the SOA website
getat <- function(atnum, attype) {
  at <- read.csv(paste0(urlpre, atnum))
  at<-at[,1:2]
  names(at)<-c("age","value")
  at$age<-as.numeric(as.character(at$age)) # convert from factor to numeric
  at$value<-as.numeric(as.character(at$value))
  at<-subset(at,!is.na(age) & !is.na(value)) # this will keep the relevant data and nothing else
  rownames(at)<-NULL
  at$tnum<-atnum
  at$ttype<-attype
  at<-at[,c(3,4,1,2)]
  return(at)
}

fn <- "GAM-1971-Male.xls"
gam1971 <- read.xls(paste0(wvd, fn), colClasses="character")
ht(gam1971)
dim(gam1971)
names(gam1971) <- c("age", "qxm")
gam1971 <- gam1971 %>% mutate_each(funs(cton)) %>%
  filter(age %in% 5:110)

# get the needed mortality tables
memt<-getat(1594,"Male employees, proj to 2000")
mamt<-getat(1595,"Male annuitants, proj to 2000")
femt<-getat(1597,"Female employees, proj to 2000")
famt<-getat(1598,"Female annuitants, proj to 2000")

# calculate unisex tables
# employees
fpct<-.51 # define female percent
uemt<-merge(memt[,c("age","value")],femt[,c("age","value")],by="age",suffixes=c(".m",".f"))
uemt$qx<-uemt$value.m*(1-fpct)+uemt$value.f*fpct # note that this assumes there are no missing values - be sure
uemt
# annuitants
fpct<-.51 # define female percent
uamt<-merge(mamt[,c("age","value")],famt[,c("age","value")],by="age",suffixes=c(".m",".f"))
uamt$qx<-uamt$value.m*(1-fpct)+uamt$value.f*fpct # note that this assumes there are no missing values - be sure
uamt
save(uemt,uamt,termtab,file="soa.rda")

qplot(age, value,data=melt(uemt,id="age"), colour=variable, geom=c("point","line"))
qplot(age, value,data=melt(uamt,id="age"), colour=variable, geom=c("point","line"))

# get the projection scales
aafscale<-getat(923,"1994 Mortality Improvement Projection Scale AA - Female")
aamscale<-getat(924,"1994 Mortality Improvement Projection Scale AA - Male")
bbfscale<-getat(1512,"Interim Mortality Improvement Scale BB - Female")
bbmscale<-getat(1511,"Interim Mortality Improvement Scale BB - Male")


# check how projection works -- compare to 10-year projection in Appendix G of the RP-2000 document
# male
tmp<-merge(uemt[,1:2],aamscale[,3:4])
tmp$proj<-round(tmp$value.m*((1-tmp$value)^10),6)
tmp # good - matches reported value

# female
tmp<-merge(uemt[,c("age","value.f")],aafscale[,3:4])
tmp$proj<-round(tmp$value.f*((1-tmp$value)^10),6)
tmp # good - matches reported value








