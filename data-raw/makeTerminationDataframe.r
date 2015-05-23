# 5/22/2015

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
dterm <- paste0(draw, "termination/")

wvfn <- "Winklevoss(6).xlsx"



#****************************************************************************************************
#                    Read Winklevoss term rates  ####
#****************************************************************************************************

twv <- read_excel(paste0(draw, wvfn), sheet="Tab2-3TermRates", skip=2)
glimpse(twv)

twvl <- twv %>% gather(ea, qx.t, -agex) %>%
  rename(age=agex) %>%
  mutate(age=as.integer(age), ea=as.integer(str_trim(as.character(ea))), tablename="Winklevoss") %>%
  filter(!is.na(qx.t)) %>%
  select(tablename, age, ea, qx.t)

#****************************************************************************************************
#                    Read Winklevoss term rates - Yimeng  ####
#****************************************************************************************************
library(xlsx)

# data table 2-3 termination rates ####

term <- read.xlsx(paste0(draw, wvfn), sheetName = "Tab2-3TermRates", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
vnames <- c("age", paste0("ea", seq(20, 60, 5)))
names(term)[1:length(vnames)] <- vnames
term <- term[, vnames]
term <- term %>% mutate_each(funs(cton)) %>%
  filter(!is.na(age))
# term contain the original termination table in winkelvoss

# Now we expand the termination rates to all entry ages, assuming the rates are the same within each 5-year interval

# First, fill up all 0 cells
term2 <- term
term2[36:40, 2:7] <- term2[36:40, 8]
term2[41:45, 2:8] <- term2[41:45, 9]

# Then reorganize termination table into long format
term2 %<>%
  gather(ea, qxt.p, -age) %>%
  mutate(ea = as.numeric(gsub("[^0-9]", "", ea)),
         qxt.p=ifelse(is.na(qxt.p), 0, qxt.p))

# Fill up termination rates for all missing entry ages.
term3 <-  expand.grid(age = 20:64, ea = 20:64) %>%
  mutate(ea.match = floor(ea*2/10)/2*10,
         yos = age - ea) %>%
  left_join(term2 %>% rename(ea.match = ea)) %>%
  filter(yos >= 0) %>%
  select(-ea.match)

# check the result
term3 %>% select(-yos) %>% spread(ea, qxt.p)

term4 <- term3 %>% mutate(tablename="Winklevoss") %>%
  select(tablename, age, ea, qxt.p) %>%
  arrange(age, ea)


#****************************************************************************************************
#                    Read other term rates (TO COME) ####
#****************************************************************************************************


#****************************************************************************************************
#                    Combine tables and save to data frame ####
#****************************************************************************************************

termination <- term4
use_data(termination, overwrite=TRUE)

