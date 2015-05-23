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
  filter(!is.na(qx.t))


#****************************************************************************************************
#                    Read other term rates (TO COME) ####
#****************************************************************************************************


#****************************************************************************************************
#                    Combine tables and save to data frame ####
#****************************************************************************************************

termination <- twvl
use_data(termination, overwrite=TRUE)

