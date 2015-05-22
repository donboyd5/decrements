# 5/22/2015

# Not yet started...

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
dproj <- paste0(draw, "projection/")



#****************************************************************************************************
#                    RUN ONCE: Get projection scale tables tables from the Society of Actuaries    ####
#                    Only repeat when getting new data
#****************************************************************************************************


# Projection scales:
# 923 1994 Mortality Improvement Projection Scale AA - Female
# 924 1994 Mortality Improvement Projection Scale AA - Male
# 1511 Interim Mortality Improvement Scale BB - Male
# 1512 Interim Mortality Improvement Scale BB - Female






