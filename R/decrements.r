#' Decrement tables for analysis of public pension plans.
#'
#' Currently includes:\cr
#' \code{\link{motality}} Various mortality tables in a single data frame\cr
#'
#' @docType package
#' @name decrements
NULL





# Create an RData file with decrements that we need

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




