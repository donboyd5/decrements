

# Per Yimeng 5/23/2015, we use a consistent naming rule for the column of decrement values:
# - mortality:    qxm
# - termination: qxt
# - disability   : qxd
# - retirement:  qxr (since we use single retirement age for now, the retirement table will be generated by the model based on the specified r.max)
# The variable names without suffix indicate multiple decrement rates (rates taking into account other competing decrements), and suffix ".p" indicates single decrement rates. Later we may have other suffix to indicate mortality for actives, retirees and disabled. For now, I think we can assume the all decrement tables we have already contain multiple decrement rates (just as what we will typically get from AVs when modeling real plans), so we can just use variable names without suffix. Please let me know whether you think this makes sense. Thank you!


#****************************************************************************************************
#                    Load packages ####
#****************************************************************************************************

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