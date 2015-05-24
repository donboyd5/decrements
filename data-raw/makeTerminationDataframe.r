# 5/24/2015

#****************************************************************************************************
#                    Termination table rules per 5/23/2015 email from Yimeng ####
#****************************************************************************************************

# 1) The data frame should include 4 columns:
# - tablename
# - age
# - ea
# - qxt
# 2) Range of age and ea:
# - min age and ea is 20, max age and ea is 64.(if we want to set the retirement age r.max greater than 65, the max age and ea should be r.max - 1)
# - only includes combos with age >= ea

#****************************************************************************************************
#                    Load packages ####
#****************************************************************************************************

library(devtools)
library(plyr) # always load BEFORE loading dplyr
library(dplyr)
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library(knitr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(xlsx)


library(btools)


#****************************************************************************************************
#                    Constants, directories, etc  ####
#****************************************************************************************************

draw <- "./data-raw/"
dterm <- paste0(draw, "termination/")

wvfn <- "Winklevoss(6).xlsx"


#****************************************************************************************************
#                    Read Winklevoss term rates - Yimeng  ####
#****************************************************************************************************

# data table 2-3 termination rates ####

# term <- read.xlsx(paste0(draw, wvfn), sheetName = "Tab2-3TermRates", colClasses = "character", startRow = 3, stringsAsFactors = FALSE)
term <- read_excel(paste0(draw, wvfn), sheet="Tab2-3TermRates", skip=2)
glimpse(term)

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
  gather(ea, qxt, -age) %>%
  mutate(ea = as.numeric(gsub("[^0-9]", "", ea)),
         qxt=ifelse(is.na(qxt), 0, qxt))

# Fill up termination rates for all missing entry ages.
term3 <-  expand.grid(age = 20:64, ea = 20:64) %>%
  mutate(ea.match = floor(ea*2/10)/2*10,
         yos = age - ea) %>%
  left_join(term2 %>% rename(ea.match = ea)) %>%
  filter(yos >= 0) %>%
  select(-ea.match)

# check the result
term3 %>% select(-yos) %>% spread(ea, qxt)

term4 <- term3 %>% mutate(tablename="Winklevoss") %>%
  select(tablename, age, ea, qxt) %>%
  arrange(age, ea)

term4 %>% filter(age<ea) # should be no records


#****************************************************************************************************
#                    Read other term rates (TO COME) ####
#****************************************************************************************************


#****************************************************************************************************
#                    Combine tables and save to data frame ####
#****************************************************************************************************

termination <- term4
use_data(termination, overwrite=TRUE)






