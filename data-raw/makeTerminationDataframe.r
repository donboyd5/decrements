# 5/28/2015

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

# DJB:
# EVERY age-ea combo must be filled ea 20:64, age 20:64 where age >=ea


#****************************************************************************************************
#                    Read Winklevoss term rates  ####
#****************************************************************************************************
range <- "A6:J51"
(term <- readWorksheetFromFile(paste0(dterm, ttfn), sheet="WinklevossTab2-3.Term", header=TRUE, region=range, colTypes="numeric"))

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

saveRDS(term4, paste0(dterm, "term.wv.rds"))

# qplot(age, qxt, data=term4, stat="summary", fun.y = "mean", geom=c("point", "line"))
count(term4, ea, age) %>% data.frame


#****************************************************************************************************
#                    AZ-SRS term rates ####
#****************************************************************************************************
range <- "B3:D24"
(df <- readWorksheetFromFile(paste0(dterm, ttfn), sheet="AZ-SRS.Term", header=TRUE, region=range, colTypes="numeric"))

# estimate age, combine male and female, add ages 41-64, and then repeat for all ea 20:64 (where age>=ea)
df2 <- df %>% mutate(age=yos+20,
                     qxt=(male + female) / 2) %>%
  select(age, qxt)

# fill in ages 41+
df3 <- bind_rows(df2, data.frame(age=41:64)) %>%
  mutate(age=as.integer(age),
         qxt=ifelse(age>40, qxt[age==40], qxt)) %>%
  arrange(age)
qplot(age, qxt, data=df3, geom=c("point", "line"))

# no missing ages, so spline not necessary
# df4 <- splong2(df3, "age")

# fatten this up with values for each ea
df4 <- expand.grid(ea=20:64, age=20:64) %>%
  filter(age >= ea) %>%
  left_join(df3) %>%
  mutate(tablename="az-srs")
qplot(age, qxt, data=df4, stat="summary", fun.y = "mean", geom=c("point", "line"))

saveRDS(df4, paste0(dterm, "term.az-srs.rds"))


#****************************************************************************************************
#                    PA-PSERS term rates ####
#****************************************************************************************************
range <- "A5:E21"
(df <- readWorksheetFromFile(paste0(dterm, ttfn), sheet="PA-PSERS.Term", header=TRUE, region=range, colTypes="character"))

df2 <- df %>% mutate_each(funs(cton), -sex) %>%
  mutate(wsum=(nvw + vwlt10 + vw10p) / 100) %>%
  select(sex, age, wsum) %>%
  spread(sex, wsum) %>%
  mutate(qxt=(male + female) / 2) %>%
  select(age, qxt)

# fill in ages 20, 64
df3 <- bind_rows(df2, data.frame(age=c(20, 64))) %>%
  mutate(age=as.integer(age),
         qxt=ifelse(age==20, qxt[age==25], qxt),
         qxt=ifelse(age==64, qxt[age==60], qxt)) %>%
  arrange(age)

qplot(age, qxt, data=df3, geom=c("point", "line"))

# fill in missing ages
df4 <- splong2(df3, "age")
qplot(age, qxt, data=df4, geom=c("point", "line"))

# fatten this up with values for each ea
df5 <- expand.grid(ea=20:64, age=20:64) %>%
  filter(age >= ea) %>%
  left_join(df4) %>%
  mutate(tablename="pa-psers")
qplot(age, qxt, data=df5, stat="summary", fun.y = "mean", geom=c("point", "line"))

saveRDS(df5, paste0(dterm, "term.pa-psers.rds"))


#****************************************************************************************************
#                    Combine tables and save to data frame ####
#****************************************************************************************************

termlist <- c("term.wv", "term.az-srs", "term.pa-psers")

getterm <- function(tname) readRDS(paste0(dterm, tname, ".rds"))

termination <- ldply(termlist, getterm)

qplot(age, qxt, data=termination, colour=tablename, stat="summary", fun.y = "mean", geom=c("point", "line"))


use_data(termination, overwrite=TRUE)


qplot(age, qxt, data=termination, colour=tablename, stat="summary", fun.y = "mean", geom=c("point", "line"))







