# 5/27/2015

# Here's what we want in the final data frame
# {tablename}{Unique name identifying the mortality table - ordinarily will select on this, character}
# {SOA unique table number if this table is on the SOA site (rather than constructed), integer}
# {memtype}{Purpose of the table: c("employee", "annuitant", "disabled", "hybrid")}
# {collar}{c("allcollars", "blue", "white")}
# {sex}{Male or female, c("male", "female", "unisex", "female75"), character}
#       unisex is 50% male, 50% female
#       female75 is 75% female, 25% male
# {age}{Attained age, integer}
# {qxm}{Rate of mortality at age x, numeric}


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
# download.file("http://mort.soa.org/Export.aspx?Type=xlsx&TableIdentity=818", paste0(dmort, "818_gam1971_male_hybrid_allcollars_1971.xlsx"), mode="wb")




#****************************************************************************************************
#                    Read, combine, and save selected SOA mortality tables ####
#****************************************************************************************************
# note that tlist from above is needed
tlist

# make a big data frame of SOA tables
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

soatabs <- tlist %>% group_by(tid, series, sex, memtype, collar, year) %>%
  do(gettab(.$fname))

glimpse(soatabs)
count(soatabs, tid)
saveRDS(soatabs, paste0(dmort, "soatabs.rds"))


#****************************************************************************************************
#                    Construct any needed hybrid tables ####
#****************************************************************************************************
# make sure hybrid tables run from age 20 to 120

soatabs <- readRDS(paste0(dmort, "soatabs.rds"))

# create hybrid rp2000 table from 1594, 1595, 1597, 1598 into a
# first combine annuitants / employees within sex
# then combine sex
rp2k <- soatabs %>% filter(tid %in% c(1594, 1595, 1597, 1598), !is.na(age)) %>%
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
         qxm=ifelse(is.na(male), female, ifelse(is.na(female), male, (male + female) / 2))) %>%
  select(-male, -female)
rp2k %>% data.frame
saveRDS(rp2k, paste0(dmort, "rp2k.rds"))


rp2k.f75 <- soatabs %>% filter(tid %in% c(1594, 1595, 1597, 1598), !is.na(age)) %>%
  ungroup %>%
  select(year, sex, age, memtype, value) %>%
  # average annuitant and employee member types
  spread(memtype, value) %>%
  mutate(value=ifelse(is.na(annuitant), employee, ifelse(is.na(employee), annuitant, (employee + annuitant) / 2))) %>%
  # now average male and female
  select(year, sex, age, value) %>%
  spread(sex, value) %>%
  mutate(tablename="rp2000.hybrid.f75",
         series="rp2000",
         memtype="hybrid",
         collar="allcollars",
         sex="female75",
         qxm=ifelse(is.na(male), female, ifelse(is.na(female), male, male*.25 + female*.75))) %>%
  select(-male, -female)
rp2k.f75 %>% data.frame
saveRDS(rp2k.f75, paste0(dmort, "rp2k.f75.rds"))


rp2k.f90 <- soatabs %>% filter(tid %in% c(1594, 1595, 1597, 1598), !is.na(age)) %>%
  ungroup %>%
  select(year, sex, age, memtype, value) %>%
  # average annuitant and employee member types
  spread(memtype, value) %>%
  mutate(value=ifelse(is.na(annuitant), employee, ifelse(is.na(employee), annuitant, (employee + annuitant) / 2))) %>%
  # now average male and female
  select(year, sex, age, value) %>%
  spread(sex, value) %>%
  mutate(tablename="rp2000.hybrid.f90",
         series="rp2000",
         memtype="hybrid",
         collar="allcollars",
         sex="female90",
         qxm=ifelse(is.na(male), female, ifelse(is.na(female), male, male*.10 + female*.90))) %>%
  select(-male, -female)
rp2k.f90 %>% data.frame
saveRDS(rp2k.f90, paste0(dmort, "rp2k.f90.rds"))


# get gam1971
gam1971base <- soatabs %>% filter(tid %in% 818, !is.na(age)) %>%
  mutate(tablename="gam1971.hybrid") %>%
  rename(qxm=value)
# add ages 110-120, with qxm=1
onerec <- gam1971base %>% filter(age==110) %>% mutate(qxm=1) %>% select(-age)
stackrecs <- function(x) return(onerec)
gam1971addon <- data.frame(age=111:120) %>% group_by(age) %>% do(stackrecs(.$age))
gam1971 <- bind_rows(gam1971base, gam1971addon)
saveRDS(gam1971, paste0(dmort, "gam1971.rds"))


#****************************************************************************************************
#                    Get RP-2014 tables ####
#****************************************************************************************************
# https://www.soa.org/Research/Experience-Study/pension/research-2014-rp.aspx
# as with RP-2000, combine the employee and healthy annuitant tables
rp2014fn <- "research-2014-rp-mort-tab-rates-exposure.xlsx"
range <- "B5:J107" # read entire table and select what we want
(df <- readWorksheetFromFile(paste0(dmort, rp2014fn), sheet="Total Dataset", header=FALSE, region=range, colTypes="character"))
names(df) <- c("age", "junk1", "employee.male", "annuitant.male", "disabled.male", "junk2", "employee.female", "annuitant.female", "disabled.female")

wmean <- function(col1, col2, wt1=0.5) {
  # return the weighted average of col1 and col2 - but if one is missing, use the other
  # define the possibilities
  bothmiss <- is.na(col1) & is.na(col2)
  nomiss <- !(is.na(col1) | is.na(col2))
  onlymiss1 <- is.na(col1) & !is.na(col2)
  onlymiss2 <- is.na(col2) & !is.na(col1)
  vwt1 <- ifelse(nomiss, wt1, ifelse(onlymiss2, 1, 0))
  vwt2 <- ifelse(nomiss, 1-wt1, ifelse(onlymiss1, 1, 0))
  naz <- function(vec) ifelse(is.na(vec), 0, vec)
  wtdcol <- vwt1 * naz(col1) + vwt2 * naz(col2)
  return(wtdcol)
}

keepvars <- c("age", "employee.male", "annuitant.male", "employee.female", "annuitant.female")
rp2014 <- df %>% select(one_of(keepvars)) %>%
  mutate_each(funs(cton)) %>%
  mutate(male=wmean(employee.male, annuitant.male),
         female=wmean(employee.female, annuitant.female),
         qxm=wmean(male, female)) %>%
  mutate(tablename="rp2014.hybrid",
         series="rp2014",
         year=2014,
         memtype="hybrid",
         collar="allcollars",
         sex="unisex") %>%
  select(tablename, series, memtype, collar, sex, year, age, qxm)

saveRDS(rp2014, paste0(dmort, "rp2014.rds"))



#****************************************************************************************************
#                    Combine tables and save to data frame ####
#****************************************************************************************************

mortlist <- c("gam1971", "rp2k", "rp2k.f75", "rp2k.f90", "rp2014")

getmort <- function(tname) readRDS(paste0(dmort, tname, ".rds"))

mortality <- ldply(mortlist, getmort) %>%
  # put variables in the desired order
  select(tablename, tid, series, memtype, collar, sex, year, age, qxm) %>%
  arrange(tablename, tid, series, memtype, collar, sex, year, age)

use_data(mortality, overwrite=TRUE)


tmp <- mortality %>% filter(age>=20) %>%
  group_by(tablename) %>%
  mutate(cqx=cumprod(1-qxm)) %>%
  ungroup
qplot(age, cqx, data=tmp, colour=tablename, geom=c("point", "line"), ylab="cqx=cumprod(1-qxm)")








