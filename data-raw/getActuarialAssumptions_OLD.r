# getActuarialAssumptions(4).r
# Don Boyd
# 5/2/2013
# minor modifications by Bob Triest, 5/19/2013

# SOA RP-2000 tables are at:
# http://www.soa.org/ccm/ content/research-publications/experience-studies-tools/the-rp-2000-mortality-tables/
# Resources more generally are at:
# http://mort.soa.org/  -- many tables
# http://www.soa.org/professional-interests/pension/resources/pen-mortality-resources.aspx
# http://www.soa.org/research/experience-study/pension/research-mortality-improve-bb.aspx


library(ggplot2) # graphics
library(XML) # so we can read html tables -- not needed for SOA tables, but will be needed for IRS tables if we use them

library(plyr) #
library(reshape2) #

#****************************************************************************************************
#
#                    Retirement    ####
#
#****************************************************************************************************
# per the AAAPC: "The format of the assumption is generally a table of rates that vary by age. The table is
# applied when an employee is projected to have met the age and service requirements for
# retirement and/or commencement of benefit payments". May have separate rates for:
# - start age for terminated vesteds,
# - start age for actives
# - start age for terminated vesteds,
# "Additionally, separate rates based on age and/or service may be applied to reflect special
# retirement eligibilities (e.g., "30 and out" and "rule of 80")."

# Several factors, some external to the pension plan, may influence an employee's decision
# to retire at a particular age and may be considered when the actuary develops a retirement
# assumption. These factors include the pension plan design (including the level of benefits
# and payment options at early and late retirement ages), the design of the employer's other
# benefit plans (including the eligibility age and amount of employer-subsidized retiree
# health and welfare benefits), and the design of other available benefits (e.g., the eligibility
# age and amount of Social Security and Medicare benefits). The actuary may find it
# reasonable to increase retirement rates at ages where subsidized benefits become
# available and reduce rates at ages immediately prior. Other employment-related factors
# may also affect retirement rates, such as physical requirements of the job and work
# environment and conditions.


#****************************************************************************************************
#
#                    Termination    ####
#
#****************************************************************************************************

# AAAPC: When plan-specific experience is not credible or available, actuaries may consider using
# the 2003 SOA Study or the V-Table instead of the T-Tables....For larger plan populations, past plan experience
# may provide a credible starting point for selecting a termination of employment assumption. However, unusual
# events (e.g., layoffs or plant closings) or trends may be factored out of the experience...

# Since termination rates generally vary by an employee's length of service, the format of
# the assumption is usually a table of rates that vary by age and service of the employees.
# The format often includes a select period with higher termination rates for the first few
# years of service in combination with an ultimate table that varies only by age. The select
# rates may or may not vary by age depending on what is determined to be reasonable.

# 1549: 2003 SOA Pension Plan Turnover Study, 2003 SOA Pension Plan Turnover Study, The Select and Ultimate Table
# has a column ages 18-60, with 2003 SOA Pension Plan Turnover Study, The Select and Ultimate Table, Turnover per 100 Lives, Age Nearest Birthday, Service < 2, Select
# then, below that, a column, ages 20-60, Service = 2, 3, 4, Select,
# then 22-60, service 5-9, select
# then 28-60, 10+, select
# then overall 18-60, Ultimate
# the csv file labels these as tables 1-5

# get the 5 tables from the SOA turnover study and put them into a data frame
urlpre<-"http://mort.soa.org/Export.aspx?Type=csv&TableIdentity="
at<-read.csv(paste0(urlpre,1549))
names(at)<-c("age","qx")
str(at)
# find the starting point for each table
(tabstarts<-which(at[,1]=="Row\\Column")+1)
(tabends<-c(tabstarts[2:length(tabstarts)],nrow(at)))
at[tabstarts,]
# create a long data frame
tabnames<-c("selectsvc01","selectsvc02to04","selectsvc05to09","selectsvc10p","ultimate")
getdf<-function(tabnum){
  df<-at[tabstarts[tabnum]:tabends[tabnum],]
  df$tabname<-tabnames[tabnum]
  df$age<-as.numeric(as.character(df$age))
  df$qx<-as.numeric(as.character(df$qx))
  df<-subset(df,!(is.na(age) | is.na(qx)))
  return(df)
}
tmp<-ldply(1:5,getdf)
(termtab<-dcast(tmp,age~tabname,value.var="qx",sum,fill=NA_real_))



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

# get an actuarial table from the SOA website
getat<-function(atnum,attype){
  at<-read.csv(paste0(urlpre,atnum),fileEncoding="cp1252") # fileEncoding added by Bob to make compatible with Mac OS X
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








######################################################################################################
######################################################################################################
#                    IGNORE EVERYTHING BELOW HERE FOR NOW    ####
######################################################################################################
######################################################################################################


#****************************************************************************************************
#
#                    Mortality tables from the IRS    ####
#
#****************************************************************************************************

# IRS has computer-readable tables:
# Internal Revenue Bulletin: 2008-42 October 20, 2008, Notice 2008-85
# Updated Static Mortality Tables for the Years 2009 Through 2013
# These regulations provide for mortality tables, based on the tables contained in the RP-2000 Mortality Tables
# Report [SOA], adjusted for mortality improvement using Projection Scale AA as recommended in that report.

irsurl<-"http://www.irs.gov/irb/2008-42_IRB/ar09.html" # this has

tables<-readHTMLTable(irsurl) # read tables from the web site
# str(tables)
(nrows<-unlist(lapply(tables, function(t) dim(t)[1])))
# class(nrows); str(nrows)
# define column names - each table follows this format
cnames<-c("age","malenonann","maleann","maleoptional","femalenonann","femaleann","femaleoptional","unisex")
idx<-which(nrows==max(nrows)) # find the elements of tables that have the most rows - these are what we want
mt2009<-as.data.frame(tables[idx[1]])
names(mt2009)
names(mt2009)<-c("age",)
mt2009[1:3,1:3]

# uitab<-max(which.max(nrows),2)
idx[1]
str(tables[idx[[1]]])

mtable<-(tables[[3]])  # get table (i.e., dataframe) with most rows
names(uiloans)<-c("stname","value")
uiloans$value<-ctov(uiloans$value)
uiloans<-subset(uiloans, !is.na(value))
uiloans$stname<-as.character(uiloans$stname)

#****************************************************************************************************
#
#                    Mortality tables from www.pensionsoft.com    ####
#
#****************************************************************************************************
# http://www.pensionsoft.com/references_mort_other.html
psu<-"http://www.pensionsoft.com/mortality/" # pensionsoft mortality table url
# Here are the tables on the site [tname, desciption] - file names of form: mort_<tname>.csv -- for example mort_1971IAM.csv:
# 1971IAM, 1971 IAM (Basic, Sex-distinct)
# 1971GAM, 1971 GAM (Basic, Sex-distinct)
# 1979 US Buck Mortality
# 1983 GAM (Basic, Sex Distinct)
# 1983 GAM (50%/50% Blended, "Applicable Mortality" per IRS Revenue Ruling 95-6)
# 1984 UP (UP84, prior PBGC lump sum mortality)
# 1983 US GAM Basic projected to 1988 (using Scale H)
# 1989 US Buck Mortality
# 1994 GAM (Basic, Sex Distinct)
# 1994 UP (UP94)
# 1994 GAR (sex distinct)
# 1994 GAR @ 2002 (GATT Mortality)
# RP-2000 Disabled Retiree Mortality
# RP-2000 Employee Mortality
# RP-2000 Healthy Annuitant Mortality
# RP-2000 Combined Healthy Mortality

tname<-"1971IAM"
tbl<-read.csv(paste0(psu,"mort_",tname,".csv"))
tbl<-tbl[,1:3]
names(tbl)<-c("age","qxmale","qxfemale")
tbl$tname<-tname
tbll<-melt(tbl,id=c("age","tname"))
tbll$value<-as.numeric(tbll$value)
tbll<-subset(tbll,!is.na(value))
head(tbll)
qplot(age,value,data=tbll,colour=variable,geom=c("point","line"))



