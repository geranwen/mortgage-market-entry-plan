#####
## 1. Environment setup

# rm(list=ls())
library(dplyr)
library(ggplot2)
library(gridExtra)


df1_fctr <- read.csv("dataset_Fall 2021.csv", header = TRUE, stringsAsFactors = TRUE)
# df1_str <- read.csv("dataset_Fall 2021.csv", header = TRUE, stringsAsFactors = FALSE)
df2 <- df1_fctr

#####
## 2. Cleanup data

date_cols = c(2,13,14,30,32,34,36,38,40,41) #all the cols that should be dates. 
df2[,date_cols] <- lapply(df2[, date_cols], as.Date)
df3 <- df2 %>%
  mutate(msa = as.factor(msa))

str(df3)
summary(df3)
save(df3, file = 'df3.Rdata')

#####
## 3. Exploratory Data Analysis (EDA)

# How many are XXX days delinquent
df3_30day<- df3 %>%
  filter(!is.na(F30_UPB))

df3_60day<- df3 %>%
  filter(!is.na(F60_UPB))

df3_180day<- df3 %>%
  filter(!is.na(F180_UPB))

df3_forclosureDate<- df3 %>%
  filter(!is.na(FCC_DTE))

df3_30n60day<- df3 %>%
  filter(!is.na(F30_UPB) & !is.na(F60_UPB))


df3_no_LAST_RT <- df3[is.na(df3$LAST_RT),]
df3_diff_rt <- df3 %>%
  filter(LAST_RT != orig_rt)


ggplot(df3, aes(x=LAST_STAT)) + geom_bar()
ggplot(df3_diff_rt, aes(x=LAST_STAT)) + geom_bar()


df3_distinct <- df3 %>%
  distinct(LOAN_ID, .keep_all = FALSE)

df3_matured <- df3 %>%
  filter(LAST_STAT == 'P')




# See only those with 30 days delinquent


## Alternative way of applying as.Date
# df3 <- df1_fctr
# df3 <- df3 %>%
#   mutate(across(date_cols, as.Date))
# str(df3)

