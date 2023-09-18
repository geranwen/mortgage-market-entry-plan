# Side Script #2

#####
## correct for skewness3

library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(forcats)
source('fn_mod_eval.R')

# load('df6.1.Rdata')
load('df6.1.Rdata')


#####
## Histograms to find skewness

df6.1_numeric <- df7.5 %>%
  select(where(is.numeric)) %>%
  select(- LOAN_ID, )

mybins <- 50

## orig_amt
p_trans_orig_amt1 <- ggplot(df6.1_numeric, aes(orig_amt)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_orig_amt2 <- ggplot(df6.1_numeric, aes(log(orig_amt))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_orig_amt3 <- ggplot(df6.1_numeric, aes(orig_amt^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')

grid.arrange(p_trans_orig_amt1, p_trans_orig_amt2, p_trans_orig_amt3, 
             nrow = 2, top="Compare with and without log trans")

## ORIG_VAL
p_trans_orig_val1 <- ggplot(df6.1_numeric, aes(ORIG_VAL)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_orig_val2 <- ggplot(df6.1_numeric, aes(log(ORIG_VAL))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_orig_val3 <- ggplot(df6.1_numeric, aes(ORIG_VAL^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')

grid.arrange(p_trans_orig_val1, p_trans_orig_val2, p_trans_orig_val3, 
             nrow = 2, top="Compare with and without log trans")

## orig_rt
p_trans_orig_rt1 <- ggplot(df6.1_numeric, aes(orig_rt)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_orig_rt2 <- ggplot(df6.1_numeric, aes(log(orig_rt))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_orig_rt3 <- ggplot(df6.1_numeric, aes(orig_rt^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')

grid.arrange(p_trans_orig_rt1, p_trans_orig_rt2, p_trans_orig_rt3, 
             nrow = 2, top="Compare with and without log trans")


## rt_dev
const_rt_dev <- 1 - min(df6.1_numeric$rt_dev, na.rm = TRUE)
p_trans_rt_dev1 <- ggplot(df6.1_numeric, aes(rt_dev)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_rt_dev2 <- ggplot(df6.1_numeric, aes(log(rt_dev + const_rt_dev))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_rt_dev3 <- ggplot(df6.1_numeric, aes(rt_dev^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')
grid.arrange(p_trans_rt_dev1, p_trans_rt_dev2, p_trans_rt_dev3, 
             nrow = 2, top="Compare with and without log trans")


## oltv
p_trans_oltv1 <- ggplot(df6.1_numeric, aes(oltv)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_oltv2 <- ggplot(df6.1_numeric, aes(log(oltv))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_oltv3 <- ggplot(df6.1_numeric, aes(oltv^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')
grid.arrange(p_trans_oltv1, p_trans_oltv2, p_trans_oltv3, 
             nrow = 2, top="Compare with and without log trans")

## ocltv
p_trans_ocltv1 <- ggplot(df6.1_numeric, aes(ocltv)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_ocltv2 <- ggplot(df6.1_numeric, aes(log(ocltv))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_ocltv3 <- ggplot(df6.1_numeric, aes(ocltv^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')
grid.arrange(p_trans_ocltv1, p_trans_ocltv2, p_trans_ocltv3, 
             nrow = 2, top="Compare with and without log trans")

## ocltv
p_trans_ocltv1 <- ggplot(df6.1_numeric, aes(ocltv)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_ocltv2 <- ggplot(df6.1_numeric, aes(log(ocltv))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_ocltv3 <- ggplot(df6.1_numeric, aes(ocltv^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')
grid.arrange(p_trans_ocltv1, p_trans_ocltv2, p_trans_ocltv3, 
             nrow = 2, top="Compare with and without log trans")

# dti
p_trans_dti1 <- ggplot(df6.1_numeric, aes(dti)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_dti2 <- ggplot(df6.1_numeric, aes(log(dti+1))) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('log transform')
p_trans_dti3 <- ggplot(df6.1_numeric, aes(dti^(1/3))) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cube root transform')
grid.arrange(p_trans_dti1, p_trans_dti2, p_trans_dti3, 
             nrow = 2, top="Compare with and without log trans")

# CScore
p_trans_cscore1 <- ggplot(df6.1_numeric, aes(CSCORE_AVG)) + 
  geom_histogram(bins = mybins, fill = '#69b3a2' ,alpha= 0.7) + 
  ggtitle('without log transform')
p_trans_cscore2 <- ggplot(df6.1_numeric, aes((CSCORE_AVG)^6)) + 
  geom_histogram(bins = mybins, fill = '#404080' ,alpha= 0.7) + 
  ggtitle('sqared transform')
p_trans_cscore3 <- ggplot(df6.1_numeric, aes(CSCORE_AVG^8)) + 
  geom_histogram(bins = mybins, fill = '#F7A4A4' ,alpha= 0.7) + 
  ggtitle('cubed transform')
grid.arrange(p_trans_cscore1, p_trans_cscore2, p_trans_cscore3, 
             nrow = 2, top="Compare with and without trans")




#####
## Random codes from Q&A sessions

load('df3.Rdata')

# Check 'LAST_UPB' and 'LAST_STAT == P'
df3 %>%
  filter(LAST_STAT == 'P', !is.na(LAST_UPB)) %>% 
  summarize(length(!is.na(LAST_UPB)), mean(LAST_UPB), quantile(LAST_UPB, c(0.25, 0.5, 0.75)))

df3 %>%
  filter(LAST_STAT == 'P') %>%
  summarize(sum(is.na(LAST_UPB)))
