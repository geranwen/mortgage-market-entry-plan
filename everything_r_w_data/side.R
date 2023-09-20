#####
## Oct 2nd, check Vedang's .csv file with 'NaN's removed. 

side_df <- read.csv("dataset_Fall 2021 cleaned Vedang.csv", header = TRUE, stringsAsFactors = TRUE)

side_df[side_df$LOAN_ID == 969195685029,]

side_df[is.na(side_df$LAST_UPB),]
side_df[is.na(df4$LAST_UPB),] %>%
  summarize(P_share = sum(LAST_STAT == 'P')/n())

side_df[is.na(df4$LAST_UPB),] %>%
  summarize(P_share = sum(LAST_STAT == 'P')/n())


df3 %>%
  group_by(LAST_STAT) %>%
  summarize(n())

side_df_R <- df4 %>%
  filter(LAST_STAT == 'R')

side_df[is.na(df4$CSCORE_B),]


side_df[is.na(df4$LAST_UPB),] %>%
  filter(LAST_STAT != 'P')

df5 %>%
  group_by(msa) %>%
  summarize(n())

count(df3[df3$LAST_STAT == 'P', ])/count(df3[df3$LAST_STAT != 'C', ])

df6 %>%
  summarize(mean = mean(CSCORE_AVG))

df3 %>% 
  filter(!is.na(CSCORE_C)) %>%
  summarize(mean = mean(CSCORE_C))