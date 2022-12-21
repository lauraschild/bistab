#local pollen anomalies

rm(list = ls())
library(popa)
library(tidyverse)

reference <- Temp %>%
  filter(Age_AD_BP <= 1000,
         Age_AD_BP >= 0) %>%
  group_by(Dataset_ID) %>%
  summarize_all(mean)%>%
  select(-Longitude, -Latitude, - Age_AD_BP)

#delete any rows that have NAs
Nas <- apply(reference[,-1],
             1,
             function(x) sum(is.na(x)))
reference <- reference[Nas == 0,]

clim <- Temp %>%
  filter(Dataset_ID %in% reference$Dataset_ID,
         Age_AD_BP < 10000,
         Age_AD_BP > -1000) %>%
  mutate(slice = cut(Age_AD_BP,
                     breaks = seq(-1000,10000,500),
                     labels = seq(-500,10000,500)))%>%
  select(-Age_AD_BP)%>%
  group_by(Dataset_ID, slice) %>%
  summarize_all(mean)%>%
  arrange(Dataset_ID)

reference <- clim %>%
  select(Dataset_ID) %>%
  merge(reference)%>%
  arrange(Dataset_ID)

#subtract reference from observed T
anom <- cbind(clim[,1:4],
              clim[,-(1:4)] - reference[,-1])


write.csv(anom,
          "C:/Users/lschild/Documents/pollen/bistab/biome/local_anom.csv",
          row.names = FALSE)
