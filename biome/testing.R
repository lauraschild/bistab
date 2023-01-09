#illustrate tree covers
rm(list = ls())

library(popa)
library(tidyverse)
library(sf)
library(rnaturalearth)

# landsat <- read.csv("//dmawi.de/potsdam/data/bioing/user/lschild/data/LANDSAT_1002.csv")%>%
#   dplyr::select(Dtst_ID,
#          mean) %>%
#   rename(Dataset_ID = Dtst_ID)
# 
# test <- merge(landsat,
#       LS,
#       by = "Dataset_ID")
# plot(test$mean, test$Landsat_TC)
PFTs <- read.csv("C:/Users/lschild/Documents/pollen/bistab/new_data/PFT_opti.csv") %>%
  filter(Age_BP < 500) %>%
  group_by(Dataset_ID) %>%
  summarize_all(mean)%>%
  dplyr::select(Dataset_ID, Longitude, Latitude,tree) %>%
  merge(LS,
        by = "Dataset_ID")
plot(tree ~ Landsat_TC,
     PFTs)
abline(a =0, b = 1,
       col = "red")
mean(abs(PFTs$tree - PFTs$Landsat_TC), na.rm = TRUE)
