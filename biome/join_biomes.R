#join biome to pollen PFT info

rm(list = ls())
library(sf)
library(tidyverse)
library(popa)

#filter PFT for modern samples + summarize PFTs
PFT <- R_PFT %>%
  mutate(B = BD+BE,
         N = NE+ND)%>%
  filter(Age_BP <= 500)%>%
  group_by(Dataset_ID) %>%
  summarize_all(mean)%>%
  select(Dataset_ID, Longitude, Latitude,
         shrub,grass,herb,B,N)

#rescale to sum to 100%
PFT[,4:8] <- t(apply(PFT[,4:8],
                     1,
                     function(x) 100*x/sum(x)))

#create sf
Points <- st_as_sf(PFT,
                   coords = c("Longitude",
                              "Latitude"),
                   crs = 4326)

#load biome sf
biomes <- read_sf("C:/Users/lschild/Documents/pollen/bistab/biome/tipping/Ecoregions2017.shp")
plot(biomes)
