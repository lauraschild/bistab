#get modern climatic envelopes for different tree cover conditions
rm(list = ls())

library(tidyverse)
library(raster)
library(sf)

#load optimized PFTs
PFTs <- read.csv("C:/Users/lschild/Documents/pollen/bistab/new_data/PFT_opti.csv")

#load modern climate and combine in brick
clims <- c("modern_Jul.tif",
           "modern_MAT.tif",
           "modern_Jan.tif",
           "modern_prec.tif")
clim <- lapply(clims,
               function(x) raster(paste0("C:/Users/lschild/Documents/pollen/data/cru/",
                                         x)))
clim <- do.call(brick, clim)
names(clim) <- gsub(pattern = ".tif",
                    replacement = "",
                    gsub(pattern = "modern_",
                         replacement = "",
                         clims))

#aggregate PFTs to modern and create sf
mod_PFT <-  PFTs %>%
  filter(Age_BP < 500) %>%
  group_by(Dataset_ID) %>%
  summarize_all(mean) %>%
  dplyr::select(-Continent) %>%
  st_as_sf(coords = c("Longitude", "Latitude"))

conts <- PFTs %>%
  group_by(Dataset_ID) %>%
  summarize(Continent = unique(Continent))


envelopes <- cbind(mod_PFT, raster::extract(clim,
                                    mod_PFT))%>%
  merge(conts,
        by = "Dataset_ID")

plot(envelopes$MAT, envelopes$tree)
plot(envelopes$Jul * envelopes$prec/100, envelopes$tree)
plot(envelopes$prec, envelopes$tree)
plot(envelopes$Jan, envelopes$tree)

ggplot(envelopes, aes(x = MAT,
                      y = prec,
                      col = tree
                      ,
                      size = (BD+BE)
                      )
       )+
  # facet_wrap(.~Continent,
  #            nrow = 3)+
  geom_point(alpha = 0.5)+
  scale_color_binned(limits = c(0,100),
                     type = "viridis")+
  labs(title = "Climatic envelope for different tree covers/types",
       subtitle = "modern observed climate, modern pollen-based tree cover (<500BP)",
       color = "tree cover",
       size = "broadleaf percentage")+
  theme_bw()+
  theme(legend.position = "bottom")

library(vegan)
comp <- sqrt(st_drop_geometry(envelopes[,3:6]))
clim <- st_drop_geometry(envelopes[,11:14])
clim <- apply(clim,
              2,
              scale)
#remove rows with clim NAs
comp <- comp[!(is.na(clim)[,1]),]
clim <- clim[!(is.na(clim[,1])),]
plot(rda(X = data.frame(comp),
    Y = data.frame(clim[,c(1,4)])))
