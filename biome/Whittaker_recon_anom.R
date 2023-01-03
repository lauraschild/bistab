#biomes

load("C:/Users/lschild/Documents/pollen/bistab/new_data/Whittaker_biomes_poly.rda")
library(sf)
library(tidyverse)

polys <- st_as_sf(Whittaker_biomes_poly)

Temp <- read.csv("C:/Users/lschild/Documents/pollen/bistab/biome/pollen_anom_MAT.csv")
prec <- read.csv("C:/Users/lschild/Documents/pollen/bistab/biome/pollen_anom_prec.csv")

clim <- merge(Temp,
              prec,
              by = c("Dataset_ID","slice", "Lat","Lon"))%>%
  mutate(prec = prec/10)

clim <- st_as_sf(clim,
                 coords = c("MAT",
                            "prec"))

biomes <- st_intersection(polys,clim)

biomes <- as.data.frame(biomes)%>%
  mutate(biome = ifelse(biome %in% c("Boreal forest", "Tundra"),
                        biome,
                        "other"))

world <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf")

for(cut in unique(biomes$slice)){
  sub <- filter(biomes, slice == cut)
  
  ggplot(world)+
    geom_sf()+
    coord_sf(ylim = c(20,90))+
    geom_point(data = sub,
               aes(x = Lon,
                   y = Lat,
                   col = biome),
               alpha = 0.6)+
    scale_color_viridis_d()+
    labs(title = paste0("Expected biome (Whittaker): ",cut," BP"),
         col = "Biome")+
    theme_bw()
  
  ggsave(paste0("C:/Users/lschild/Documents/pollen/bistab/plots/biomes/whittaker2/",
                cut,
                ".png"),
         width = 8,
         height = 4)
}
