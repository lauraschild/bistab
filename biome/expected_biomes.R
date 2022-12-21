#expected biomes
#based on MAT and TJul boundaries

rm(list = ls())
library(tidyverse)

#boundaries based on Gerten et al.
bound <- data.frame(biome = c("boreal","tundra"),
                    TJul = c(16,9),
                    MAT = c(-4,-11))

clim_vars <- c("Jul","MAT")
for(var in clim_vars){
  #load anomaly climate
  path <- "C:/Users/lschild/Documents/pollen/bistab/biome/pollen_anom_"
  clim <- read.csv(paste0(path,
                          var,
                          ".csv"))
  col <- ifelse(var == "Jul",
                2,
                3)
  bounds <- bound[,col]
  
  clim <- clim %>%
    mutate(biome = ifelse(get(var) > bounds[1],
                          "other",
                          ifelse(get(var) > bounds[2],
                                 "boreal",
                                 "tundra")))
  
  world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  
  for(slices in unique(clim$slice)){
    sub <- filter(clim, slice == slices)
    
    ggplot(world)+
      geom_sf()+
      coord_sf(ylim = c(20,90))+
      geom_point(data = sub,
                 aes(x = Lon,
                     y = Lat,
                     col = biome),
                 alpha = 0.6)+
      theme_bw()+
      scale_color_viridis_d()+
      labs(title = paste0("Expected Biomes: ",slices,"BP"),
           col = "Biome",
           subtitle = paste0("based on ", var))
    ggsave(paste0("C:/Users/lschild/Documents/pollen/bistab/plots/expected_biomes/",
                  var,"_",
                  slices,".png"),
           width = 8,
           height = 4)
  }
  
  assign(var,clim)
}

expected <- cbind(Jul, MAT = MAT$biome) %>%
  select(-Jul) %>%
  rename(Jul = biome)
head(expected)

write.csv(expected,
          "C:/Users/lschild/Documents/pollen/bistab/biome/expected_biomes.csv",
          row.names = FALSE)
