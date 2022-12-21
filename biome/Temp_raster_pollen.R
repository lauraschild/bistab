#isoline temperature maps holocene
#(+precipitation?)
rm(list = ls())

library(raster)
library(sp)
library(popa)
library(tidyverse)

#load reconstructed climate
Temp <- Temp %>%
  filter(Age_AD_BP <= 10000,
         Age_AD_BP > -1000)%>%
  mutate(slice = cut(Age_AD_BP,
                     breaks = seq(-1000,10000,1000),
                     labels = seq(0,10000,1000)))%>%
  group_by(Dataset_ID, slice, Longitude, Latitude) %>%
  summarize(MAAT_WAPLS = mean(MAAT_WAPLS, na.rm = TRUE))

#make spatial points df? or make sf object
Temp_Points <- sf::st_as_sf(coords = c("Longitude","Latitude"),
                                      x = Temp,
                                      crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
SpatialPointsDataFrame(coords = Temp[,c("Longitude","Latitude")],
                       data = Temp[,c("slice","MAAT_WAPLS")])

#rasterize
#try different raster sizes
empty <- raster(ncol = 36,
                nrow = 18)
make_raster <- function(slices){
  sub <- Temp_Points %>%
    filter(slice == slices)
  
  sub_raster <- rasterize(sub,
                          empty,
                          field = "MAAT_WAPLS",
                          fun = mean)
  sub_raster <- crop(sub_raster,
                     extent(c(-180,180,20,90)))
  return(sub_raster)
}
rasters <- lapply(as.numeric(as.character(unique(Temp_Points$slice))),
                  make_raster)

r <- do.call(brick, rasters)
names(r) <- as.character(unique(Temp_Points$slice))

world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")

for(index in 1:length(names(r))){
  raster <- r[[index]]
  name <- paste(sub("X","",names(raster)),"BP")
  
  png(paste0("C:/Users/lschild/Documents/pollen/bistab/plots/temperature/",
             name,
             ".png"),
      width = 1200,
      height = 600)
  plot(world$geometry,
       ylim = c(30,90),
       main = name,
       sub = "pollen-based MAAT")
  brks <- seq(-20,20,5)
  plot(raster,
       add =TRUE,
       alpha = 0.5,
       legend = FALSE,
       main = name,
       breaks = brks,
       col=terrain.colors(9),
       interpolate = TRUE)
  contour(raster,
          add = TRUE,
          col = par("fg"),
          nlevels = 10,
          lwd = 2,
          labcex = 1,
          interpolate = TRUE)
  dev.off()
}
  
