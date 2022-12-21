#temp raster modern + anomalie

rm(list=ls())
library(raster)
library(tidyverse)

mod <- brick("C:/Users/lschild/Documents/pollen/data/cru/cru_ts4.06.1941.1950.tmp.dat.nc")
mod <- mean(mod[[tail(1:length(names(mod)),12)]])%>%
  crop(extent(c(-180,180, 20,90)))

plot(mod)

recon <- read.csv("C:/Users/lschild/Documents/pollen/pollen/ordination/clim/recon.csv")

#mean anomalies with slices 
recon <- recon %>%
  filter(yr <= 10000)%>%
  mutate(slice = cut(yr,
                     breaks = seq(-1000,10000,1000),
                     labels = seq(0,10000,1000)))%>%
  group_by(slice) %>%
  summarize(tr = mean(tr))

world <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")


for(slice in unique(recon$slice)){
  raster <- mod + recon$tr[recon$slice == slice]
  name <- paste(slice, "BP")
  
  png(paste0("C:/Users/lschild/Documents/pollen/bistab/plots/temperature/CRU_",
             name,
             ".png"),
      width = 1200,
      height = 600)
  
  plot(world$geometry,
       ylim = c(30,90),
       main = name,
       sub = "modern climate + anomaly")
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
