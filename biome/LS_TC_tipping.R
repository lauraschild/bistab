#TC tipping
rm(list = ls())

library(tidyverse)
library(raster)
library(sf)
library(popa)

PFTs <- LS %>%
  merge(P[,c("Dataset_ID","Longitude","Latitude")],
        all.x = TRUE)%>%
  group_by(Dataset_ID) %>%
  summarize_all(unique)
tip <- raster("C:/Users/lschild/Documents/pollen/bistab/biome/tipping/biome_chenge_class.tif")


gimme_clim <- function(clim_var){
  if(clim_var == "July"){
    file <- "climate_scenarios_july.tif"
    label <- "July T in °C"
  }
  if(clim_var == "MAT"){
    file <- "climate_scenarios.tif"
    label <- "MAT in °C"
  }
  if(clim_var == "prec"){
    file <- "climate_scenarios_prec.tif"
    label <- "Annual precipitation in mm"
  }
  if(clim_var == "jan"){
    file <- "climate_scenarios_jan.tif"
    label <- "January T in °C"
  }
  
 clim <- brick(paste0("C:/Users/lschild/Documents/pollen/bistab/biome/tipping/",file))
  
  #which locations tip?
  tipping <- cbind(Dataset_ID = PFTs$Dataset_ID,
                   tipping = extract(tip,
                                     PFTs%>%
                                       dplyr::select(Longitude, Latitude),
                                     method = "simple"))
  tipping <- as.data.frame(tipping)
  
  #red = 5, yellow = 6, green = 7
  get_clim <- function(color){
    
    points <- PFTs%>%
      filter(Dataset_ID %in% tipping$Dataset_ID[which(tipping$tipping == color)])
    
    points <- st_as_sf(points,
                       coords = c("Longitude","Latitude"))
    
    tip_clim <- cbind(points,
                      clim = extract(clim[[color - 4]],
                                     points))
    names(tip_clim)[ncol(tip_clim)-1] <- clim_var
    
    return(tip_clim)
    
  }
  
  tip_clim <- lapply(c(5,6,7),
                     get_clim) %>%
    bind_rows()
  
  if(clim_var != "MAT"){
    tip_clim <- data.frame(tip_clim[[clim_var]])
    names(tip_clim) <- clim_var
  }
  
  return(st_drop_geometry(tip_clim))
}

tip_clim <- lapply(c("MAT","July","prec","jan"),
                   gimme_clim) %>%
  bind_cols() %>%
  merge(PFTs[,-2],
        by = "Dataset_ID")

plot(tip_clim$MAT, tip_clim$Landsat_TC,
     xlab = "MAT",
     ylab = "Landsat_TC")

plot(tip_clim$July, tip_clim$Landsat_TC,
     xlab = "avergae July T",
     ylab = "Landsat_TC")

plot(tip_clim$prec, tip_clim$Landsat_TC,
     xlab = "annual precipitation",
     ylab = "Landsat_TC")

plot(tip_clim$jan, tip_clim$Landsat_TC,
     xlab = "mean January T",
     ylab = "Landsat_TC")


p <- ggplot(tip_clim, aes(y= prec, 
                          x = July, 
                          col = Landsat_TC,
                          text = paste("Lat:",Latitude),
                          label = Longitude))+
  geom_point()+
  ylim(c(0,2500))+
  scale_color_viridis_b()+
  labs(title = "Tipping conditions for different tree covers",
       x = "mean July T",
       y = "annual precipitation",
       color = "tree cover (%)",
       subtitle = "modern remote sensing tree cover (Landsat) for pollen source areas")+
  theme_bw()+
  theme(legend.position = c(0.1,0.75),
        legend.background = element_rect(fill=alpha('white', 0.4)),
        legend.title= element_text(size=10))


# ggsave("C:/Users/lschild/Documents/pollen/bistab/plots/Landsat_TC_tipping_MAT.png",
#        width = 8,
#        height = 5)
library(plotly)
p <- ggplotly(p)
htmlwidgets::saveWidget(as_widget(p), "C:/Users/lschild/Documents/pollen/bistab/plots/July_LS.html")
