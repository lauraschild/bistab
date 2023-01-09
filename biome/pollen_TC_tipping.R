#TC tipping
rm(list = ls())

library(tidyverse)
library(raster)
library(sf)
PFTs <- read.csv("C:/Users/lschild/Documents/pollen/bistab/new_data/PFT_opti.csv")
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
  tipping <- cbind(Dataset_ID = sort(unique(PFTs$Dataset_ID)),
                   tipping = extract(tip,
                                     PFTs%>%
                                       group_by(Dataset_ID)%>%
                                       summarize(Longitude = unique(Longitude),
                                                 Latitude = unique(Latitude))%>%
                                       dplyr::select(-Dataset_ID),
                                     method = "simple"))
  tipping <- as.data.frame(tipping)
  
  #red = 5, yellow = 6, green = 7
  get_clim <- function(color){
    
    points <- PFTs%>%
      filter(Dataset_ID %in% tipping$Dataset_ID[which(tipping$tipping == color)],
             Age_BP < 500)%>%
      group_by(Dataset_ID) %>%
      summarize(Longitude = unique(Longitude),
                Latitude = unique(Latitude), 
                shrub = mean(shrub),
                grass = mean(grass),
                tree = mean(tree),
                herb = mean(herb),
                BE = mean(BE),
                BD = mean(BD),
                NE = mean(NE),
                ND = mean(ND),
                .groups = "drop")
    
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
  bind_cols()

coords <- PFTs %>%
  group_by(Dataset_ID)%>%
  summarize(Lat = mean(Latitude),
            Lon = mean(Longitude),
            .groups = "drop")

tip_clim <- merge(tip_clim,
                  coords,
                  by = "Dataset_ID")
plot(tip_clim$MAT, tip_clim$tree,
     xlab = "MAT",
     ylab = "pollen-based tree cover")

plot(tip_clim$July, tip_clim$tree,
     xlab = "avergae July T",
     ylab = "pollen-based tree cover")

plot(tip_clim$prec, tip_clim$tree,
     xlab = "annual precipitation",
     ylab = "pollen-based tree cover")

plot(tip_clim$jan, tip_clim$tree,
     xlab = "mean January T",
     ylab = "pollen-based tree cover")

p <- ggplot(tip_clim, aes(y= prec, 
                     x = MAT, 
                     col = tree, 
                     size = BD+BE,
                     text = paste("Lat:",Lat),
                     label = Lon))+
  geom_point(alpha = 0.6)+
  ylim(c(0,2500))+
  scale_color_viridis_b()+
  labs(title = "Tipping conditions for different tree covers",
       x = "MAT",
       y = "annual precipitation",
       color = "tree cover (%)",
       size = "broadleaved",
       subtitle = "modern (<500BP) pollen-based tree cover with optimized reconstructions")+
  theme_bw()+
  theme(legend.position = "bottom",
        #legend.position = c(0.1,0.75),
        legend.background = element_rect(fill=alpha('white', 0.4)),
        legend.title= element_text(size=10))

library(plotly)
p <- ggplotly(p)
htmlwidgets::saveWidget(as_widget(p), "C:/Users/lschild/Documents/pollen/bistab/plots/July.html")
ggsave("C:/Users/lschild/Documents/pollen/bistab/plots/pollen_TC_tipping_MAT.png",
       width = 8,
       height = 5)

