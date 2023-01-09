#envelopes past model

rm(list= ls())
library(tidyverse)
library(raster)

#get PFTs
PFTs <- read.csv("C:/Users/lschild/Documents/pollen/bistab/new_data/PFT_opti.csv")
PFTs <- PFTs %>%
  mutate(slice = cut(Age_BP,
                     breaks = seq(-1000,10000,500),
                     labels = seq(-500,10000,500)))%>%
  group_by(Dataset_ID, slice) %>%
  summarize_all(mean, na.rm = TRUE)

#get climate
prec <- brick("//dmawi.de/potsdam/data/bioing/data/CHELSA_paleoclim/lower_res/prec_CHELSA_ageslices.tif")
MAT <- brick("//dmawi.de/potsdam/data/bioing/data/CHELSA_paleoclim/lower_res/MAT_CHELSA_ageslices.tif")
JJA <- brick("//dmawi.de/potsdam/data/bioing/data/CHELSA_paleoclim/lower_res/JJA_CHELSA_ageslices.tif")

#extract climate for record locations
coords <- PFTs %>%
  group_by(Dataset_ID) %>%
  summarize(Lon = mean(Longitude),
            Lat = mean(Latitude))

prec_p <- cbind(Dataset_ID = coords$Dataset_ID,
                raster::extract(prec,
                                coords[,2:3])) %>%
  as.data.frame() %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "slice",
               values_to = "prec")%>%
  mutate(slice = as.numeric(gsub("BP",
                                 "",
                                 gsub("X",
                                      "",
                                      slice))))

MAT_p <- cbind(Dataset_ID = coords$Dataset_ID,
               raster::extract(MAT,
                               coords[,2:3])) %>%
  as.data.frame() %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "slice",
               values_to = "MAT")%>%
  mutate(slice = as.numeric(gsub("BP",
                                 "",
                                 gsub("X",
                                      "",
                                      slice))))

JJA_p <- cbind(Dataset_ID = coords$Dataset_ID,
               raster::extract(JJA,
                               coords[,2:3])) %>%
  as.data.frame() %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "slice",
               values_to = "JJA")%>%
  mutate(slice = as.numeric(gsub("BP",
                                 "",
                                 gsub("X",
                                      "",
                                      slice))))
clim <- merge(merge(prec_p,
              MAT_p,
              by = c("Dataset_ID","slice")),
              JJA_p)
total <- merge(PFTs,
               clim,
               by = c("Dataset_ID", "slice"))

ggplot(total[sample(nrow(total),1500),], aes(x = JJA, 
                  y = prec, 
                  col = tree,
                  size = (BD + BE)))+
  geom_point(alpha = 0.5)+
  scale_color_binned(limits = c(0,100),
                     type = "viridis")+
  labs(title = "Climatic envelop for different tree covers/ type",
       subtitle = "modeled paleo climate (CHELSA/Trace21k) and pollen-based vegetation",
       color = "tree cover",
       size = "broadleaf percentage")+
  ylim(c(0,3000))+
  theme_bw()+
  theme(legend.position = "bottom")

#stability landscape?

get_min_max <- function(end){
  start <- end -2
  
  trees <- total %>%
    filter(JJA > start & JJA <= end)%>%
    select(tree)%>%
    unname()%>%
    unlist()
  
  bw <- 1.06 * sd(trees)/length(trees)^(1/5)
  
  d <- density(trees,
               bw = bw,
               kernel = "gaussian")
  
  #determine local maxima and minima
  ts_y <- ts(d$y)
  tp <- pastecs::turnpoints(ts_y)
  
  #plot for checking
  plot(d)
  points(d$x[tp$tppos], d$y[tp$tppos], col = "red")
  
  #collect max and mi with thresholds
  max <- photobiology::get_peaks(d$x,
                   d$y,
                   ignore_threshold = 0.1)
  min <- photobiology::get_valleys(d$x,
                     d$y,
                     ignore_threshold = 0.1)
  
  min <- cbind(rep(end -1, nrow(min)),
               min$x,
               rep("min", nrow(min)))
  max <- cbind(rep(end -1, nrow(max)),
               max$x,
               rep("max",nrow(max)))
  points <- rbind(min,max)
  
  colnames(points) <- c("T",
                        "tree",
                        "type")
  
  return(points)
}
points <-lapply(0:34,
                get_min_max)
points <- do.call(rbind,
                  points) %>%
  as.data.frame() %>%
  mutate(T = as.numeric(T),
         tree = as.numeric(tree))

ggplot(total,
       aes(JJA,
           tree))+
  geom_point(alpha = 0.2)+
  geom_point(data = points,
             aes(T,
                 tree,
                 col = factor(type,
                              label = c("stable",
                                        "unstable"))))+
  labs(title = "Stability landscapes: modeled paleo climate",
       col = "states")

