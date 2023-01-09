#envelopes of the past
rm(list = ls())

library(tidyverse)
library(popa)

vars <- c("Jul", "MAT", "prec")

clim <- lapply(vars,
               function(x) read.csv(paste0("C:/Users/lschild/Documents/pollen/bistab/biome/pollen_anom_",
                                           x,".csv"))) 
clim <- cbind(clim[[1]],
              MAT = clim[[2]]$MAT,
              prec = clim[[3]]$prec)

#aggregate PFTs into slices as well
PFTs <- read.csv("C:/Users/lschild/Documents/pollen/bistab/new_data/PFT_opti.csv")
PFTs <- PFTs %>%
  mutate(slice = cut(Age_BP,
                     breaks = seq(-1000,10000,500),
                     labels = seq(-500,10000,500)))%>%
  group_by(Dataset_ID, slice) %>%
  summarize_all(mean, na.rm = TRUE)%>%
  filter(tree < 100)

#rescale compositions to sum to 100 + apply openness correction after
# 
# PFTs <- cbind(PFTs[,1:2],
#               t(apply(PFTs[,7:10],
#                       1,
#                       function(c) 100*c/sum(c))))
head(PFTs)
head(clim)
total <- merge(clim,
               PFTs,
               by = c("Dataset_ID", "slice"))%>%
  filter(prec >0)
ggplot(total[sample(nrow(total),1000),], aes(x = Jul, 
                  y = prec,
                  col = tree,
                  size = (BD+BE)))+
  geom_point(alpha = 0.5)+
  scale_color_binned(limits = c(0,100),
                     type = "viridis")+  
  labs(title = "Climatic envelope for different tree covers/ type",
       subtitle = "reconstructed paleo climate (modern + anomaly) and vegetation",
       color = "tree cover",
       size = "broadleaf percentage")+
  theme_bw()+
  theme(legend.position = "bottom")


get_min_max <- function(end){
  start <- end -2
  
  trees <- total %>%
    filter(Jul > start & Jul <= end)%>%
    select(tree)%>%
    unname()%>%
    unlist()
  
  if(length(trees) > 1){
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
 
}
points <-lapply(-4:32,
                get_min_max)
points <- do.call(rbind,
                  points) %>%
  as.data.frame() %>%
  mutate(T = as.numeric(T),
         tree = as.numeric(tree))

ggplot(total,
       aes(Jul,
           tree))+
  geom_point(alpha = 0.2)+
  geom_point(data = points,
             aes(T,
                 tree,
                 col = factor(type,
                              labels = c("stable",
                                         "unstable"))))+
  labs(title = "Stability landscapes: measured clim + pollen anomalies",
       col = "states")
