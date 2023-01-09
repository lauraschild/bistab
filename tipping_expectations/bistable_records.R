#select bistable records
rm(list = ls())

library(tidyverse)
library(multimode)

PFTs <- read.csv("C:/Users/lschild/Documents/pollen/bistab/new_data/PFT_opti.csv")
PFTs <- PFTs %>%
  filter(Age_BP <= 8000)

is_bistab <- function(ID){
  trees <- PFTs$tree[PFTs$Dataset_ID == ID]
  
  if(diptest::dip.test(trees)$p.value < 0.05){
    modes <- multimode::locmodes(trees, 2)$locations
    modes <- modes[-2]
    if(diff(modes)>= 10){
      v <- c("bistab",
             as.character(modes[1]),
             as.character(modes[2]))
    }else{
      v <- c("bimod",
             as.character(modes[1]),
             as.character(modes[2]))
    }
  }else{
    v <- c("not_bimodal","0","0")
  }
  
  return(v)
}

res <- lapply(unique(PFTs$Dataset_ID),
              is_bistab)

res <- do.call(rbind,res) %>%
  cbind(unique(PFTs$Dataset_ID)) %>%
  as.data.frame()

names(res) <- c("Stability",
                "mode1",
                "mode2",
                "Dataset_ID")

coords <- PFTs %>%
  group_by(Dataset_ID) %>%
  summarize(Lat = mean(Latitude),
            Lon = mean(Longitude))
res_plot <- merge(res,
                  coords,
                  by = "Dataset_ID")

world <- rnaturalearth::ne_countries(scale= "medium",
                                     returnclass = "sf")

ggplot(data = world)+
  geom_sf()+
  coord_sf(ylim = c(20,90))+
  geom_point(data = res_plot,
             aes(x = Lon,
                 y =Lat,
                 col = Stability,
                 alpha = Stability))+
  scale_color_manual(values = c( rgb(4, 139, 168,
                                     maxColorValue = 255),
                                 rgb(241, 143, 1,
                                    maxColorValue = 255),
                                "grey"))+
  scale_alpha_manual(values = c(1,1,0.1))+
  theme_bw()+
  labs(title = "Location of bimodal and suspected bistable records")
  
PFTs %>%
  filter(Dataset_ID %in% res_plot$Dataset_ID[res_plot$Stability == "bistab"])%>%
  ggplot(aes(x=Age_BP,
             y = tree,
             group = Dataset_ID))+
  geom_line()+
  theme(legend.position = "none")+
  facet_wrap(.~Dataset_ID)
