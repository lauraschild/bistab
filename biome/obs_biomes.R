#regression tree with modern biomes and PFTs

rm(list = ls())
library(tidyverse)
library(randomForest)
library(popa)
library(MASS)

grasslands <- c("Montane Grasslands & Shrublands",
                "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                "Temperate Grasslands, Savannas & Shrublands")
mixed_forests <- c("Temperate Broadleaf & Mixed Forests",
                   "Mediterranean Forests, Woodlands & Scrub")
df <- read.csv("C:/Users/lschild/Documents/pollen/bistab/biome/biome_modern.csv")%>%
  filter(!(BIOME_NAME %in% c("Mangroves", "Deserts & Xeric Shrublands",
                             "Tropical & Subtropical Coniferous Forests",
                             "Tropical & Subtropical Moist Broadleaf Forests",
                             "Flooded Grasslands & Savannas")))%>%
  mutate(BIOME_NAME = ifelse(BIOME_NAME %in% grasslands,
                             "Grasslands",
                             ifelse(BIOME_NAME %in% mixed_forests,
                                    "Broadleaf & Mixed Forests",
                                    BIOME_NAME)))

# df <- read.csv("C:/Users/lschild/Documents/pollen/bistab/biome/biome_modern.csv")%>%
#   filter(!(BIOME_NAME %in% c("Mangroves", "Deserts & Xeric Shrublands",
#                              "Tropical & Subtropical Coniferous Forests",
#                              "Tropical & Subtropical Moist Broadleaf Forests",
#                              "Flooded Grasslands & Savannas")))%>%
#   mutate(BIOME_NAME = ifelse(BIOME_NAME %in% c("Boreal Forests/Taiga","Tundra"),
#                              BIOME_NAME,
#                              "other"))

# df %>%
#   filter(!grepl("Grasslands",BIOME_NAME),
#          !grepl("Forests",BIOME_NAME)) %>%
#   pivot_longer(cols = 2:6,
#                names_to = "veg",
#                values_to = "cover") %>%
#   ggplot(aes(y = cover,x = veg, fill = veg))+
#   geom_boxplot()+
#   facet_grid(.~BIOME_NAME)
# ggsave("C:/Users/lschild/Documents/pollen/bistab/plots/biome_comp/other.png",
#        width = 10,
#        height = 4)

#test random forest
set.seed(123)
sub <- sample(size = 0.7*nrow(df),
              x = 1:nrow(df), 
              replace = FALSE)
response <- as.factor(df$BIOME_NAME[sub])
expl <- sqrt(df[sub,2:6])

test_response <- df$BIOME_NAME[-sub]
test_expl <- sqrt(df[-sub,2:6])

f <- randomForest(x = expl,
                  y = response)


response <- predict(f,
                    test_expl,
                    type = "response")
table(response == test_response)

#make full tree
response <- as.factor(df$BIOME_NAME)
expl <- sqrt(df[,2:6])

ff <- randomForest(x = expl,
                   y = response)

#predict all other observations
#add age slices and average
PFT <- R_PFT %>%
  filter(Age_BP < 10000,
         Age_BP > -1000) %>%
  mutate(slice = cut(Age_BP,
                     breaks = seq(-1000,10000,500),
                     labels = seq(-500,10000,500)),
         B = BD + BE,
         N = NE+ND)%>%
  select(-Age_BP, -NE,-ND,-BD, -BE, -Continent)%>%
  group_by(Dataset_ID, slice) %>%
  summarize_all(mean)%>%
  arrange(Dataset_ID)

#replace negative values with 0
PFT[,5:10][PFT[,5:10]<0] <- 0

PFT[,5:10] <- t(apply(PFT[,5:10],
                      1,
                      function(x) 100*x/sum(x)))

expl <- sqrt(PFT[,5:10])

#predicct
assignments <- predict(ff,
                       expl,
                       type = "response")

obs_biomes <- cbind(PFT[,1:4],
                    biome = unname(assignments))
head(obs_biomes)

#visualize
world <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf")

for(cut in unique(obs_biomes$slice)){
  sub <- filter(obs_biomes, slice == cut)
  ggplot(world)+
    geom_sf()+
    coord_sf(ylim = c(20,90))+
    geom_point(data = sub,
               aes(x = Longitude,
                   y = Latitude,
                   col = biome),
               alpha = 0.6)+
    scale_color_viridis_d()+
    labs(title = paste("Observed biomes:",cut,"BP"),
         col = "Biome")+
    theme_bw()
  
  ggsave(paste0("C:/Users/lschild/Documents/pollen/bistab/plots/biome_comp/obs_biome_",
                cut,".png"),
         width = 8,
         height = 4)
}

ggplot(world)+
  geom_sf()+
  coord_sf(ylim = c(20,90))+
  geom_point(data = obs_biomes,
             aes(x = Longitude,
                 y = Latitude,
                 col = biome),
             alpha = 0.6)+
  scale_color_viridis_d()+
  facet_grid(slice~.)+
  labs(title = paste("Observed biomes"),
       col = "Biome")+
  theme_bw()

ggsave("C:/Users/lschild/Documents/pollen/bistab/plots/biome_comp/ts_obs2.png",
       width = 8,
       height = 40,
       limitsize = FALSE)
