#biomes

load("C:/Users/lschild/Documents/pollen/bistab/new_data/Whittaker_biomes_poly.rda")
library(sf)
library(tidyverse)

polys <- st_as_sf(Whittaker_biomes_poly)

Temp <- read.csv("C:/Users/lschild/Documents/pollen/pollen/ordination/clim_recons/datasets/local_recons.csv")
prec <- read.csv("C:/Users/lschild/Documents/pollen/pollen/ordination/clim_recons/datasets/local_recons_prec.csv")

clim <- merge(Temp,
              prec,
              by = c("Dataset_ID","Age_AD_BP", "Longitude","Latitude"))%>%
  select(Dataset_ID, Longitude, Latitude, Age_AD_BP, ends_with("WAPLS")) %>%
  mutate(Prec_WAPLS = Prec_WAPLS/10) %>%
  filter(!(is.na(Prec_WAPLS)))%>%
  rename(Age_BP = Age_AD_BP)

clim <- st_as_sf(clim,
                 coords = c("MAAT_WAPLS",
                            "Prec_WAPLS"))

biomes <- st_intersection(polys,clim)

biomes <- as.data.frame(biomes)


#how many biomes have records had?
number_of_biomes <- biomes %>%
  filter(Age_BP <= 8000)%>%
  group_by(Dataset_ID, biome, Latitude, Longitude)%>%
  summarize(n = n()) %>%
  group_by(Dataset_ID, Latitude, Longitude)  %>%
  summarize(number_of_biomes = n())

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world)+
  geom_sf()+
  coord_sf(ylim = c(20,90))+
  geom_point(data = number_of_biomes,
              aes(x = Longitude,
                  y = Latitude,
                  col = factor(number_of_biomes)),
             alpha = 0.5)+
  scale_color_viridis_d()+
  labs(title = "Number of biomes between 8kaBP and present",
       subtitle = "according to reconstructed MAT and prec, following Whittaker 1975",
       col = "Number of biomes")

ggsave("C:/Users/lschild/Documents/pollen/bistab/plots/biomes/n_biomes.png",
       width = 10,
       height = 7)

#what is the dominant biome at sites?
dominant <- biomes %>%
  filter(Age_BP <= 8000) %>%
  group_by(Dataset_ID, Longitude, Latitude, biome)%>%
  summarize(n = n())%>%
  mutate(max = max(n))%>%
  filter(n == max)

ggplot(data = world)+
  geom_sf()+
  coord_sf(ylim = c(20,90))+
  geom_point(data = dominant,
             aes(x = Longitude,
                 y = Latitude,
                 col = biome),
             alpha = 0.6)+
  scale_color_viridis_d()+
  labs(title = "Dominant biomes between 8kaBP and present",
       col = "Biome")
ggsave("C:/Users/lschild/Documents/pollen/bistab/plots/biomes/dominant.png",
       width = 10,
       height = 7)

#timeslices with dominant biomes
slices <- biomes %>%
  filter(Age_BP <= 8000)%>%
  mutate(slice = cut(Age_BP,
                     breaks = seq(-3000,8000,1000),
                     labels = seq(-3000,7000,1000)))%>%
  group_by(Dataset_ID, Longitude, Latitude, biome, slice) %>%
  summarize(n = n())%>%
  group_by(Dataset_ID, Longitude, Latitude, slice) %>%
  mutate(max= max(n)) %>%
  filter(n == max,
         slice != "-2000",
         slice != "-3000")

p <- ggplot(data = world)+
  geom_sf()+
  coord_sf(ylim = c(20,90))+
  geom_point(data = slices,
             aes(x = Longitude,
                 y = Latitude,
                 col = biome),
             alpha = 0.6)+
  # facet_wrap(.~factor(slice,
  #                     labels = c("-1000 - 0 BP",
  #                                "0 - 1000 BP",
  #                                "1000 - 2000 BP",
  #                                "2000 - 3000 BP",
  #                                "3000 - 4000 BP",
  #                                "4000 - 5000 BP",
  #                                "5000 - 6000 BP",
  #                                "6000 - 7000 BP",
  #                                "7000 - 8000 BP")))+
  scale_color_viridis_d()+
  labs(title = "Dominant biomes in time slices",
       col = "Biome")+
  theme_bw()

a <- p + gganimate::transition_time(as.numeric(slice))

gganimate::animate(a, duration = 5, fps = 20, width = 400, height = 200, 
                   renderer = gganimate::gifski_renderer())

gganimate::anim_save("C:/Users/lschild/Documents/pollen/bistab/plots/biomes/anim.gif",
                     a)
ggsave("C:/Users/lschild/Documents/pollen/bistab/plots/biomes/slices.png",
       width = 10,
       height = 7)

number_of_biomes %>%
  ggplot(aes(x = number_of_biomes))+
  geom_bar(stat = "count")+
  labs(title = "Number of biomes between 8ka BP and present")
