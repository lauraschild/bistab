#script to make SD and tree cover resid maps
#Laura Schild 04.11.2022
rm(list = ls())

packages <- c("dplyr",
              "ggplot2",
              "rnaturalearth",
              "popa",
              "tidyr")
lapply(packages, library, character.only = TRUE)

#get SD for each record
SD <- R_PFTopen %>%
  group_by(Dataset_ID,
           Latitude,
           Longitude) %>%
  summarize(SD = sd(tree))

#get resid for each record
resid <- R_PFTopen %>%
  filter(Age_BP <= 500) %>%
  group_by(Dataset_ID) %>%
  summarize(modern = mean(tree))%>%
  merge(LS)%>%
  mutate(resid = abs(modern - Landsat_TC))

all <- merge(SD, resid) %>%
  pivot_longer(cols = c("SD", "resid"),
               names_to = "measure",
               values_to = "value")

world <- ne_countries(scale = "medium",
                      returnclass = "sf")

ggplot(data = world)+
  geom_sf()+
  geom_point(data = all,
             aes(x = Longitude,
                 y = Latitude,
                 col = value))+
  facet_wrap(.~factor(measure,
                      labels = c("Validation residuals",
                                 "SD in time")),
             nrow = 2)+
  coord_sf(ylim = c(20,90))+
  scale_color_viridis_c()+
  theme_bw()

all2 <- merge(SD, resid)%>%
  mutate(check = resid > SD)

ggplot(data = world)+
  geom_sf()+
  geom_point(data = all2,
             aes(x = Longitude,
                 y = Latitude,
                 col = check))+
  coord_sf(ylim = c(20,70))+
  theme_bw()+
  labs(title = "Residual > SD?")
ggsave("plots/misc/map_SD.png",
       width = 10,
       height = 3)
