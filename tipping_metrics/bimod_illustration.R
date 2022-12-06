#illustrating bimodality results
#05.12.2022
rm(list = ls())
library(tidyverse)
library(popa)
library(diptest)
library(sf)
library(rnaturalearth)

load("C:/Users/lschild/Documents/pollen/bistab/tipping_metrics/sur_bimod2.rda")

#R_PFTopen
pollen <- R_PFTopen %>%
  group_by(Dataset_ID) %>%
  filter(Dataset_ID %in% sur_bimod[[1]]$Dataset_ID,
         min(Age_BP) <= 2000,
         max(Age_BP) >= 8000,
         Age_BP <= 8000)%>%
  mutate(n = n()) %>%
  filter(n > 6)

#function to return TRUE or FALSE for bimodality for a given vector
bimod <- function(v){ #vector of values
  res <- dip.test(v)
  bi <- ifelse(res$p.value < 0.05,
               TRUE,
               FALSE)
  return(bi)
}

#get bimodality results for all records in filtered pollen
bis <- sapply(unique(pollen$Dataset_ID),
              function(x) bimod(pollen$tree[pollen$Dataset_ID == x]))

#combine to df
pollen_res <- data.frame(Dataset_ID = unique(pollen$Dataset_ID),
                         bimod = bis)

filter(R_PFTopen, Dataset_ID %in% sample(true$Dataset_ID,5)) %>%
  ggplot(aes(x = Age_BP, y = tree, group = Dataset_ID,
             col  = factor(Dataset_ID)))+
  geom_line()

#### just orignal ####
#make histograms
pollen_res %>%
  group_by(bimod) %>%
  summarize(freq = n()/nrow(pollen_res)) %>%
  ggplot(aes(x = bimod, y= freq))+
  geom_bar(stat = "identity")+
  labs(title = "Bimodality in original data",
       y = "Proportion of records",
       x = "Bimodality")
#make map
pollen_res <- merge(pollen_res,
                    P[,c("Dataset_ID","Longitude","Latitude")],
                    all = FALSE)%>%
                group_by(Dataset_ID) %>%
                summarize_all(unique)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world)+
  geom_sf()+
  coord_sf(ylim = c(50, 60),
           xlim =  c(0,40),
           expand = FALSE)+
  geom_point(data = pollen_res[pollen_res$Dataset_ID == 4369,],
             aes(x = Longitude,y = Latitude, col = bimod),
             alpha = 0.5)+
  theme_bw()
  

#### just surrogates ####

#prepping data
names <- names(sur_bimod)
sur_bimod <- sur_bimod %>%
  bind_rows()

sur_bimod$surrogate <- rep(names,
                           each = nrow(sur_bimod)/6)
sur_bimod$surrogate <- factor(sur_bimod$surrogate,
                              levels = c("H0_5", "H0_10","H0_15",
                                         "H25_5","H25_10","H25_15"))
#illustrate bimod_frac
sur_bimod %>%
  ggplot( aes(x = bimod_frac,
              fill = surrogate)) +
  geom_histogram(position = 'identity',
                 bins = 10) +
  facet_grid(surrogate~signal)+
  labs(fill="")+
  scale_fill_viridis_d()

#barplot with counts
sur_bimod %>%
  ggplot(aes(x = bimod, fill = surrogate))+
  geom_bar(stat = "count",
           position = "dodge")+
  facet_grid(.~signal)+
  scale_fill_viridis_d()

#histogram with percentages
sur_bimod %>%
  group_by(surrogate, signal) %>%
  mutate(count = n())%>%
  group_by(surrogate, signal, bimod)%>%
  summarize(count = unique(count),
            percent = n()/count)%>%
  ggplot(aes(x = bimod, fill = surrogate, y = percent))+
  geom_bar(stat = "identity",
           position = "dodge")+
  facet_grid(.~signal)+
  scale_fill_viridis_d()+
  labs(y = "proportion of records")

sur_bimod %>%
  filter(signal == "steps",
         bimod == FALSE)%>%
  select(Dataset_ID) %>%
  table()%>%
  unname()%>%
  data.frame() %>%
  ggplot(aes(x = Freq))+
  geom_bar(stat = "count")+
  labs(x = "number of times not itdentified as bimodal",
        y = "number of records",
        title = "Are always the same records not being identified as steps?")

load("//dmawi.de/potsdam/data/bioing/user/lschild/surrogate/output/surrogates2.rda")

steps <- sur_bimod %>%
          filter(signal == "steps",
                 bimod == FALSE)%>%
          select(Dataset_ID) %>%
          table()

steps <- data.frame(Dataset_ID = names(steps),
                    freq = c(unname(steps)))


R_PFTopen %>%
  filter(Dataset_ID %in% steps$Dataset_ID[steps$freq > 5],
         Age_BP <= 8000)%>%
  ggplot(aes(x = Age_BP, 
             y = tree, 
             col = factor(Dataset_ID), 
             group = Dataset_ID))+
  geom_line()+
  xlim(0,8000)

R_PFTopen%>%
  filter(Age_BP <= 8000,
         Dataset_ID %in% unique(sur_bimod$Dataset_ID)) %>%
  group_by(Dataset_ID) %>%
  mutate(n = n(),
         cat = ifelse(Dataset_ID %in% steps$Dataset_ID[steps$freq > 5],
                      "bad_guy",
                      "good_guy"))%>%
  ggplot(aes(x = n, fill = cat))+
  geom_histogram(position = "identity",
                 alpha = 0.5)
  
#prepping data
# names <- names(surrs)
# surrs <- surrs %>%
#   bind_rows()
# 
# surrs$surrogate <- rep(names,
#                            each = nrow(surrs)/6)
# surrs$surrogate <- factor(surrs$surrogate,
#                               levels = c("H0_5", "H0_10","H0_15",
#                                          "H25_5","H25_10","H25_15"))
# head(surrs)
# 
# surrs %>%
#   filter(Age_BP <= 8000,
#          Dataset_ID %in% steps$Dataset_ID[steps$freq > 5],
#          sig == "steps",
#          realization %in% 1:5)%>%
#   ggplot(aes(x = Age_BP, y = value, 
#              group = interaction(Dataset_ID, realization,surrogate),
#              col = factor(Dataset_ID)))+
#   geom_line(alpha = 0.2)+
#   theme(legend.position = "none")
