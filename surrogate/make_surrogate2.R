#make surrogate2
#Laura Schild
rm(list = ls())

#packages
library(tidyverse)
library(popa)
#functions
source("~/Documents/pollen/bistab/surrogate/surrogate2.R", echo=TRUE)

IDs <- unique(P$Dataset_ID)
surrogate(9,
          0.1,
          0.5295,
          0,
          1)

test <- lapply(sample(IDs,3),
               surrogate,
               noise = 0.1,
               H = 0.5295,
               min = 0,
               max = 1)
test <- bind_rows(test)

test %>%
  filter(realization %in% 1:10)%>%
  ggplot(aes(x= Age_BP,
             y = value,
             group = realization,
             col = factor(Dataset_ID)))+
  geom_line()+
  facet_grid(sig~Dataset_ID,
             scales = "free")+
  labs(title = "Example surogate data",
       subtitle = "noise level=10%; H ~ 0.5")
