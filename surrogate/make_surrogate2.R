#make surrogate2
#Laura Schild
rm(list = ls())

#packages
library(tidyverse)
library(popa)
#functions
source("~/Documents/pollen/bistab/surrogate/surrogate2.R", echo=TRUE)

IDs <- unique(P$Dataset_ID)

IDs <- c(518,1824,4404)
Hs <- c(0,0.1,0.25)
ns <- c(0.05,0.1)
vars <- expand.grid(H = Hs,
                    noise = ns)

for(n in 1:nrow(vars)){
  H <- vars$H[n]
  noise <- vars$noise[n]
  test <- lapply(IDs,
                 surrogate,
                 noise = noise,
                 H = H,
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
               scales = "free_x")+
    labs(title = "Example surogate data",
         subtitle = paste0("noise level = ",noise,"%; H = ",H))
  
  ggsave(paste0("plots/surrogate_examples/new",H,"_",noise,".png"))
  
}

