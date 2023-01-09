#script to get optimized tree cover

rm(list = ls())

library(popa)
library(tidyverse)
source("~/Documents/pollen/pollen/A/function_reclass.R", echo=TRUE)

open <- read.csv("C:/Users/lschild/Documents/pollen/data/open_sites.csv")%>%
  mutate(open_frac = 1- open_frac)
#get pollen compositions
head(P)
#get LANDSAT_multirange4 cofs
apply_opti <- function(cont){
  #filter pollen dataset
  sub <- filter(P, Continent == cont)
  
  #get opti coefficients
  path <- "//dmawi.de/potsdam/data/bioing/user/lschild/opti_cluster/"
  file <- paste0("LANDSAT_multi_range4_",cont,"_complete_10.csv")
  opti <- read.csv(paste0(path,file))
  bounds <- read.csv(paste0("//dmawi.de/potsdam/data/bioing/user/lschild/data/bounds_multirange4_",cont,".csv"))%>%
    select(Taxa,
           medians) %>%
    rename(weights = medians)
  
  #order weights to be same as cols
  weights <- rbind(opti,
                   bounds[!(bounds$Taxa %in% opti$Taxa),])
  weights <- weights[match(names(sub[,-(1:5)]), weights$Taxa),]
  weights[is.na(weights)] <- 1
  
  corrected <-   sweep(sub[,-(1:5)],
                       2,
                       weights$weights,
                       "*")
  sub <- cbind(sub[,!(names(sub) %in% (opti$Taxa))],
               corrected)
  #rescale to 100
  sub <- cbind(sub[,1:5],
               t(apply(sub[,-(1:5)],
                       1,
                       function(x) 100*x/sum(x))))
  
  #return opti
  return(sub)
}

#lapply apply opti
optis <- lapply(c("Asia",
                  "North_America",
                  "Europe"),
                apply_opti) %>%
  bind_rows()


#save opti pollen
write.csv(optis,
          "C:/Users/lschild/Documents/pollen/bistab/new_data/opti.csv",
          row.names = FALSE)

#reclass all
PFT <- reclass(optis)%>%
  select(1:5,
         tree, shrub, grass, herb, BD, BE, ND, NE) %>%
  merge(open,
        by = "Dataset_ID")%>%
  mutate(tree = tree * open_frac,
         shrub = shrub * open_frac,
         grass = grass * open_frac,
         BD = BD * open_frac,
         BE = BE *open_frac,
         ND = ND * open_frac,
         NE = NE * open_frac)%>%
  select(-open_frac)
write.csv(PFT,
          "C:/Users/lschild/Documents/pollen/bistab/new_data/PFT_opti.csv",
          row.names = FALSE)


