#script to get optimized tree cover

rm(list = ls())

library(popa)
library(tidyverse)

#get pollen compositions
head(P)
#get LANDSAT_multirange4 cofs
apply_opti <- function(cont){
  #filter pollen dataset
  sub <- filter(P, Continent == cont)
  
  #get opti coefficients
  path <- "//dmawi.de/potsdam/data/bioing/user/lschild/opti_cluster/"
  file <- paste0("LANDSAT_multi_range_",cont,"_complete_10.csv")
  opti <- read.csv(paste0(path,file))
  
  #just ordered compositions
  head(sub[,opti$Taxa])
  sweep(sub[,opti$Taxa],
        2,
        opti$weights,
        "*")
  
  
}

#correct pollen compositions and rescale to 100

#reclass to tree cover

#save as tree opti