#combine surrogates and save locally

rm(list = ls())

library(popa)
library(PaleoSpec)
library(tidyverse)
library(parallel)

setwd("C:/Users/lschild/Documents/pollen/bistab/new_data/")

#make dataframes + filter
Hs <- c(0)
ns <- c(0.15)
vars <- expand.grid(H = Hs,
                    noise = ns)


#function to combine surrogate records from server into one df per H and per noise level
get_surrs <- function(row){
  H <- vars$H[row]
  noise <- vars$noise[row]
  
  files <- list.files(paste0("H",
                             sub("0.","",H),
                             "/",noise*100))
  files <- paste0("H",
                  sub("0.","",H),
                  "/",noise*100,
                  "/",files)
  
  surrs <- lapply(files,function(x) read.csv(x))%>%
    bind_rows()
  
  file <- paste0("C:/Users/lschild/Documents/pollen/bistab/new_data/H",
                 sub("0.","",H),"/",noise*100,"/","complete.csv")
  data.table::fwrite(surrs,
                     file)
  
  return(surrs)
  
}

surrogates <- lapply(1:nrow(vars),
                     get_surrs)
names(surrogates) <- sapply(1:nrow(vars),
                            function(n) paste(vars[n,], collapse = "_"))
# save(surrogates,
#      file = "C:/Users/lschild/Documents/pollen/bistab/new_data/surrogates.rda")
