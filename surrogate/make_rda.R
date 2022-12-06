#make surrogate rda
# 02.12.

rm(list = ls())

setwd("//dmawi.de/potsdam/data/bioing/user/lschild/surrogate/output")

paths <- expand.grid(H = c("H0","H25"),
                     noise = c(5,10,15))
load_surrs <- function(i){
  surr <- data.table::fread(paste0(paths[i,"H"], "/",
                                   paths[i,"noise"], "/",
                                   "complete.csv"))  
  return(as.data.frame(surr))
}

surrs <- lapply(1:nrow(paths),
                load_surrs)
names(surrs) <- paste(paths$H, paths$noise, sep = "_")

save(surrs,
     file = "surrogates2.rda")
