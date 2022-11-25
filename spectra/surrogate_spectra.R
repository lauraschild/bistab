#check surrogate data spectra

rm(list = ls())

library(popa)
library(PaleoSpec)
library(tidyverse)
source("~/Documents/pollen/bistab/spectra/spectrum_functions.R", echo=FALSE)

setwd("//dmawi.de/potsdam/data/bioing/user/lschild/surrogate/output")

#make dataframes + filter
Hs <- c(0,0.25)
ns <- c(0.05,0.1)
vars <- expand.grid(H = Hs,
                    noise = ns)

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
  surrs <- lapply(files,function(x) data.frame(data.table::fread(x)))%>%
    bind_rows()
  
  surrs <- surrs %>%
    group_by(Dataset_ID) %>%
    mutate(max = max(Age_BP),
           min = min(Age_BP))%>%
    filter(max >= 8000,
           min <= 0,
           Age_BP <= 8000) %>%
    group_by(Dataset_ID, Age_BP, sig, realization) %>%
    mutate(dupl = n())%>%
    filter(dupl == 1)%>%
    group_by(Dataset_ID, sig, realization) %>%
    mutate(sample = n()) %>%
    filter(sample > 6)%>%
    arrange(Dataset_ID, Age_BP, realization, sig)
  
  return(surrs)
}

surrogates <- lapply(1:nrow(vars),
                     get_surrs)

#calculate mean resolutions per record per realization
ages <- surrogates[[1]]%>%
  filter(sig == "cons") %>%
  select(Age_BP,Dataset_ID, realization)%>%
  arrange(Dataset_ID, realization, Age_BP) %>%
  mutate(Bacon = paste0(Dataset_ID,"_",realization))

bacons <- unique(ages$Bacon)

res <- sapply(bacons,
              function(x) mean(diff(ages$Age_BP[ages$Bacon == x])))
mean(res)
#prepare spectra per surrogate set (H and noise value) and per signal (cons,trend,step)
