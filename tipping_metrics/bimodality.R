#testing for bimodality
rm(list = ls())

library(diptest)
library(popa)
library(tidyverse)

#load data
load("//dmawi.de/potsdam/data/bioing/user/lschild/surrogate/output/surrogates2.rda")
surrogates <- surrs
rm(surrs)
#R_PFTopen
pollen <- R_PFTopen %>%
  group_by(Dataset_ID) %>%
  filter(min(Age_BP) <= 2000,
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

# #get bimodality results for all records in filtered pollen
# bis <- sapply(unique(pollen$Dataset_ID),
#               function(x) bimod(pollen$tree[pollen$Dataset_ID == x]))
# 
# #combine to df
# pollen_res <- data.frame(Dataset_ID = unique(pollen$Dataset_ID),
#                          bimod = bis)
# #surrogate data

#function to get bimod for all surrogates
bi_surrs <- function(i){
  #filter surrogate df
  surr <- surrogates[[i]]%>%
    filter(Dataset_ID %in% unique(pollen$Dataset_ID),
           Age_BP <= 8000,
           realization %in% 1:50)
  #create empty df for this set of surrogates
  results <- data.frame(Dataset_ID = numeric(),
                        signal = character(),
                        bimod_frac = numeric(),
                        bimod = logical())
  #function to get bimod_frac and bimod for all realizations
  #of one dataset_ID with one signal
  get_bimod <- function(ID,
                        signal){
    record <- surr %>%
      filter(Dataset_ID == ID,
             sig == signal)
    
    inter <- function(realization){
      value <- record$value[record$realization == realization]
      age <- record$Age_BP[record$realization == realization]
      
      new_vec <- approx(x = age,
                        y = value,
                        xout = min(age):max(age))
      return(new_vec$y)
    }
    
    bis <- sapply(unique(record$realization),
                  function(x) bimod(inter(x)))
    bimod_frac <- sum(bis)/length(bis)
    bimod <- ifelse(bimod_frac > 0.5,
                    TRUE,
                    FALSE)
    return(data.frame(Dataset_ID = ID,
                      signal = signal,
                      bimod_frac = bimod_frac,
                      bimod = bimod))
  }
  records <- expand.grid(Dataset_ID = unique(surr$Dataset_ID),
                         signal = unique(surr$sig))
  bis <- lapply(1:nrow(records),
                function(x) get_bimod(records[x,1],
                                      records[x,2]))%>%
    bind_rows()
}

test <- bi_surrs(1)
sur_bimod <- lapply(1:length(surrogates),
                    bi_surrs)

names(sur_bimod) <- names(surrogates)

save(sur_bimod,
     file = "C:/Users/lschild/Documents/pollen/bistab/tipping_metrics/sur_bimod_inter.rda")


