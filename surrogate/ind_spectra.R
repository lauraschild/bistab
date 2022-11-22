#looking at individual tree cover spectra
#Laura Schild
rm(list = ls())

library(popa)
library(tidyverse)
library(PaleoSpec)

#filter PFT dataframe
PFT <- R_PFTopen %>%
  group_by(Dataset_ID) %>%
  mutate(max = max(Age_BP),
         min = min(Age_BP)) %>%
  group_by(Dataset_ID, Age_BP) %>%
  mutate(dupl = n()) %>%
  filter(dupl == 1,
         min <= 1500,
         max >= 8000,
         Age_BP <= 8000)%>%
  group_by(Dataset_ID) %>%
  mutate(samples = n(),
         res = max(diff(Age_BP))) %>%
  filter(samples >= 10,
         res <= 2500)


get_slope <- function(ID,
                      cutoff = FALSE){
  print(ID)
  tc <- PFT$tree[PFT$Dataset_ID == ID]
  age <- PFT$Age_BP[PFT$Dataset_ID == ID]
  
  #interpolate
  tc_int <- MakeEquidistant(t.x = age,
                            t.y = tc,
                            dt = 100)
  
  if(sum(tc_int, na.rm = TRUE) != 0 & sum(!is.na(tc_int))> 8){
    spec <- SpecMTM(tseries::na.remove(tc_int))
    spec <- LogSmooth(spec, 0.05)
    
    specs <- log(spec$spec)
    freq <- log(spec$freq)
    if(cutoff){
      cutoff <- 1/(2*max(diff(age)))
      specs <- log(spec$spec)[spec$freq > cutoff]
      freq <- log(spec$freq)[spec$freq > cutoff]
    }
    if(length(specs)>1){
      lm <- lm(specs ~freq)
      slope <- lm$coefficients[2]
      return(slope)
    }

  }
}


slopes <- sapply(unique(PFT$Dataset_ID),
                 get_slope,
                 cutoff = FALSE)
slopes <- unname(unlist(slopes))
hist(slopes)
mean(slopes)
