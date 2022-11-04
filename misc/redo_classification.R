#script to redo PFT reclassifications and save in package
#Laura Schild- 04.11.2022
rm(list = ls())
library(dplyr)
source("~/Documents/pollen/pollen/A/function_read_pollen.R", echo = FALSE)
source("~/Documents/pollen/pollen/A/function_reclass.R", echo = FALSE)

#### pure pollen ####
#load pollen and REVEALs data
P <- data.frame(load_pollen("complete",
                "Pollen"))%>%
  mutate(Area = ifelse(Area %in% c("NamericaW", "NamericaE"),
                       "North_America",
                       Area)) %>%
  rename(Age_BP = Age_AD_BC,
         Continent = Area)%>%
  arrange(Dataset_ID, Age_BP)
R <- data.frame(load_pollen("complete",
                 "CF_US"))%>%
  rename(Continent = Region,
         Age_BP = Age_AD_BC)%>%
  arrange(Dataset_ID, Age_BP)

#save pollen and REVEALS as rda file to popa
save(list = c("P","R"),
     file = "C:/Users/lschild/Documents/pollen/popa/data/pollen.rda")

#### reclassification ####
#reclass no open correction
cols <- c("Dataset_ID","Longitude","Latitude",
          "Age_BP", "Continent", "shrub","tree","grass","herb",
          "BD","BE","ND","NE")
P_PFT <- reclass(P)%>%
  select(all_of(cols))

R_PFT <- reclass(R)%>%
  select(all_of(cols))


#reclass with openness correction
#see areas classfied as open here: https://spaces.awi.de/display/ENVI/REVEALS+Validation
#under validation with weights
open <- read.csv("C:/Users/lschild/Documents/pollen/data/open_sites.csv")
save(open,
     file = "C:/Users/lschild/Documents/pollen/popa/data/open.rda")
open <- merge(P_PFT, open)%>%
  select(open_frac)%>%
  mutate(open_frac = 1 - open_frac)
P_PFTopen <- cbind(P_PFT[,1:5], P_PFT[,-(1:5)]*open$open_frac)

open <- merge(R_PFT, open)%>%
  select(open_frac)%>%
  mutate(open_frac = 1 - open_frac)
R_PFTopen <- cbind(R_PFT[,1:5], R_PFT[,-(1:5)]*open$open_frac)

#save all PFT objects to package
save(list = c("P_PFT","R_PFT", "P_PFTopen","R_PFTopen"),
     file = "C:/Users/lschild/Documents/pollen/popa/data/PFT.rda")

#### Landsat TC####

#load Landsat and save to package
LS <- read.csv("C:/Users/lschild/Documents/pollen/data/TC_Landsat/TC_US_100.csv") %>%
  select(Dtst_ID, mean) %>%
  rename(Dataset_ID = Dtst_ID,
         Landsat_TC = mean)
save(LS,
     file = "C:/Users/lschild/Documents/pollen/popa/data/LS.rda")
