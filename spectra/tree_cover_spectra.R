#tree cover spectral analysis
#Laura Schild
rm(list = ls())
packages <- c("popa",
              "dplyr",
              "tidyr",
              "PaleoSpec",
              "ggplot2")
lapply(packages, require, character.only = TRUE)

#### prepare data####
#combine temp with pft df
#check for ID frequencies since Ages are not the same
#(were rounded for Pangeae upload of reconstructions)
t <- table(Temp$Dataset_ID)
c <- table(R_PFTopen$Dataset_ID)
t <- data.frame(ID = names(t),
                count = as.numeric(t))
c <- data.frame(ID = names(c),
                count = as.numeric(c))
comb <- merge(c,t,
              by = "ID")%>%
  mutate(check = count.x == count.y)%>%
  filter(check == TRUE)

#use only records which have the same amount of samples in Temp and PFT
IDs <- comb$ID

#combine PFT and T
PFT <- R_PFTopen %>%
  filter(Dataset_ID %in% IDs) %>%
  arrange(Dataset_ID, Age_BP)

Clim <- Temp %>%
  filter(Dataset_ID %in% IDs) %>%
  arrange(Dataset_ID, Age_AD_BP)

df <- cbind(PFT, MAT = Clim[,"MAAT_MAT"])
nrow(df)
#further filtering
df <- df %>%
  group_by(Dataset_ID) %>%
  mutate(max = max(Age_BP),
         min = min(Age_BP))%>%
  filter(max >= 8000,
         min <= 0,
         Age_BP <= 8000) %>%
  group_by(Dataset_ID, Age_BP) %>%
  mutate(dupl = n())%>%
  filter(dupl == 1)%>%
  group_by(Dataset_ID) %>%
  mutate(sample = n()) %>%
  filter(sample > 6)%>%
  arrange(Dataset_ID, Age_BP)
  

nrow(df)
res <- sapply(unique(df$Dataset_ID),
              function(ID) mean(diff(df$Age_BP[df$Dataset_ID == ID])))

mean_res <- mean(res)

####make histograms ####
hist(df$MAAT_MAT, 
     breaks = 50,
     main = "Histogram MAAT_MAT")
hist(df$tree,
     breaks = 50,
     main = "Histogram tree cover")

####functions ####
source("~/Documents/pollen/bistab/spectra/spectrum_functions.R", echo=FALSE)

####applications #####

#get tc spectra
specs_tc <- lapply(unique(df$Dataset_ID)[1:100],
                record_spec,
                type = "tc")
#get T spectra
specs_temp <- lapply(unique(df$Dataset_ID)[1:100],
                     record_spec,
                     type = "temp")


#get averages and plot subset of spectra
avrg_temp <- plot_spectra("temp")

avrg_tc <- plot_spectra("tc")


#get slopes and plot log plot
cuts <- c(1,mean_res, 2*mean_res,2.5*mean_res, max(res))

for(cut in cuts){
  get_slope("temp",
            cutoff = cut)
  ggsave(paste0("plots/temp_spectrum_",round(cut),".png"))
  
  get_slope("tc", 
            cutoff = cut)
  ggsave(paste0("plots/tc_spectrum_",round(cut),".png"))
  
}