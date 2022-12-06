#histograms
#Laura Schild
rm(list = ls())
packages <- c("popa",
              "tidyverse",
              "PaleoSpec")
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
         min <= 2000,
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
hist(df$MAT, 
     breaks = 50,
     main = "Histogram MAAT_MAT")
hist(df$tree,
     breaks = 50,
     main = "Histogram tree cover")

#calculate mean per record
mean <- df %>%
  group_by(Dataset_ID) %>%
  summarize(MAT = mean(MAT),
            tree = mean(tree))
hist(mean$MAT,
     breaks = 50,
     main = "Hist over space: MAT")
hist(mean$tree,
     breaks = 50,
     main = "Hist over space: TC")

#subtract mean from each record
time <- df %>%
  group_by(Dataset_ID) %>%
  mutate(mean_MAT = mean(MAT),
         mean_tree = mean(tree),
         MAT_time = MAT - mean_MAT,
         tree_time = tree - mean_tree)

hist(time$MAT_time,
     breaks = 50,
     main = "Hist over time: MAT")
hist(time$tree_time,
     breaks = 50,
     main = "Hist over time: tree")
