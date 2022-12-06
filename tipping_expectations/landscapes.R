#calculate stability landscapes
rm(list=ls())
library(ggplot2)
library(dplyr)
library(KernSmooth)
library(photobiology)
library(popa)
library(pastecs)

#source function for finding peaks
source("//dmawi.de/potsdam/user/personal/lschild/Stability/skripts/find_peaks_function.R", echo=TRUE)

setwd("//dmawi.de/potsdam/data/bioing/data/boreal_stability_study")

#### prepare data####
#combine temp with pft df
#check for ID frequencies since Ages are not the same
#(were rounded for Pangeae upload of reconstructions)
prec <- read.csv("C:/Users/lschild/Documents/pollen/pollen/ordination/clim_recons/datasets/local_recons_prec.csv")
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
  cbind(Prec_WAPLS = prec[,"Prec_WAPLS"]) %>%
  filter(Dataset_ID %in% IDs) %>%
  arrange(Dataset_ID, Age_AD_BP)

af <- cbind(PFT, MAT = Clim[,"MAAT_WAPLS"], PREC = Clim[,"Prec_WAPLS"])

ggplot(af[sample(1:nrow(af),10000),], aes(x = MAT, y = PREC, col = tree))+
  geom_point(alpha = 0.5)+
  scale_color_viridis_b()

#plot
plot(tree ~ MAT,
     data = af[sample(1:nrow(af),5000),])

#create matrix with TC and summer temp values
x <- cbind(af$tree,af$MAT)
x <- x[(!is.na(af$MAT)),]


temps <- unique(round(x[,2]))
eq <- data.frame(temp = numeric(),
                 TC = numeric(),
                 class = factor())
eq_test <- data.frame(temp =numeric(),
                      TC = numeric(),
                      class = factor())

for (temp in temps){
  print(temp)
  df <- x[x[,2]< temp + 0.5 & x[,2] >= temp - 0.5,]
  df <- matrix(df,
               ncol = 2)
  if (nrow(df)>1 & sd(df[,1])> 0){
  
    bw <- 1.06 * sd(df[,1])/length(df[,1])^(1/5)
  
    d <- density(df[,1],
                 bw = bw,
                 kernel = "gaussian")
    
    #determine local maxima and minima
    ts_y <- ts(d$y)
    tp <- turnpoints(ts_y)

    #plot for checking
    plot(d)
    points(d$x[tp$tppos], d$y[tp$tppos], col = "red")
    
    #collect max and mi with thresholds
    max <- get_peaks(d$x,
                     d$y,
                     ignore_threshold = 0.1)
    min <- get_valleys(d$x,
                       d$y,
                       ignore_threshold = 0.1)
    min <- cbind(rep(temp, nrow(min)),
                 min$x,
                 rep("min", nrow(min)))
    max <- cbind(rep(temp, nrow(max)),
                 max$x,
                 rep("max",nrow(max)))
    
    #add to eq_test
    eq_test <- rbind(eq_test, rbind(min,max))
    
    #collect points (SEPERATE BY PITS AND PEAKS)
    pits <- cbind(rep(temp,length(d$x[tp$pits])),
                  d$x[tp$pits],
                  rep("min",length(d$x[tp$pits])))
    peaks <- cbind(rep(temp, length(d$x[tp$peaks])),
                   d$x[tp$peaks],
                   rep("max", length(d$x[tp$peaks])))

    #add to df
    eq <- rbind(eq,rbind(pits,peaks))
  }
  else{
    print(paste("not enough values for temp",temp,sep=" "))
    next
  }
}

#plot with all max and min
names(eq) <- c("temp","TC","class")
eq$temp <- as.numeric(eq$temp)
eq$TC <- as.numeric(eq$TC)
eq$class <- as.factor(eq$class)

plot(x = eq$temp, y = eq$TC, col = eq$class,
     xlab = "MAT",
     ylab = "Tree Cover in Percent")

#plot max and min with thresholds
names(eq_test) <- c("temp","TC","class")
eq_test$temp <- as.numeric(eq_test$temp)
eq_test$TC <- as.numeric(eq_test$TC)
eq_test$class <- as.factor(eq_test$class)

plot(x = eq_test$temp, y = eq_test$TC, col = eq_test$class,
     xlab = "MAT",
     ylab = "Tree Cover in Percent")

ggplot(af[sample(nrow(af),5000),], aes(x=MAT, y= tree))+
  geom_point(alpha = 0.5,
             col = "gray30")+
  geom_point(data = eq_test,
             aes(x = temp, y = TC, col = class),
             shape =19,
             size =2)+
  scale_color_manual(labels = c("stable", "unstable"),
                     values = c("darkblue","gold"))+
  labs(col = "State",
       x = "MAT_WAPLS",
       y = "Tree Cover in Percent",
       title = "Stabilty Landscapes")
