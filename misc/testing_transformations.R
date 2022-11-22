rm(list = ls())

library(popa)
library(RScaling)
library(tidyverse)

noise <- 0.1    #desired noise level
H <- 0.5295     #scaling for fractional noise
min <- 0      #min for trend/step
max <- 1      #maximum for trend/step
ID <- 11        #Dataset_ID

ID <- 7
adj_trend <- function(trend,  
                      noise,  #both as zoo timeseries
                      type){  #type of signal (trend or step)
  #get new upper and lower bounds
  upper <- 1- noise
  lower <- 0-noise
  #create first check
  check <- ifelse(max(trend+noise) > 1 | min(trend + noise) < 0,
                  FALSE,
                  TRUE)
  count <- 0
  while(!check){
    if(count > 20){
      trend <- zoo::zoo(NA, zoo::index(noise))
      break
    }
    count <- count + 1
    #new max min
    undershoot <- lower - trend
    ts_min <- zoo::index(trend)[which(undershoot == max(undershoot))]
    if(max(undershoot) >= 0){
      y_min <- as.numeric(lower[paste(ts_min)])
    }else{
      y_min <- as.numeric(trend[paste(ts_min)])
    }
    
    overshoot <- trend - upper
    ts_max <- zoo::index(trend)[which(overshoot == max(overshoot))]
    if(max(overshoot) >= 0){
      y_max <- as.numeric(upper[paste(ts_max)])
    }else{
      y_max <- as.numeric(trend[paste(ts_max)])
    }
    
    #get new slope and intcp to fit those points
    if(type == "trend"){
      slope <- (y_max - y_min)/(ts_max - ts_min)
      y_int <- y_min - ts_min * slope
      trend <- zoo::zoo(zoo::index(noise)*slope + y_int,
                        zoo::index(noise))
    }else if(type =="step"){
      step1 <- rep(y_min, round(median(1:length(noise))))
      step2 <- rep(y_max, length(noise)- round(median(1:length(noise))))
      #make ts
      trend <- zoo::zoo(c(step1,step2), zoo::index(noise))
    }else if(type == "cons"){
      if(max(overshoot) > 0){
        if(max(undershoot > 0)){
          trend <- rep(NA, length(noise))
        }else{
          trend <- rep(y_max, length(noise))
        }
      }else{
        trend <- rep(y_min, length(noise))
      }
      trend <- zoo::zoo(trend, zoo::index(noise))
      
    }
    
    #check again
    check <- ifelse(max(trend+noise) > 1+1e-15 | min(trend + noise) < -1e-15,
                    FALSE,
                    TRUE)
  }
  return(trend)
}


timesteps <- P$Age_BP[P$Dataset_ID == ID]
#pad (length of Lowpass Filter (has to be odd))
pad <- 10*max(diff(timesteps))
if(pad %% 2 == 0) pad <- pad+1
#length of timeseries to simulate (n and NN) )
start <- floor(timesteps[1] - max(diff(timesteps)) - pad)
end <- ceiling(timesteps[length(timesteps)] + max(diff(timesteps))+ pad)
n <- end - start +1
tau <- mean(diff(timesteps))

fn <- FractionalNoise(nn = n, 
                      H = H,
                      mu = 0,
                      dts = start:end, 
                      sigma = noise)

#center timeseries around 0
fn <- fn - mean(fn)

####create trend####
slope <- (max -min)/(end-start)
yint <- max - end*slope
trend <- zoo::zoo(start:end * slope + yint,
                  start:end)

plot(trend + fn,
     xlab = "Age_BP",
     ylab = "Value",
     main = "Testing normalizations")

norm <- trend+fn
tr_norm <- (trend -min(norm))/(max(norm)-min(norm))
norm <- (norm - min(norm))/(max(norm)-min(norm))
sd_norm <- sd(norm- tr_norm)

lines(norm, col = "blue")

log <- 1/(1+exp(-(trend+fn)))
log_tr <- 1/(1+exp(-trend))
sd_log <- sd(log-log_tr)

lines(log, col = "red")

trend_adj <- adj_trend(trend,
                   fn,
                   type = "trend")

lines(trend_adj + fn, col ="green")

legend(x= 6000,
       y=0.4,
       legend = c("original trend + noise",
                  "Max-min normalized",
                  "Logistic transform",
                  "adjusted trend"),
       col = c("black",
               "blue",
               "red",
               "green"),
       lty = 1,
       cex = 0.8)

