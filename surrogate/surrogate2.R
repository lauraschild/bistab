#functions to make surrogate data 2.0
# Laura Schild
packages <- c("RScaling",
              "dplyr",
              "popa")

lapply(packages, 
       library, 
       character.only = TRUE,
       quietly = TRUE)

#### vars for testing####
# noise <- 0.1    #desired noise level
# H <- 0.5295     #scaling for fractional noise
# min <- 0      #min for trend/step
# max <- 1      #maximum for trend/step
# ID <- 26608        #Dataset_ID
# 
# timesteps <- P$Age_BP[P$Dataset_ID == ID]

#### helper funs ####
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
      step1 <- rep(y_min, median(1:length(noise)))
      step2 <- rep(y_max, length(noise)- median(1:length(noise)))
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

lowsub <- function(trend,  #underlying signal as zoo
                   noise,  #fractional noise as zoo
                   timesteps,     #timeseries to be subsampled to as vector
                   ID){    #Dataset_ID as numeric
  
  #add both to create complete signal
  sig <- trend + noise
  
  #### Lowpass ####
  #low pass filter for Rauschen
  tau <- mean(diff(timesteps))
  #pad (length of Lowpass Filter (has to be odd))
  pad <- 10*max(diff(timesteps))
  if(pad %% 2 == 0) pad <- pad+1
  
  filter <- Lowpass(omega.c = 1/tau,
                    n = pad)
  sig_fltr <- zoo::zoo(as.vector(ApplyFilter(zoo::coredata(sig), filter)),
                       zoo::index(noise))
  
  #reduce sampling resolution
  irreg <- sig_fltr[paste(timesteps)]
  
  #add age uncertainty
  names(Infl.Bacon.100) <- names(Bacon.100)
  ensemb <- lapply(1:100,function(i)ChangeTime(irreg, 
                                               Infl.Bacon.100[[paste(ID)]][,i], 
                                               roffset = 353.55))

  #make into dataframe
  ensemb <- lapply(1:length(ensemb), 
                   function(x) data.frame(Age_BP = zoo::index(ensemb[[x]]), 
                                          value = zoo::coredata(ensemb[[x]]),
                                          realization = x))%>%
    bind_rows()
  
  return(ensemb)
  
}

#### the big fun####
#creates 100 surrogate datasets for an underlying trend, step or constant each
#returns a dataframe with Age_BP, value, Age model realization, type of signal
surrogate <- function(ID,     #Dataset_ID
                      noise,  #noise level
                      H,      #H
                      min,    #desired minimum value for underlying signal
                      max){   #desired maximum values for underlying signal

  packages <- c("RScaling",
                "dplyr",
                "popa")
  
  lapply(packages, 
         library, 
         character.only = TRUE,
         quietly = TRUE)
  print(ID)
  ####create noise####
  timesteps <- round(P$Age_BP[P$Dataset_ID == ID])
  #pad (length of Lowpass Filter (has to be odd))
  pad <- 10*max(diff(timesteps))
  if(pad %% 2 == 0) pad <- pad+1
  #length of timeseries to simulate (n and NN) )
  start <- floor(timesteps[1] - max(diff(timesteps)) - pad)
  end <- ceiling(timesteps[length(timesteps)] + max(diff(timesteps))+ pad)
  n <- end - start +1
  
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
  
  trend <- adj_trend(trend,
                     fn,
                     type = "trend")
  
  #### create step ####
  step1 <- rep(min, median(1:length(fn)))
  step2 <- rep(max, length(fn)- median(1:length(fn)))
  #make ts
  steps <- zoo::zoo(c(step1,step2), start:end)
  
  
  
  steps <- adj_trend(steps,
                      fn,
                      "step")
  
  #### create constant####
  cons <- rep(mean(c(max,min)), length(fn))
  cons <- zoo::zoo(cons, start:end)
  
  cons <- adj_trend(cons,
                    fn,
                    "cons")
  
  
  
  df <- lapply(list(trend,cons,steps),
               lowsub,
               noise = fn,
               timesteps = timesteps,
               ID = ID)%>%
    bind_rows()%>%
    mutate(Dataset_ID = ID)
  df$sig <- rep(c("trend", "cons", "steps"),
                each = nrow(df)/3)

  return(df)
}
