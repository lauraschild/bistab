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
noise <- 0.15    #desired noise level
H <- 0.25     #scaling for fractional noise
min <- 0.2      #min for trend/step
max <- 0.8      #maximum for trend/step
ID <- 518       #Dataset_ID


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

lowsub <- function(trend,  #underlying signal as zoo
                   noise,  #fractional noise as zoo
                   timesteps,     #timeseries to be subsampled to as vector
                   ID){    #Dataset_ID as numeric
  
  #add both to create complete signal
  sig <- trend + noise
  
  #adjust signal to be within boundaries
  sig[sig < 0] <- 0
  sig[sig > 1] <- 1
  
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
  irreg <- sapply(timesteps, function(x) zoo::coredata(sig_fltr[paste(x)]))
  irreg <- zoo::zoo(irreg, timesteps)

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
  tau <- mean(diff(timesteps))
  
  #check if bacon age model exists for the record
  bacon <- !is.null(Bacon.100[[paste(ID)]])
  
  if((1/tau) < 0.5 & bacon){ #bacon check and testing if tau will work for Lowpass
    #set coefficient to increase noise depending on H
    cof <- ifelse(H == 0,
                  1/0.8,
                  1/0.85)
    
    fn <- FractionalNoise(nn = n, 
                          H = H,
                          mu = 0,
                          dts = start:end, 
                          sigma = noise*cof)

    
    #center timeseries around 0
    fn <- fn - mean(fn)
    
    ####create trend####
    slope <- (max -min)/(end-start)
    yint <- max - end*slope
    trend <- zoo::zoo(start:end * slope + yint,
                      start:end)
  
    # trend <- adj_trend(trend,
    #                    fn,
    #                    type = "trend")
    
    #### create step ####
    jump <- ifelse(4000 %in% zoo::index(fn),
                   which(zoo::index(fn) == 4000),
                   round(median(1:length(fn))))
    step1 <- rep(min, jump)
    step2 <- rep(max, length(fn)- jump)
    #make ts
    steps <- zoo::zoo(c(step1,step2), start:end)
    
    #adjust steps to be within 0 and 1
    # steps <- adj_trend(steps,
    #                    fn,
    #                    "step")
    
    #### create constant####
    cons <- rep(mean(c(max,min)), length(fn))
    cons <- zoo::zoo(cons, start:end)
    
    #adjust constant to be within 0 and 1
    # cons <- adj_trend(cons,
    #                   fn,
    #                   "cons")
    # 
    
    #subsample all signals and add age uncertainty
    df <- lapply(list(trend,cons,steps),
                 lowsub,
                 noise = fn,
                 timesteps = timesteps,
                 ID = ID)%>%
      bind_rows()%>%
      mutate(Dataset_ID = ID)
    #add column for underlying signal
    df$sig <- rep(c("trend", "cons", "steps"),
                  each = nrow(df)/3)
    
    #write record as csv
    data.table::fwrite(df,
                       paste0("/bioing/user/lschild/surrogate/output/H",
                              sub("0.","",H),"/",
                              ID,"_",noise,".csv"))

    return(df)
  }
}

#### some testing stuff ####

test_buff <- function(H,
                      noise){
  fn <- FractionalNoise(nn = n, 
                        H = H,
                        mu = 0,
                        dts = start:end, 
                        sigma = noise)
  
  #center timeseries around 0
  fn <- fn - mean(fn)
  sds <- c()
  for(signal in list(trend, steps)){
    sig <- signal + fn
    sig[sig < 0] <- 0
    sig[sig > 1] <- 1
    fn2 <- sig - signal
    sds <- c(sds,sd(fn2))
  } 
  return(sds)
}


H0 <- lapply(seq(0.05,0.3,0.025),
             test_buff,
             H = 0)

H0 <- data.frame(do.call(rbind, H0))%>%
  mutate(noise = seq(0.05, 0.3, 0.025),
         H = 0)
names(H0)[1:2] <- c("trend", "steps")

H10 <- lapply(seq(0.05,0.3,0.025),
             test_buff,
             H = 0.1)

H10 <- data.frame(do.call(rbind, H10))%>%
  mutate(noise = seq(0.05, 0.3, 0.025),
         H = 10)
names(H10)[1:2] <- c("trend", "steps")

H25 <- lapply(seq(0.05, 0.3, 0.025),
              test_buff,
              H = 0.25)

H25 <- data.frame(do.call(rbind, H25))%>%
  mutate(noise = seq(0.05, 0.3, 0.025),
         H = 0.25)
names(H25)[1:2] <- c("trend", "steps")

buffs <- rbind(H0, H10, H25)%>%
  pivot_longer(cols = c("trend", "steps"),
               names_to = "signal",
               values_to = "rest_noise")

ggplot(buffs, aes(x = noise, y = rest_noise, col = factor(H),
                  linetype = signal,
                  group = interaction(signal,H)))+
  geom_line()+
  geom_abline(slope = 1, col = "yellow")+
  labs(title = "Loss of noise when cutting fixing boundaries",
       sub = "step and trend min max set to 0.2 and 0.8 resp.",
       x = "original noise level",
       y = "remaining noise level")
