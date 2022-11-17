#surrogate data 2.0
# Laura Schild
rm(list = ls())

packages <- c("RScaling",
              "PaleoSpec",
              "dplyr",
              "popa")

lapply(packages, 
       library, 
       character.only = TRUE,
       quietly = TRUE)

#### vars####
noise <- 0.1
H <- 0.1
min <- 0.2
max <- 0.7

ts <- P$Age_BP[P$Dataset_ID == 7]

#### other funs ####
adj_trend <- function(trend,  
                      noise){ #both as zoo timeseries
  #get new upper and lower bounds
  upper <- 1- noise
  lower <- 0-noise
  #create first check
  check <- ifelse(max(trend+noise) > 1 | min(trend + noise) < 0,
                  FALSE,
                  TRUE)
  while(!check){
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
    slope <- (y_max - y_min)/(ts_max - ts_min)
    y_int <- y_max - ts_max * slope
    trend <- zoo::zoo(zoo::index(noise)*slope + y_int,
                      zoo::index(noise))
    
    #chekc again
    check <- ifelse(max(trend+noise) > 1 | min(trend + noise) < 0,
                    FALSE,
                    TRUE)
  }
  return(trend)
                      
}

####create noise####

#H (Exponent for simulated noise)
#pad (length of Lowpass Filter (has to be odd))
pad <- 10*max(diff(ts))
if(pad %% 2 == 0) pad <- pad+1
#length of timeseries to simulate (n and NN) )
start <- floor(ts[1] - max(diff(ts)) - pad)
end <- ceiling(ts[length(ts)] + max(diff(ts))+ pad)
n <- end - start +1

fn <- FractionalNoise(nn = n, 
                H = H, 
                dts = start:end, 
                sigma = noise)

####create trend####
slope <- (max -min)/(end-start)
yint <- max - end*slope
trend <- zoo::zoo(start:end * slope + yint,
                  start:end)

trend <- adj_trend(trend,
                   fn)

#add both to create complete signal
sig <- trend +fn

#### Lowpass ####
#low pass filter for Rauschen
tau <- mean(diff(ts))

filter <- Lowpass(omega.c = 1/tau,
                  n = pad)
sig_fltr <- zoo::zoo(as.vector(PaleoSpec::ApplyFilter(zoo::coredata(sig), filter)),
                     zoo::index(fn))

plot(sig)
lines(sig_fltr, col = "red")

#reduce sampling resolution


#add age uncertainty(?)