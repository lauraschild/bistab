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
noise <- 0.1    #desired noise level
H <- 0.5295     #scaling for fractional noise
min <- 0.2      #min for trend/step
max <- 0.7      #maximum for trend/step
type <- "trend" #type of underlying signal wanted
ID <- 7         #Dataset_ID

ts <- P$Age_BP[P$Dataset_ID == ID]

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

# Simple function to change the timesteps of a zoo object and add a random offset
ChangeTime<-function(zser, # Input zoo timeseries
                     time, # New timesteps
                     roffset=0, # Standard deviation of the offset drawn from a normal distribution
                     meanoffset=500 # Mean of the offset drawn from a normal distribution
) if(length(zser)<2){
  NA
}else{
  zoo::zoo(zoo::coredata(zser),
           order.by = time[1:length(zser)]+rnorm(1,mean = meanoffset,sd = roffset))
}
# Inflates the errors from the Bacon age model realizations
InflateErrors<-function(mat, # Matrix of realizations of the Bacon age model
                        fac, # Factor to inflate the spread of the realizations 
                        fixed=FALSE, # Not needed, keep to FALSE
                        na.res=NA # Useful if some realizations are missing so rather than return NAs you can return the mean realization, 
                        # useful in the sense that if you are making a loop over all pollen records you can avoid problems with 
                        # the 29 records which missing age realizations
){
  if(is.na(mat[1])) return(matrix(rep(na.res,100),length(na.res),100))
  
  if(sum(apply(mat,2,sd),na.rm = TRUE)<0.1) return(matrix(rep(na.res,100),length(na.res),100))
  
  mean.realization<-apply(mat,1,mean) # Calculates the mean realization
  # if(fac<50){fixed<-FALSE}else{fixed<-TRUE}
  if(fixed){
    sd.realization<-apply(mat,1,sd)
    fac.vec<-fac/sd.realization
  }else{
    fac.vec<-fac
  }
  if(fac>50) fac.vec<-fac/mean(apply(mat,1,sd))
  rescaled.sd<-(mat-mean.realization)*fac.vec
  rescaled.mat<-rescaled.sd+mean.realization
  return(rescaled.mat)
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
irreg <- sig_fltr[paste(ts)]

#add age uncertainty
names(Infl.Bacon.100) <- names(Bacon.100)
ensemb <- lapply(1:100,function(i)ChangeTime(irreg, 
                                  Infl.Bacon.100[[paste(ID)]][,i], 
                                  roffset = 353.55))
# plot(irreg, col = "red")
# for(i in 20:30) lines(ensemb[[i]], col = "darkgrey")
# lines(irreg, col = "red", lw = 2)
# legend(x = 100, 
#        y= 0.28, 
#        col = c("red", "darkgrey"), 
#        legend = c("mean realization", "other realizations with added offset"),
#        lty = 1,
#        cex = 0.8)

ensemb <- lapply(1:length(ensemb), 
                 function(x) data.frame(Age_BP = zoo::index(ensemb[[x]]), 
                                        value = zoo::coredata(ensemb[[x]]),
                                        realization = x))%>%
  bind_rows()
head(ensemb)
