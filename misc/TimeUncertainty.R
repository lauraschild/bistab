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

load(paste("//dmawi.de/potsdam/data/bioing/data/boreal_stability_study/rphl/","Bacon100.rda",sep=""))

plot(rowMeans(Bacon.100[[1]])) # We start a plot with the mean realization
for(i in 1:100) lines(Bacon.100[[1]][,i],lwd=0.5) # We add the individual realizations
lines(rowMeans(Bacon.100[[1]]),col='red',lwd=3) # We add back the mean one for clarity
points(rowMeans(Bacon.100[[1]]),col='red',lwd=3) # Also with points to see the timesteps
# Now we inflate the errors of the Bacon age models by increasing the spread around the mean by a factor of 1.43139
Infl.Bacon.1<-InflateErrors(Bacon.100[[1]],1.43139,
              na.res=rowMeans(Bacon.100[[1]])) 
for(i in 1:100) lines(Infl.Bacon.1[,i],lwd=0.5,col='orange') # If we replot, we see a wider spread
# For the same of the example, we increase the spread of the age realizations by a factor fo 5
# Infl.Bacon.1<-InflateErrors(Bacon.100[[1]],5,
#                             na.res=rowMeans(Bacon.100[[1]]))
for(i in 1:100) lines(Infl.Bacon.1[,i],lwd=0.5,col='skyblue') # Even wider
# We calculate the mean timesteps for the first record
Timesteps.Bacon.1<-rowMeans(Bacon.100[[1]],na.rm=TRUE)
# We make a fractional noise with the mean timesteps
FN.Bacon.1<-RScaling::Paleoseries(dts = Timesteps.Bacon.1,
                      H = 0.1, # H could be estimated from the real timeseries using RScaling::Fit(RScaling::Spectrum(timeseries))$H
                      seed = 1111, # Just to make a reproducible result for the sake of the example, but can be set to NULL (Default) to have something random
                      blockwidth = 0.1, # By having a small blockwidth we are just sub-sampling
                      tau = mean(diff(Timesteps.Bacon.1))) # We give a timescale for the lowpass filter

Ensemble.Timeuncertainty<-lapply(1:100,function(i) 
  ChangeTime(FN.Bacon.1$Irregular, # Paleoseries
             Infl.Bacon.1[,i], # New timesteps with inflated errors
             roffset = 353.55 # Standard deviation of the offset drawn from normal distribution with mean of 500 years
             )
)

plot(FN.Bacon.1$Irregular)  # Original series
for(i in 6:10) lines(Ensemble.Timeuncertainty[[i]],col='red',lwd=.5) # Let's plot a few example to get an idea
