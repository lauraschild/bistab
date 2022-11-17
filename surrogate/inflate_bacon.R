#prep inflated bacon models with random offset to be used for surrogate data
#Laura Schild
rm(list = ls())

library(popa)

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

Infl.Bacon.100 <- lapply(1:length(Bacon.100),
                         function(x) InflateErrors(Bacon.100[[x]],
                                                   fac = 1.43139,
                                                   na.res = NA))
save(Infl.Bacon.100,
     file= "C:/Users/lschild/Documents/pollen/popa/data/InflBacon.rda")
