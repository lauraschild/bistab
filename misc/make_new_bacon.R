#create new age model subset data file
#Laura Schild
rm(list = ls())
library(tidyr)
library(dplyr)

path <- "//dmawi.de/potsdam/data/bioing/data/GlobalPollenDataset/To_PANGAEA/Reconstruction_Dataset/From_Raphael/Bacon1000v4"
files <- list.files(path)[1:4]
bacons <- lapply(files,
                 function(x) data.frame(data.table::fread(paste0(path,
                                                                 "/",
                                                                 x)))) %>%
  bind_rows()

bacon <- bacons %>%
  select(`Age..ka.BP...mean.`,
         `ID..Dataset.`,
         starts_with("R"),
         -starts_with("Reference"))%>%
  rename(Age_BP = `Age..ka.BP...mean.`,
         Dataset_ID = `ID..Dataset.`)%>%
  arrange(Dataset_ID, Age_BP)
names(bacon)[1:50]

get_matrix <- function(ID){
  df <- bacon %>%
    filter(Dataset_ID == ID)
  mat <- as.matrix(df[,sample(3:1002,100)])*1000
  colnames(mat) <- 1:100
  return(mat)
}

get_matrix(7)
bacon_mat <- lapply(unique(bacon$Dataset_ID),
                    get_matrix)
names(bacon_mat) <- unique(bacon$Dataset_ID)

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

Infl.Bacon.100 <- lapply(1:length(bacon_mat),
                         function(x) InflateErrors(bacon_mat[[x]],
                                                   fac = 1.43139,
                                                   na.res = NA))
names(Infl.Bacon.100) <- names(bacon_mat)
Bacon.100 <- bacon_mat
save(Infl.Bacon.100, Bacon.100,
     file= "C:/Users/lschild/Documents/pollen/popa/data/Bacons.rda")
