#script to make surrogate data
#Laura Schild
rm(list = ls())

packages <- c("dplyr",
              "tidyr",
              "RScaling",
              "popa")
lapply(packages, library, character.only = TRUE)
noise <- 0.1

#get time steps for all records
ts <- P %>%
  group_by(Dataset_ID, Age_BP) %>%
  mutate(dupl = n()) %>%
  filter(dupl == 1) %>%
  select(Dataset_ID, Age_BP)

makepaleo <- function(ID,
                      noise){
  #make irregular paleoseries with pollen time steps
  timesteps <- round(ts$Age_BP[ts$Dataset_ID == ID])
  print(ID)
  if(1/mean(diff(timesteps)) < 0.5){
    paleo <- RScaling::Paleoseries(dts = timesteps,
                                   H = 0.1,
                                   seed = 1111,
                                   blockwidth = 0.1, # By having a small blockwidth we are just sub-sampling
                                   tau = mean(diff(timesteps))) # We give a timescale for the lowpass filter
    
    #constant signal (just noise)
    cons <- paleo$Irregular
    
    sd_noise <- sd(cons)
    sd_trend <- sd_noise/noise
    range <- sd_trend/0.57735 - 0.0043
    
    #set trend so that it has the same range as the noise
    #increase trend range with factor m
    slope <- (range + range)/(max(timesteps)-min(timesteps))
    intcp <- range - slope*max(timesteps)
    trend <- (timesteps*slope + intcp)+ cons
    trend <- zoo::zoo(trend, timesteps)
    
    #add step with same range as noise
    #increase range with factor m
    step1 <- rep(-sd_trend, sum(timesteps < max(timesteps)/2))
    step2 <- rep(sd_trend, sum(timesteps > max(timesteps)/2))
    #check if a timestep is the exact middle by chance
    if(sum(timesteps == max(timesteps)/2)>0){
      steps <- c(step1, min(cons),step2) + cons
    }else{
      steps <- c(step1,step2) + cons
    }
    steps <- zoo::zoo(steps, timesteps)
    
    # plot(trend, col = "blue",
    #      ylim = c(min(cons,trend,steps),max(cons,trend,steps)),
    #      xlab = "Age_BP",
    #      ylab = "value",
    #      main = "Surrogate data before normalization")
    # lines(steps, col = "red")
    # lines(cons)
    # legend(1000, 10,
    #        legend=c("Noise","Trend","Step"),
    #        col=c("black","blue","red"),
    #        lty=1,
    #        cex=0.8)
    
    #rescale all three timeseries to be between 0 and 1
    normalize <- function(x){
      x <- (x -min(trend,cons,steps))/(max(trend,cons,steps)-min(trend,cons,steps))
      return(as.vector(x))
    }
    
    steps1 <- normalize(steps)
    trend1 <- normalize(trend)
    cons1 <- normalize(cons)
    
    # plot(trend1,
    #      col = "blue",
    #      type = "l",
    #      ylim = c(min(cons1,steps1, trend1), max(cons1, trend1, steps1)),
    #      xlab = "Age_BP",
    #      ylab = "Value",
    #      main = "Surrogate data after normalization")
    # lines(steps1, col = "red")
    # lines(cons1)
    # legend(1000, 1,
    #        legend=c("Noise","Trend","Step"),
    #        col=c("black","blue","red"),
    #        lty=1,
    #        cex=0.8)
    
    return(data.frame(Dataset_ID = ID,
                      Age_BC = timesteps,
                      cons = cons1,
                      trend = trend1,
                      step = steps1))
  }else print(paste(ID,"not working"))
  
}



surrogates <- lapply(unique(ts$Dataset_ID)[1131:length(unique(ts$Dataset_ID))],
       makepaleo,
       noise = noise)
# 
# cl <- parallel::makeCluster(cores,
#                             type = "SOCK")
# doSNOW::registerDoSNOW(cl)
# parallel::clusterExport(cl,
#                         c("makepaleo",
#                                  "ts",
#                           "sites"))
# surrogates <- parallel::clusterApplyLB(cl,
#                                        sites,
#                                        makepaleo,
#                                        noise = 0.1)
# parallel::stopCluster(cl)
# surrogates
res <- surrogates %>% bind_rows()

write.csv(res,
          "C:/Users/lschild/Documents/pollen/bistab/new_data/surrogate_01.csv",
          row.names = FALSE)
