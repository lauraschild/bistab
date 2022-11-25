library(PaleoSpec)
library(tidyverse)
record_spec <- function(ID,
                        type = c("temp","tc")){
  print(ID)
  if(type == "tc"){
    value <- df$tree[df$Dataset_ID == ID]
  }else if(type == "temp"){
    value <- df$MAT[df$Dataset_ID == ID]
  }
  
  age <- df$Age_BP[df$Dataset_ID == ID]
  
  #interpolate
  tc_int <- MakeEquidistant(t.x = age,
                            t.y = value,
                            dt = mean_res/6)
  tc_int2 <- zoo::zoo(tc_int)
  
  #make spectrum
  if(sum(tc_int, na.rm = TRUE) != 0 & sum(!is.na(tc_int) > 1)){
    spec <- spectrum(tc_int[!is.na(tc_int)])
    spec <- approx(spec$freq/(mean_res/6),
                   spec$spec,
                   seq(0.000125,0.5,0.000125))
  }else{
    spec <- data.frame(x = NA,
                       y  =NA)
  } 
  
  df <- data.frame(Dataset_ID = ID,
                   freq = spec$x,
                   spec = spec$y)
  return(df)
}

#function to plot a subset of spectra and calculate the avrg
plot_spectra <- function(type = c("temp","tc")){
  if(type == "temp"){
    specs <- specs_temp
    lab <- "Temperature"
    lim <- 50
  }else{
    specs <- specs_tc
    lab <- "Tree Cover"
    lim <- 200
  }
  
  plot(spec ~freq, 
       specs[[1]], 
       type = "l", 
       col = rgb(0,0,0,alpha = 0.1),
       ylim = c(0,lim),
       xlim = c(0, max(specs[[1]][,2][!is.na(specs[[1]][,3])])),
       xlab = "Frequency",
       ylab = "spectral density",
       main = paste("Spectra of", lab),
       sub = "incl samples <= 8000 BP")
  
  for(i in sample(2:length(specs), 50)){
    df <- specs[[i]]
    lines(df$freq,
          df$spec,
          col = rgb(0,0,0,alpha =0.1))
  }
  
  avrg <- specs %>%
    bind_rows() %>%
    #filter(freq > cutoff)%>%
    group_by(freq) %>%
    summarize(mean_spec = mean(spec, na.rm = TRUE))
  
  lines(avrg$freq,
        avrg$mean_spec,
        col = "red")
  legend(x = 0.004,
         y = 200,
         legend = c("record spectra", "mean spectrum"),
         col = c("grey", "red"),
         lty= 1,
         cex= 0.8)
  
  return(avrg)
}

get_slope <- function(type = c("temp", "tc"),
                      cutoff = 1){ #time scale to cutoff high frequencies
  if(type == "temp"){
    avrg <- avrg_temp%>%
      filter(freq < 1/cutoff)
    title <- "Temperature"
  }else{
    avrg <- avrg_tc %>%
      filter(freq < 1/cutoff)
    title <- "Tree Cover"
  }
}
  