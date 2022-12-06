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
  
  age <- round(df$Age_BP[df$Dataset_ID == ID])
  
  #interpolate
  if(length(value) > 1){
    tc_int <- MakeEquidistant(t.x = age,
                              t.y = value,
                              dt = mean_res/6)
    tc_int2 <- zoo::zoo(tc_int)
    
    #make spectrum
    if(sum(tc_int, na.rm = TRUE) != 0 & sum(!is.na(tc_int) > 1)){
      spec <- spectrum(tc_int[!is.na(tc_int)])
      spec <- approx(spec$freq/(mean_res/6),
                     spec$spec,
                     seq(0.000125,0.5,0.0000125))
    }else{
      spec <- data.frame(x = NA,
                         y  =NA)
    } 
    
    specs <- data.frame(Dataset_ID = ID,
                        freq = spec$x,
                        spec = spec$y)
    return(specs)
  }
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
    lim <- 10
  }
  
  plot(spec ~freq, 
       specs[[1]], 
       type = "l", 
       col = rgb(0,0,0,alpha = 0.1),
       # ylim = c(0,lim),
       xlim = c(0, 2/mean_res),
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
  legend(x = 1/mean_res,
         y = 0.5*max(avrg$mean_spec, na.rm = TRUE),
         legend = c("record spectra", "mean spectrum"),
         col = c("grey", "red"),
         lty= 1,
         cex= 0.8)
  
  return(avrg)
}

get_slope <- function(type = c("temp", "tc"), #type of data to use 
                      cutoff = 1,             #time scale to cutoff high frequencies
                      surrogate = NULL){      #surrogate vars as vector c(H,noise,signal)
  if(type == "temp"){
    avrg <- avrg_temp%>%
      filter(freq < 1/cutoff)
    title <- "Temperature"
  }else{
    avrg <- avrg_tc %>%
      filter(freq < 1/cutoff)
    title <- "Tree Cover"
  }
  
  lm <- lm(log(avrg$mean_spec)~log(avrg$freq))
  slope <- -(as.numeric(lm$coefficients[2]))
  
  p <- ggplot(avrg, aes(x = freq, y = mean_spec))+
    geom_line()+
    scale_y_log10()+
    scale_x_log10()+
    geom_smooth(method = "lm")+
    labs(title = paste("Mean power spectrum for", title),
         subtitle = ifelse(is.null(surrogate),
                           "",
                           paste0("H = ", surrogate[1],
                                  "; noise = ", surrogate[2],
                                  "; ", surrogate[3])) ,
         x = "Frequency",
         y  = "PSD")+
    annotate(geom = "label",
             x = mean(avrg$freq[!is.na(avrg$mean_spec)]),
             y = max(avrg$mean_spec, na.rm = TRUE),
             label = paste0("slope = ", round(slope,2), "\n",
                            "cutoff = 1/", round(cutoff)))
  
  print(p)
  
  return(slope)
}


  