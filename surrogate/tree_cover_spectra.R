#tree cover spectral analysis
#Laura Schild
rm(list = ls())
packages <- c("popa",
              "dplyr",
              "tidyr",
              "PaleoSpec")
lapply(packages, require, character.only = TRUE)

#filter PFT dataframe
PFT <- R_PFTopen %>%
  group_by(Dataset_ID) %>%
  mutate(max = max(Age_BP),
         min = min(Age_BP)) %>%
  group_by(Dataset_ID, Age_BP) %>%
  mutate(dupl = n()) %>%
  filter(dupl == 1,
         min <= 1500,
         max >= 8000,
         Age_BP <= 8000)%>%
  group_by(Dataset_ID) %>%
  mutate(samples = n(),
         res = max(diff(Age_BP))) %>%
  filter(samples >= 10,
         res <= 2500)

#function to get spectrum per ID
record_spec <- function(ID){
  print(ID)
  tc <- PFT$tree[PFT$Dataset_ID == ID]
  age <- PFT$Age_BP[PFT$Dataset_ID == ID]
  
  #interpolate
  tc_int <- MakeEquidistant(t.x = age,
                            t.y = tc,
                            dt = 100)
  tc_int2 <- zoo::zoo(tc_int)
  
  #make spectrum
  if(sum(tc_int, na.rm = TRUE) != 0){
    spec <- spectrum(tc_int[!is.na(tc_int)])
    spec <- approx(spec$freq/100,
                   spec$spec,
                   seq(0.000125,0.005,0.000125))
  }else{
    spec <- data.frame(x = NA,
                       y  =NA)
  } 

  df <- data.frame(Dataset_ID = ID,
                   freq = spec$x,
                   spec = spec$y)
  return(df)
}
specs <- lapply(unique(PFT$Dataset_ID),
                record_spec)


max_res <- max(PFT$res)

cutoff <- 1/(2*max_res)

plot(spec ~freq, 
     specs[[1]], 
     type = "l", 
     col = rgb(0,0,0,alpha = 0.1),
     ylim = c(0,1000),
     xlab = "Frequency",
     ylab = "spectral density",
     main = "Spectra of tree cover",
     sub = "incl samples <= 8000 BP")

for(i in sample(2:length(specs), 500)){
  df <- specs[[i]]
  lines(df$freq,
        df$spec,
        col = rgb(0,0,0,alpha =0.1))
}
abline(v = cutoff, col = "blue")
avrg <- specs %>%
  bind_rows() %>%
  #filter(freq > cutoff)%>%
  group_by(freq) %>%
  summarize(mean_spec = mean(spec, na.rm = TRUE))

lines(avrg$freq,
      avrg$mean_spec,
      col = "red")
legend(x = 0.004,
       y = 600,
       legend = c("record spectra", "mean spectrum"),
       col = c("grey", "red"),
       lty= 1,
       cex= 0.8)
plot(log(avrg$freq),
     log(avrg$mean_spec),
     type ="l")

ggplot(avrg, aes(x= freq, y = mean_spec))+
  geom_line(aes(col = "Mean spectrum"),
            size = 1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method = "lm", 
              aes(col = "Linear Model")
              )+
  labs(x = "Frequency",
       y = "PSD",
       title = "Power spectral density for Tree Cover")+
  scale_colour_manual(name="legend", values=c("blue", "red"))+
  geom_label(mapping = aes(x = 0.2,
             y = 1000),
             label = "Slope = -2.059")

ggplot(avrg, aes(x = log(freq), y = log(mean_spec)))+
  geom_line()+
  geom_smooth(method = "lm")

lm <- lm(log(avrg$mean_spec)~log(avrg$freq))
lm

spec <- list(spec = avrg$mean_spec[-nrow(avrg)],
             freq = avrg$freq[-nrow(avrg)])
LPlot(spec)
LLines(LogSmooth(spec,df.log=0.01),lwd=2,col='green')
LLines(LogSmooth(spec,df.log=0.05),lwd=2,col='blue')
LLines(LogSmooth(spec,df.log=0.1),lwd=2,col='red')
