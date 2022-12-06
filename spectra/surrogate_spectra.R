#check surrogate data spectra

rm(list = ls())

library(PaleoSpec)
library(tidyverse)
source("~/Documents/pollen/bistab/spectra/spectrum_functions.R", echo=FALSE)

load("//dmawi.de/potsdam/data/bioing/user/lschild/surrogate/output/surrogates2.rda")
load("C:/Users/lschild/Documents/pollen/bistab/new_data/spectrum_IDs.rda")
surrogates <- surrs
rm(surrs)

#### functions ####
filter_surrs <- function(i){
  df <- surrogates[[i]] %>%
    filter(Dataset_ID %in% IDs,
           Age_BP <= 8000,
           realization %in% 1:5) %>%  #currently only using 3 realizations per site!!!!!!
    mutate(Dataset_ID_old = Dataset_ID,
           Dataset_ID = paste(Dataset_ID, realization, sep = '_'))%>%
    arrange(Dataset_ID, sig, Age_BP) %>%
    rename(tree = value)
  
  return(df)
}

results <- data.frame(surrogate = character(),
                      signal = character(),
                      slope = character())

for(i in 1:length(surrogates)){
  #filter df
  df_full <- filter_surrs(i)

  #get unique records (Dataset_ID + realization)
  records <- sample(unique(df_full$Dataset_ID),1000)
  
  #get mean resolution
  #calculate mean resolutions per record per realization
  ages <- df_full%>%
    filter(sig == "cons")
  
  res <- sapply(records,
                function(x) mean(diff(ages$Age_BP[ages$Dataset_ID == x]), 
                                 na.rm = TRUE))
  mean_res <- mean(res, na.rm = TRUE)
  
  
  #loop through different signals
  for(signal in c("trend","steps","cons")){
    df <- df_full %>%
      filter(sig == signal)
    
    #get spectra for all records 
    specs_tc <- lapply(records,
                       record_spec,
                       type = "tc")
    
    #get average spectrum
    avrg_tc <- plot_spectra(type = "tc")
    
    vars <- c(unlist(strsplit(names(surrogates)[i],"_")),signal)
    
    slope <- get_slope(type = "tc",
                       cutoff = 2*mean_res,
                       surrogate = vars)
    path <- "C:/Users/lschild/Documents/pollen/bistab/plots/surrogate_spectra/"
    file <- paste0(vars[1],"_",vars[2],"_",signal,".png")
    
    ggsave(filename = paste0(path,file),
           width = 6,
           height = 4)
    
    results[nrow(results)+1,] <- c(names(surrogates)[i], signal, paste(round(slope,3)))
    
  }
}

results

save(results, 
     file = "spectra/res_surrogate_spectra.rda")

ggplot(results, aes(y = as.numeric(slope), x = surrogate, fill = factor(signal,
                                                            levels = c("cons","trend","steps"))))+
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(fill  = "underlying signal",
       x = "surrogate data set (HX_noise)",
       y = "scaling exponent",
       title = "Scaling for different surrogate datasets")

ggsave("plots/surrogate_spectra/scaling_exponents.png",
       width = 8,
       height = 4)

# test <- df_full %>%
#   filter(Dataset_ID_old == 12)
# 
# ggplot(test, aes(x= Age_BP, y = tree, col = realization))+
#   geom_line()+
#   facet_grid(Dataset_ID_old ~ sig)
# 
# #plot surrogate data without filtering
# test <- surrogates[[1]]%>%
#   filter(Dataset_ID %in% unique(surrogates[[1]]$Dataset_ID)[1:3])
# 
# surrogates[[1]]%>%
#   filter(Dataset_ID == 12) %>%
# ggplot( aes(x= Age_BP, y = value, col = realization))+
#   geom_line()+
#   facet_grid(Dataset_ID ~ sig)
