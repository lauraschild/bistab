#threshold detection
#function

#make ts for testing
test <- rep(c(1,3), each = 50) + runif(100, 0,1)
test2 <- 1:100 + runif(100,0,10)
test3 <- runif(100,0,1)

t_detect <- function(x,     #vector with values
                     y,     #index(age) for values
                     buf){  #buffer of fraction of values at end and beginning to exclude from detection
  #go through all time points and calculate diff between post and ante mean
  
  #exclude some points at beginning and end
  start <- length(x)*buf +1
  end <- length(x) - length(x)*buf
  #remove trend of x beforehand
  
  diffs <- sapply(start:end,
                  function(i) abs(mean(head(x,i) - mean(tail(x, length(x)-i)))))
  max_diff <- max(diffs, na.rm = TRUE)
  
  timestep <- y[start:end][which(diffs == max_diff)]
  
  #get random diffs for comparison
  r_diff <- function(i){
    #use sd from original timeseries
    noise <- RScaling::FractionalNoise(length(x),
                                       H=0,
                                       mu  =0,
                                       sigma = sd(residuals(lm(x~y))))
    
    #use sd from residuals for original timeseries?
    # noise <- RScaling::FractionalNoise(length(x),
    #                                    H = 0,
    #                                     mu = 0,
    #                                    sigma = sd(x))

    max_diff <- max(sapply(start:end,
                       function(i) abs(mean(head(noise,i) - mean(tail(noise, length(noise)-i))))),
                    na.rm = TRUE)
    return(max_diff)
  }
  
  r_max_diff <- quantile(sapply(1:500,
                                r_diff),
                         0.95)
  
  if(max_diff <= r_max_diff){
    message("No significant breakpoint detected.")
    plot(y,x, type = "l")
  }
  if(max_diff > r_max_diff){
    message(paste0("Siginificant breakpoint detected at ", timestep, "."))
    mean1 <- mean(head(x, which(diffs == max_diff)))
    mean2 <- mean(tail(x, length(x)-which(diffs == max_diff)))
    plot(y,x, type = "l")
    abline(v = timestep,
           lty = 3,
           col = "red",
           lwd = 2)
    return(data.frame(shift_at = timestep,
                      mean_state1 = mean1,
                      mean_state2 = mean2))
  }
  
}

t_detect(test,1:100, 0.05)
t_detect(test2,1:100, 0.1)
t_detect(test3, 1:100, 0.05)
