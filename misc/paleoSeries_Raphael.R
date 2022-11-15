# Generate irregular timesteps according to a gamma distribution with skewness of 1
timesteps<-RScaling::GenerateTime(dt=200,
                                  tmin=0,
                                  tmax = 8000,
                                  skew = 1.)

# Generate a Fractional noise with H=0.1 (i.e. beta=1.2) and does a block average according to the irregular timesteps above
e.g.PS.BA<-RScaling::Paleoseries(dts = timesteps,
                                 H = 0.1, # Exponent for the fractional noise
                                 seed = 1111) # seed for the random series to make result reproducible (can be removed when doing ensembles)
plot(e.g.PS.BA$Irregular)
plot(e.g.PS.BA$Annual)
plot(e.g.PS.BA$Filtered)
plot(e.g.PS.BA$Regular)

# Same as above, bur rather than block average, the series is lowpass filtered and sub-sampled
e.g.PS.Ft<-RScaling::Paleoseries(dts = timesteps,
                                 H = 0.1,
                                 seed = 1111,
                                 blockwidth = 0.1, # By having a small blockwidth we are just sub-sampling
                                 tau = mean(diff(timesteps))) # We give a timescale for the lowpass filter
lines(e.g.PS.Ft$Irregular)

plot(e.g.PS.BA$Filtered)
lines(e.g.PS.Ft$Filtered,col='red',lwd=3)


whitenoise<-rnorm(1000)
plot(whitenoise)
plot(cumsum(whitenoise))

set.seed(123)
plot(1 / (1 + exp(-rnorm(1000))), type = "l")
plot(rnorm(1000), type = "l")
