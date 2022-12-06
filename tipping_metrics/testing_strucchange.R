#test strucchange package
# 02.12.

rm(list = ls())
library(tidyverse)
library(popa)
library(diptest)
library(strucchange)

load("//dmawi.de/potsdam/data/bioing/user/lschild/surrogate/output/surrogates2.rda")
surrogates <- surrs

test <- surrogates[[1]] %>%
  filter(sig == "trend",
         Dataset_ID == 100019,
         realization == 3)

ggplot(test, aes(x = Age_BP, y = value, group = realization))+
  geom_line(alpha = 0.5)+
  geom_smooth(method = "lm")+
  labs(title = "Test data with 100 realizations")

dip.test(test$value[test$realization == 3])

start <- round(min(test$Age_BP))
end <- round(max(test$Age_BP))

#interpolate to make it work 
test_int <-approx(x = test$Age_BP,
                   y = test$value,
                   xout = seq(start, end, 100))

test_int <- ts(test_int$y,
               start = min(test_int$x),
               end = max(test_int$x),
               deltat = 100)

test_n <- zoo::zoo(x = test$value,
                  order.by = test$Age_BP)
#F tests
fs <- Fstats(test_n ~ 1)
plot(fs)
bp <- breakpoints(test_int ~ 1, breaks = 2)
plot(bp)

#cusum method (empirical fluctuation processes)
efp <- efp(test_n ~ 1,
           type = "OLS-CUSUM")
plot(efp)

#statistical testing
#is there a significant breakpoint?
sctest(efp)

plot(test_int)
lines(breakpoints(fs))
plot(Nile)

## test the null hypothesis that the annual flow remains constant
## over the years
fs.nile <- Fstats(Nile ~ 1)
plot(fs.nile)
sctest(fs.nile)
## visualize the breakpoint implied by the argmax of the F statistics
plot(Nile)
lines(breakpoints(fs.nile))
