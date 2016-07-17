# You can imagine that there is a inherent variation in the population,
#  this is the standard deviation we have found above.

# But also, there is some variation in finding the mean of the outcomes.
#  The more coin flips, the closer the mean will be to 0.5... see the plot of
#  the running average above.  This plot scales with 1/sqrt(n).


##-------------------------------------------------------------
## 
## Bootstrapping of confidence intervals.
##
##---------------------------------------------------------------
library(HistData)
library(resample)

male = GaltonFamilies[GaltonFamilies$gender == 'male',]
female = GaltonFamilies[GaltonFamilies$gender == 'female',]

plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 80, p = 0.05){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = 'red')
  abline(v = quantile(b, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(b, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
  par(mfrow = c(1, 1))
}

plot.diff <- function(a, cols = 'difference of means', nbins = 80, p = 0.05){
  maxs = max(a)
  mins = min(a)
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

## Bootstrap the mean of the sons and of daughters
mean.boot.male = one.boot(male$childHeight, mean, R = 100000)
mean.boot.female = one.boot(female$childHeight, mean, R = 100000)
plot.t(mean.boot.male$t, mean.boot.female$t, nbins = 80)

## Bootstrap the mean of the father of sons
mean.boot.father = one.boot(male$father, mean, R = 100000)
plot.t(mean.boot.male$t, mean.boot.father$t, nbins = 80)

## Bootstrap the difference in means of sons and daughters
two.boot.mean = two.boot(male$childHeight, female$childHeight, mean, R = 100000)
plot.diff(two.boot.mean$t)

## Is the bootstrapped distribution Normal?
qqnorm(two.boot.mean$t, main = 'Quantiles of standard Normal vs. bookstrapped mean')
 
## Bootstrap the difference in means of sons and fathers
two.boot.mean = two.boot(male$childHeight, male$father, mean, R = 100000)
plot.diff(two.boot.mean$t)

## Is the bootstrapped distribution Normal?
qqnorm(two.boot.mean$t, main = 'Quantiles of standard Normal vs. bookstrapped mean')


## -------- Try the Jackknife ------------------
##
## Jackknife the mean of the sons and of daughters
mean.jack.male = jackknife(male$childHeight, mean)
mean.jack.male$stats
mean.jack.female = jackknife(female$childHeight, mean)
mean.jack.female$stats
plot.t(mean.jack.male$replicates, mean.jack.female$replicates, nbins = 80)



