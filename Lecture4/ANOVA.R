##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
## ANOVA
##
##--------------------------------------------

##-----ANOVA Example-----
df = data.frame('group'=c(rep(1,50),
                          rep(2,50),
                          rep(3,60),
                          rep(4,40)),
                'val' = c(rnorm(50, mean=0, sd=1),
                          rnorm(50, mean=0, sd=1),
                          rnorm(60, mean=0.5, sd=1),
                          rnorm(40, mean=0, sd=1)))
df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)

boxplot(df$val ~ df$group)
df_aov = aov(val ~ group, data = df)
summary(df_aov)

# we get statistics on the groups and total residuals:
# DF = degrees of freedom
# Sum Sq = sum of squares
# Mean Sq = Mean Squared Error
# F -value = our statistic based on a ratio of Mean Square Errors
# Pr(>F) = p-value for the NULL hypothesis that all groups are the same.

# BUT WHICH GROUP IS DIFFERENT? ANOVA does not tell us which.  We need 
#  to further test to find out.

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
# This test is built on the distribution of testing two means (say mean1 and mean2),
#   and the statistic is given by (mean1 - mean2)/sd(both samples)
#   Know that this statistic has a known (albeit uncommon) distribution and this allows us
#   (or R) to create multiple hypotheses (pairwise tests).
plot(tukey_anova)

##----CLT and Confidence Intervals----
x = c(rnorm(1000),rnorm(1000,mean=3,sd=0.5))
plot(density(x)) # Definitely not normal

# generate 100 samples
x_samples = lapply(1:500, function(i) sample(x, size=50, replace=TRUE))
x_means = lapply(x_samples, mean)
hist(unlist(x_means))
qqnorm(unlist(x_means)) # Yay normality!

pop_mean_estimate = mean(unlist(x_means))
pop_mean_estimate
pop_mean_sd = sd(unlist(x_means))
pop_mean_sd

actual_mean = mean(x)
actual_mean

# Create an alpha-level confidence interval
alpha = 0.95
half_width = qnorm((1+alpha)/2, mean=pop_mean_estimate, sd = pop_mean_sd) - pop_mean_estimate
half_width

ci_low = pop_mean_estimate - half_width
ci_high = pop_mean_estimate + half_width

print(paste('The actual mean is',round(actual_mean,3)))
print(paste('The',alpha,'level CI is (',round(ci_low,3),',',round(ci_high,3),').'))

##---Confidence interval of monty hall switching-----

resample <- function(x, ...) x[sample.int(length(x), ...)]

n=1000
switch_outcomes = sapply(1:n, function(x){
  prize = resample(1:3,1)           # select a prize
  non_prizes = setdiff(1:3,prize)   # set the non-prizes
  first_pick = resample(1:3,1)      # select a door
  revealed_non_prize = resample(setdiff(non_prizes,first_pick),1) # reveal a non-prize/non-selected door
  switch_pick = setdiff(1:3,c(first_pick,revealed_non_prize))     # select the other door
  return(switch_pick==prize)
})

sample_mean = mean(switch_outcomes)
sample_var = var(switch_outcomes)

alpha_level = 0.95
scaled_var = sample_var/sqrt(n)

# How many sd's away from the mean is our cutoff?
n_sds = qnorm((1+alpha_level)/2)
# or
n_sds = -qnorm((1-alpha_level)/2)

c_i_upper = sample_mean + n_sds*sqrt(scaled_var)
c_i_lower = sample_mean - n_sds*sqrt(scaled_var)

print('Population Mean Estaimate:')
print(paste(round(sample_mean,3),'with a',100*alpha_level,'% CI: (',
            round(c_i_lower,3),',',round(c_i_upper,3),').'))

# Try changing the alpha-level:
# A lower alpha level, say 0.9 (90% C.I.) will be smaller because
#   we are going to be LESS sure of where the population mean is.
#
# A higher alpha level, say 0.99 (99% C.I.) will be larger because
#   we are going to be MORE confident of where the populatio mean is.
