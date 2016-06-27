##--------------------------------------------
##
## Counting/Probability R code (lecture 2)
##
## Class: PCE Data Science Methods Class
##
## Contains examples of:
##
## -Counting, Probability
##
## -More on Distributions
##
##--------------------------------------------

library(MASS) # Contains the function 'fractions()', 

##-----Sandwich Count----
breads = c('white', 'wheat', 'italian', 'sevengrain')
meats = c('ham', 'turkey', 'chicken', 'pastrami', 'meatballs')
toppings = c('mustard', 'mayo', 'salt_pepper', 'oil_vinegar')

sandwiches = expand.grid(breads,
                         meats,
                         toppings)
nrow(sandwiches)
sandwiches


##-----Two Dice Example ------
twoDice = expand.grid(1:6,1:6)
twoDice

twoDice$sum = twoDice$Var1 + twoDice$Var2
twoDice$isdouble = twoDice$Var1 == twoDice$Var2 ## == is logical equals
twoDice

# Count different sums
sumCounts = table(twoDice$sum)
sumCounts

# Count doubles
doubles = sum(twoDice$isdouble)
doubles

# Probability of a double:
fractions(doubles/nrow(twoDice))

# Probabilities of sums:
sumProb = fractions(table(twoDice$sum)/nrow(twoDice)) # type ?fractions for detail
barplot(sumProb)



system.time(sapply(1:1000, function(x){
  five_cards = sample(1:nrow(deck),5)
  return(length(unique(deck$suits[five_cards]))==1)
}))
# 0.08 on my system for 1000, so 1 million would take ~ 1000*0.08 = 80 seconds,
#    but 100K would take 8 seconds.

# For better system times, use the package 'microbenchmark':
# Careful! this essentially does system.time(rep(1000, f() ))
library(microbenchmark)
microbenchmark(sapply(1:1, function(x){
  five_cards = sample(1:nrow(deck),5)
  return(length(unique(deck$suits[five_cards]))==1)
}))

