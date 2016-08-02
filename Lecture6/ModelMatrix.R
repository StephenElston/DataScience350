##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R Part 2 ----
##
##----- Model matrix with categorical variables ---
##

unique(auto.price$drive.wheels) # How many values?

## First attempt to build a simple model matrix
mod.mat = model.matrix(lnprice ~ drive.wheels, data = auto.price)
head(mod.mat)  # Note the use of contrast to intercept

## Or, code without the intercept
mod.mat = model.matrix(lnprice ~ 0 + drive.wheels, data = auto.price)
head(mod.mat)

mod.mat = model.matrix(lnprice ~ 0 + fuel.type +drive.wheels + aspiration + engine.size, data = auto.price)
head(mod.mat)

