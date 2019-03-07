# Assignment_7_week_25_Feb

## IF possible, please don't mark this yet.  I am working on making it "less" horrible 
## and trying to figure ot what I am doing here ..... thanks!! smr

## Install JAGS (and either rjags or R2jags). 
## Use jags to fit a Bayesian model to your data, in some way that at least roughly makes sense. 
## Discuss your prior assumptions, and compare your simple fit to an analogous frequentist fit.


library(tidyverse)
library(lmPerm)
library(ggplot2); theme_set(theme_bw())
library(R2jags)
library(readr)
library(effects): carData
library(dplyr)
library(coda)
library(broom.mixed)
library(arm)        ## for coefplot, bayesglm
#library(dotwhisker)
#library("emdbook")    ## for lump.mcmc.list() and as.mcmc.bugs()
#library("lattice")
#library("rstanarm")


## Fish morphometric data:

fish_info_data <- read_csv("Genetics book Corse 2018.csv")
# print(fish_info_data)


#  Analogous frequentist fit to compare to Bayesian model:

# Select what data I want, add body condition column and drop NA values. 
# Date format is ok for this dataset
fi_d2 <- fish_info_data %>% 
  filter((Type %in% c("NM","Sat","sn"))) %>% 
  #filter((Type %in% c("NM","Sat"))) %>%   
  droplevels() %>% 
  select(Type, SL, Wt, Date) %>% 
  mutate(body_condition = 1e8 * (Wt/(10*SL)^3)) %>% 
  drop_na(
  )

## Said by BMB: "I'm still curious what the multiplication by 1e8 is doing."
## SMR: body condition is usually 100*(Wt/(10*SL)^3)) and JD suggested that
## "Since condition is more or less on an arbitrary scale, 
## why not multiply by a million to get human-friendly numbers?"

# Examine my data 
print(ggplot(fi_d2, aes(x=Type, y=body_condition))
      + geom_point()
      + geom_smooth()
)

# Show 4 plots in one frame
par(mfrow=c(2,2)
)

# Fit a linear model
# Use LM instead of ANOVA as unequal sample size for each male Type
fi_d2.lm <- lm(body_condition~Type, fi_d2)
class(fi_d2.lm)
summary(fi_d2.lm)
plot(lm(fi_d2.lm), las=1, col = "green"
)

N <- nrow(fi_d2)

dwplot(fi_d2.lm)

## Not sure that I am setting up the linear model appropriately...
## My script keeps breaking (errors) I think bc of some of the items loaded in library 
## at the very beginning ? And then I go back and run it in pieces and I am okay...


## Use jags to fit a Bayesian model to your data, in some way that at least 
## roughly makes sense. 

# Model with main effect of Male Type
bayesmod <- with(fi_d2, jags.model(model.file='bayes.bug'
                          , parameters=c("b_body_condition", "b_0", "tau")
                          , data = list('Type' = Type, 'body_condition' = body_condition, 'N'=N)
                          , n.chains = 4
                          , inits=NULL
))

print(bayesmod)
plot(bayesmod)
traceplot(bayesmod)


# Another method for making a model with the main effect of Male "Type" ??
# parameterized by group means

body_condition_model1 <- function() {
  for (i in 1:N) {
    ## Poisson model
    logmean[i] <- b_body_condition[body_condition[i]]    ## predicted log(counts)
    pred[i] <- exp(logmean[i])       ## predicted counts
    Type[i] ~ Type(pred[i])
  }
  ## define priors in a loop
  for (i in 1:ntime) {
    b_Type[i] ~ dnorm(0,0.001)
  }
}

j1 <- jags(data=fi_d2,
           inits=NULL,
           parameters=c("b_body_condition"),
           model.file=body_condition_model1)

tidy(j1,conf.int=TRUE, conf.method="quantile")

plot(jags)

# I can't get any of this to run without errors...

## Discuss your prior assumptions, and compare your simple fit to an 
## analogous frequentist fit.

# My prior assumptions of body condition are based upon my existing dataset.

## compare with linear models
m1 <- lm(body_condition ~ Type, data=fi_d2, family=poisson)

# Can't seem to generate the simple fit? 
# I have installed R2jags (and it seems to be connecting) but am having difficulty 
# generating the Bayesian model to my data.

## Apologies that this assignment is obviously not very easy to follow on my end.
## Working through examples and trying to apply them to my dataset in 
## a coherent manner. Still working through this exercise!



