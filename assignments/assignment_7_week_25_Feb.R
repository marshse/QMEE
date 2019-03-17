# Assignment_7_week_25_Feb

# March_9
# Thanks for holding off marking this assignment. It is still not "great" but have 
# gotten a little further with some more research.

### Feb_ 25  IGNORE THIS COMMENT (LINE 7-9)
## BMB: best to delete stuff you don't need any more ... keep the file clean.
## IF possible, please don't mark this yet.  I am working on making it "less" horrible 
## and trying to figure out what I am doing here ..... thanks!! smr


## Install JAGS (and either rjags or R2jags). 
## Use jags to fit a Bayesian model to your data, in some way that at least roughly makes sense. 
## Discuss your prior assumptions, and compare your simple fit to an analogous ffrequentist fit.

## BMB: do you need all of these packages? try to restrict to only necessary
## ones (e.g. lmPerm?)
library(tidyverse)
library(lmPerm)
library(ggplot2); theme_set(theme_bw())
library(R2jags)
library(readr)
## BMB: this wouldn't work as written ('object "carData" not found')
library(effects)
library(carData)
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
  ## BMB: if MASS package gets loaded after dplyr, the version of select()
  ##  in that pkg masks the dplyr one ...  
  dplyr::select(Type, SL, Wt, Date) %>% 
  mutate(body_condition = 1e8 * (Wt/(10*SL)^3)) %>% 
  drop_na(
  )

## Said by BMB: "I'm still curious what the multiplication by 1e8 is doing."
## SMR: body condition is usually 100*(Wt/(10*SL)^3)) and JD suggested that
## "Since condition is more or less on an arbitrary scale, 
## why not multiply by a million to get human-friendly numbers?"

## BMB: OK, that's fine ... thanks.

## Next step: You should

# Examine my data 
print(ggplot(fi_d2, aes(x=Type, y=body_condition))
      + geom_point()
      + geom_smooth()
      )

## BMB: maybe boxplots would be better for this?

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
## BMB: could be the MASS/dplyr select thing I mentioned above


## PLEASE SKIP AHEAD TO LINE 146 FOR THIS ASSIGNMENT
## BMB: If you want me to skip it, please leave it out of the file ...

## Use jags to fit a Bayesian model to your data, in some way that at least 
## roughly makes sense. 

## BMB: the only .bug file I can find in your repository is fev.bug --
## that means it's hard for me to debug since I don't have the model files

# Model with main effect of Male Type
bayesmod <- with(fi_d2, jags.model(model.file='bayes.bug'
                                 , parameters=c("b_body_condition", "b_0", "tau"
                                   ## BMB: have to pass factors (e.g. Type) as numeric variables,
                                   ## e.g. as.numeric(Type)
                          , data = list('Type' = Type, 'body_condition' = body_condition, 'N'=N)
                          , n.chains = 4
                          , inits=NULL
))

print(bayesmod)
plot(bayesmod)
traceplot(bayesmod)


# Another method for making a model with the main effect of Male "Type" ??
# parameterized by group means

## BMB: doesn't make sense to model body condition as a Poisson variable --
##  it's not a count ...
body_condition_model1 <- function() {
  for (i in 1:N) {
      ## Poisson model
      ## BMB: you're treating body_condition as categorical here, but it's
      ##  a continuous variable ???
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

body_condition_model2 <- function() {
  for (i in 1:N) {
      ## Poisson model
      ## BMB: you're treating body_condition as categorical here, but it's
      ##  a continuous variable ???
    logmean[i] <- b_body_condition[Type[i]]    ## predicted log(counts)
      pred[i] <- exp(logmean[i])       ## predicted counts
      ## BMB: Type is an *observed covariate*, it doesn't need to be modeled in this way (and Type() is not a JAGS/BUGS function ...)
      
    Type[i] ~ Type(pred[i])
  }
  ## define priors in a loop
  for (i in 1:ntime) {
    b_Type[i] ~ dnorm(0,0.001)
  }
}

## BMB: I got "unknown variable ntime"
## would need to include ntime in data (which should really be nType)
tidy(j1,conf.int=TRUE, conf.method="quantile")

plot(jags)

# I can't get any of this to run without errors...

## Discuss your prior assumptions, and compare your simple fit to an 
## analogous frequentist fit.

# My prior assumptions of body condition are based upon my existing dataset.

## compare with linear models
m1 <- lm(body_condition ~ Type, data=fi_d2, family=poisson)

## BMB: lm ignores 'family' (warning message), fits a regular linear model

# Can't seem to generate the simple fit? 
# I have installed R2jags (and it seems to be connecting) but am having difficulty 
# generating the Bayesian model to my data.

## Apologies that this assignment is obviously not very easy to follow on my end.
## Working through examples and trying to apply them to my dataset in 
## a coherent manner. Still working through this exercise!


###  MARCH 9, 2019

## I have looked at on-line examples, Kruschke's book and examples, read through 
## Gotelli and Ellis and the class examples and am still having difficulty
## translating into a coherent R script for this assignment.
## I've outlined the steps below and how I'd like to solve this ultimately.
## Will continue to try to work this out!

# BAYESIAN ANALYSIS (Gotelli and Ellison 2013)

## 1. Specify hypothesis.
# Mean body condition score between male Types (Fish species, S. ocellatus) 
# will be higher for NM versus Sat 

## 2. Specify parameters as random variables.
# body_condition will be considered as a random variable

## 3. Specify the prior probability distribution.
# Little known about this system, use an uninformative prior
# b_body_condition ~ dnorm(0, 0.001)

## 4. Calculate the likelihood.
# distribution that is proportional to the probability that 
## mean body condition is different between male types

## BMB: not quite.  It is the probability (density) of observing a
## particular set of body conditions, given a model and parameters
## BMB: you don't need to calculate the likelihood yourself, JAGS
##  does it for you ...

## 5. Calculate the posterior probability distribution.
## = (Prior * likelihood) / normalizing constant
##  BMB: JAGS does this for you by sampling

## 6. Interpret the results.
# examine means and standard deviations in lm vs Bayesian model
# compare credibility interval (Bayesian) with confidence interval (linear model) 

## BMB: the hypothesis is reasonable.
## score: 2
