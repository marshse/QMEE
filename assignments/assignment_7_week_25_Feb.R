## Install JAGS (and either rjags or R2jags). 
## Use jags to fit a Bayesian model to your data, in some way that at least roughly makes sense. 
## Discuss your prior assumptions, and compare your simple fit to an analogous frequentist fit.

# Print from Class Example:

library(R2jags)
library(readr)
library(ggplot2) 

### STOPPED HERE
df <- read_csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/FEV.csv")

print(ggplot(df, aes(x=height, y=fev))
      + geom_point()
      + geom_smooth()
)

linmod <- lm(fev~height, data=df)
plot(linmod)

N <- nrow(df)

bayesmod <- with(df, jags(model.file='fev.bug'
                          , parameters=c("b_height", "b_0", "tau")
                          , data = list('fev' = fev, 'height' = height, 'N'=N)
                          , n.chains = 4
                          , inits=NULL
))

print(bayesmod)
plot(bayesmod)
traceplot(bayesmod)
