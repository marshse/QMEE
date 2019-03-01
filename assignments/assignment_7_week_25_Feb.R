## Install JAGS (and either rjags or R2jags). 
## Use jags to fit a Bayesian model to your data, in some way that at least roughly makes sense. 
## Discuss your prior assumptions, and compare your simple fit to an analogous frequentist fit.

# Print from Class Example:

library(tidyverse)
library(R2jags)
library(readr)
library(ggplot2); theme_set(theme_bw())

library("coda")
library("emdbook")    ## for lump.mcmc.list() and as.mcmc.bugs()
library("arm")        ## for coefplot, bayesglm
library("lattice")
library("rstanarm")
library("dotwhisker")
library("broom.mixed")

df <- read_csv("Genetics book Corse 2018.csv")

# Create a function that I can use to generate different subsets of the data
# and to calculate body condition
proc_fun <- function(data,types) {
  return(data
         %>%  filter(Type %in% types)
         %>% droplevels()
         %>% select(Type, SL, Wt, Date)
         %>% mutate(body_condition = 1e8 * (Wt/(10*SL)^3))
         %>% drop_na())
}
fi_d3 <- df %>% proc_fun(types=c("NM","Sat","sn"))  


print(ggplot(fi_d3, aes(x=Type, y=body_condition))
      + geom_point()
      + geom_smooth()
)

linmod <- lm(Type~body_condition, data=fi_d3)
plot(linmod)

N <- nrow(fi_d3)

## Still not sure that I am setting up the linear model appropriately...

### STOPPED HERE
bayesmod <- with(fi_d3, jags(model.file='fev.bug'
                          , parameters=c("b_height", "b_0", "tau")
                          , data = list('fev' = fev, 'height' = height, 'N'=N)
                          , n.chains = 4
                          , inits=NULL
))

print(bayesmod)
plot(bayesmod)
traceplot(bayesmod)
