## Assignment_8_week_march_4

## Generalized Linear Models
# Make a generalized linear model for one or more of your hypotheses.
# Discuss your results (NOTE: this is found throughout as comments)

library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(lmPerm)

fish_info_data <- read.csv("Genetics book Corse 2018.csv")

# Select what data I want, add body condition column and drop NA values. 
# Date format is ok for this dataset
fi_d1 <- fish_info_data %>% 
  filter((Type %in% c("NM"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt, Date) %>% 
  mutate(body_condition = 1e8 * (Wt/(10*SL)^3)) %>% 
  drop_na(
  )
summary(fi_d1)

## Hypothesis: 
# Body condition score of Nesting Males (NM) will decrease over
# the spawning season

## Apply a GLM

g1 <- glm(body_condition ~ Date, data = fi_d1, family = gaussian)

## JD: It is not really clear that this is a glm. 
## How does it differ from an lm (which is also family=gaussian)?
## Is there some reason to think some part of your system is not 
## Gaussian, or would benefit from a (non-identity) link function?

## Examine for overdisperisan
summary(g1)
# Residual deviance/residual df is much less than 1
# Dispersion parameter for gaussian family taken to be 0.0318402
# this indicates that there is underdispersian? 
# however dispersian is not relevant because family is gaussian?
## JD: the last one (Gaussian fits dispersion correctly)

## Check diagnostic plots
# Show 4 plots in one frame
par(mfrow=c(2,2))
plot(g1)
# data looks normally (gaussian) distributed and within Cook's distance of 0.5
# but with a slight skew to the right?
# error message: not plotting observatiosn with leverage one
# does this mean that on some dates there is only one point and R doesn't like it?

## JD: These are warnings, not errors,
## but you are absolutely right to always worry about warnings
## In this case, it's because of the structure of your model:
## you are treating Date as a categorical variable, so the model is
## unstable if you have only one observation for a given date.
## If you are looking for patterns across date, you probably want to 
## treat date as a number, and maybe use a spline response

# Just because I couldn't figure out dotwhisker in an earlier assignment
# and it also gives me some info about my data
library(dotwhisker)
dwplot(g1)
# I "think" this is showing me very large 95% Confidence Intervals for most dates
# I "think" this means I have a lot of variation during most of these dates
## JD: Right, because you are estimating each date separately.

## Check for autocorrelation
par(mfrow=c(1,1))
acf(residuals(g1))
# "A spike at lag 1 in an ACF plot indicates a strong correlation 
# between each series value and the preceding value" 
# https://www.ibm.com/support/knowledgecenter/en/SS3RA7_15.0.0/com.ibm.spss.modeler.help/timeseries_acf_pacf.htm

## JD: To do an ACF, we would want to:
#### _not_ fit dates separately
#### have one point in our residual series for each lag
###### (here a lot of your lag 1 points are really from the same time point, since you are just comparing each value to the next one on your list)
#### have equally spaced dates

#################################
# First attempt at generating a quadratic model

## Check out quadratic model
g2 <- g1+geom_smooth(method="glm", formula=y~poly(x,2),
                     method.args=list(family="gaussian"))
# SMR: This is giving me a null set for g2???
## JD: Because you don't have a numeric predictor, I guess
## But these are just attempts at drawing, not at fitting
## What you want is to make Date a numeric variable, and refit
## lm or glm (probably with spline instead of poly)

#################################
# Second attempt at generating a quadratic model

## Trying another approach to generate quadratic model
# Date is easier to read on the y axis :-)
ggplot(fi_d1, aes(body_condition,Date))+
  geom_point()+
  geom_smooth(method = "glm",
              formula = y~poly(x,2),
              method.args=list(family=gaussian(link = "identity")),
              fill="blue")

g2 <- g1+geom_smooth(method="glm", formula=y~poly(x,2),
                     method.args=list(family="gaussian"))
# g2 still has a null set :-(


#################################
# Third attempt at generating a quadratic model

# This script generated gg data frames but I could not plot the quadratic model

gg0 <- ggplot(fi_d1, aes(Date, body_condition))+geom_point()

gg1 <- gg0 + geom_smooth(method="glm", colour="red",
                         method.args=list(family=gaussian()))

gg2 <- gg1+geom_smooth(method="glm", formula = y~poly(x,2),
                       method.args=list(family="gaussian"))
gg2+scale_y_log10()

#################################
## THIS SECTION IS STILL IN PROGRESS

## Trying yet another approach at generating a quadratic model
#  I haven't yet been able to figure out how to make this code work to get anything?

# Set up a data frame for predictions
## JD There is something seriously wrong with the syntax of this line
## (it is calling fi_d1 as a function)
## Adding quit so it's easier to run what's above
quit()
pred_df <- fi_d1(Date = seq(from = 1, to = 3, length = 6))

# Generate predictions
pred_df$predicted <- predict(gaussian_glm, pred_df)

# Look at the data frame
pred_df

# Add model line to plot
ggplot(fi_d1) +
  geom_point(aes(x = Date, y = body_condition)) +
  geom_line(aes(x = Date, y = predicted), data = pred_df)

linear.model <-lm(body_condition ~ Date)

#################################

## JD: Grade 1.6/3. OK.
