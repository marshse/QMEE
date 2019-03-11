## Assignment_8_week_march_4

## Generalized Linear Models

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


## Examine for overdisperisan
summary(g1)
# Residual deviance/residual df is much less than 1
# Dispersion parameter for gaussian family taken to be 0.0318402


## Check diganostic plots
# Show 4 plots in one frame
par(mfrow=c(2,2))
plot(g1)
# data looks normally (gaussian) distributed and within Cook's distance of 0.5
# but with a slight skew to the right?


# Just because I couldn't figure out dotwhisker in an earlier assignment
# and it also gives me some info about my data
library(dotwhisker)
dwplot(g1)


## Check for autocorrelation
par(mfrow=c(1,1))
acf(residuals(g1))


## Check out quadratic model
g2 <- g1+geom_smooth(method="glm", formula=y~poly(x,2),
                     method.args=list(family="gaussian"))
# SMR: This is giving me a null set for g2  ?
g2+scale_y_log10()


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
# This script generated gg data frames but I could not plot the quadratic model

gg0 <- ggplot(fi_d1, aes(Date, body_condition))+geom_point()

gg1 <- gg0 + geom_smooth(method="glm", colour="red",
                         method.args=list(family=gaussian()))

gg2 <- gg1+geom_smooth(method="glm", formula = y~poly(x,2),
                       method.args=list(family="gaussian"))

gg2+scale_y_log10()

#################################



#################################
## THIS SECTION IS STILL IN PROGRESS
## Trying yet another approach

# Set up a data frame for predictions
pred_df <- fi_d1(Date = seq(from = 1, to = 5, length = 10))

# Generate predictions
pred_df$predicted <- predict(gaussian_glm, pred_df)

# Look at the data frame
pred_df

# Add model line to plot
ggplot(fi_d1) +
  geom_point(aes(x = Date, y = body_condition)) +
  geom_line(aes(x = Date, y = predicted), data = pred_df)

linear.model <-lm(body_condition ~ Date)


