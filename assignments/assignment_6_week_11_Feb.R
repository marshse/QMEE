### IN PROGRESS

# Assignment_6_week_11_Feb

# 1. Make a linear model for one or more of your hypotheses. 
# 2. Draw and discuss at least one of each of the following:
#   2.1. diagnostic plot
#   2.2  inferential plot (e.g., a coefficient plot, or something from emmeans or effects)


library(tidyverse)
library(ggplot2); theme_set(theme_light())
library(lmPerm)
library(emmeans)
library(effects)

# 1. Make a linear model

# HYPOTHESIS
#  Mean Body Condition will differ between Male Types (NM, Sat, sn) based on assumptions
#  of fitness investments.
#  Using data from 2018 field season with larger sample sizes
#  High Body Condition:  Nm > Sat > sn   :Low Body Condition


fish_info_data <- read_csv("Genetics book Corse 2018.csv")
# print(fish_info_data)


# Create a function that I can use to generate different subsets of the data
proc_fun <- function(data,types) {
  return(data
         %>%  filter(Type %in% types)
         %>% droplevels()
         %>% select(Type, SL, Wt, Date)
         %>% mutate(body_condition = 1e8 * (Wt/(10*SL)^3))
         %>% drop_na())
  }

## Said by BMB: "I'm still curious what the multiplication by 1e8 is doing."
## SMR: body condition is usually 100*(Wt/(10*SL)^3)) and JD suggested that
## "Since condition is more or less on an arbitrary scale, 
## why not multiply by a million to get human-friendly numbers?"


fi_d2 <- fish_info_data %>% proc_fun(types=c("NM","Sat"))    
fi_d3 <- fish_info_data %>% proc_fun(types=c("NM","Sat","sn"))  

# Show 4 plots in one frame
par(mfrow=c(2,2)
  )

# One way ANOVA to examine differences of Mean of Body Condition by Type (NM, Sat, sn)
fi_d3.aov<- aov(body_condition~Type, fi_d3)
class(fi_d3.aov)
summary(fi_d3.aov)
plot(aov(fi_d3.aov, conf.level = 0.99),las=1, col = "purple"
  )

# Use LM instead as unequal numbers in each Type
fi_d3.lm <- lm(body_condition~Type, fi_d3)
class(fi_d3.lm)
summary(fi_d3.lm)
plot(lm(fi_d3.lm), las=1, col = "green")
  )


# 2. Draw and discuss at least one of each of the following:
#   2.1. diagnostic plot
# Residuals vs Fitted
#   the flat line indicates that this data is normally distributed

# Normal Q-Q
#   Outliers are indicated on both the low (28) and high (182, 26) ends
#   blah blah

# Scale Location
#   Shows spread of residuals. 
#   Checks for assumption of equal variance
#   A flat line horizontal line with spread out residual points is good

# Residuals vs Leverage
#   Cook's distance is a measure of overall influence.
#   A measure of less than 0.5 of Leverage on x axis to your first data point is good.


#   2.2  inferential plot (e.g., a coefficient plot, or something from emmeans or effects)

THIS IS NOT WORKING :-(

fish.lm <- lm(log(conc) ~ source + factor(percent), data = fi_d3)
pigs.emm.s <- emmeans(fish.lm, "source")
pairs(fish.emm.s)
