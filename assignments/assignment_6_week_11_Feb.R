# Assignment_6_week_11_Feb

# 1. Make a linear model for one or more of your hypotheses. 
# 2. Draw and discuss at least one of each of the following:
#   2.1. diagnostic plot
#   2.2  inferential plot (e.g., a coefficient plot, or something from emmeans or effects)


library(tidyverse)
library(ggplot2); theme_set(theme_light())
library(lmPerm)
library(emmeans)
library(effects)#: carData  #carData won't seem to load now?

# 1. Make a linear model

# HYPOTHESIS
#  In a fish species with three different male phenotype we will hypothesize that 
#  we will find Mean Body Condition differences between Male Types (NM, Sat, sn)
#  in the following way:
#  High Body Condition:  Nm > Sat > sn   :Low Body Condition
#  Now using data collected from 2018 field season that has larger sample sizes


fish_info_data <- read_csv("Genetics book Corse 2018.csv")
# print(fish_info_data)


# Create a function that I can use to generate different subsets of the data
# and to calculate body condition
proc_fun <- function(data,types) {
  return(data
         %>%  filter(Type %in% types)
         %>% droplevels()
         %>% select(Type, SL, Wt, Date)
         %>% mutate(body_condition = 1e8 * (Wt/(10*SL)^3))
			%>% mutate_if(is.character, as.factor) ## COOL code from BB
         %>% drop_na())
  }

## Said by BMB: "I'm still curious what the multiplication by 1e8 is doing."
## SMR: body condition is usually 100*(Wt/(10*SL)^3)) and JD suggested that
## "Since condition is more or less on an arbitrary scale, 
## why not multiply by a million to get human-friendly numbers?"
## JD: A million is 1e6 ☺ – but the lesson here is that you should a comment 
## when you do something weird like that.


fi_d2 <- fish_info_data %>% proc_fun(types=c("NM","Sat"))    
fi_d3 <- fish_info_data %>% proc_fun(types=c("NM","Sat","sn"))  

# Show 4 plots in one frame
par(mfrow=c(2,2)
  )

# This didn't work!
## JD: I don't generally recommend AOV
# One way ANOVA to examine differences of Mean of Body Condition by Type (NM, Sat, sn)
#fi_d3.aov<- aov(body_condition~Type, fi_d3)
#class(fi_d3.aov)
#summary(fi_d3.aov)
#plot(aov(fi_d3.aov, conf.level = 0.99),las=1, col = "purple")

# Now figured out how to use LM instead as unequal sample size for each male Type
fi_d3.lm <- lm(body_condition~Type, fi_d3)
class(fi_d3.lm)
summary(fi_d3.lm)
plot(lm(fi_d3.lm), las=1, col = "green"
     )
  

# 2. Draw and discuss at least one of each of the following:
#   2.1. diagnostic plot

##  DISCUSSION
# Both the ANOVA and LM diagnostic plots appear very similar: 

# Residuals vs Fitted GRAPH
#   Flat line across fitted values indicates that this data is normally distributed

# Normal Q-Q GRAPH
#   Outliers are indicated on both the low (28) and high (182, 26) ends
#   but majority of residuals fall nicely along a fairly straight line

# Scale Location GRAPH
#   Residuals appear to be randomly spread which supports an 
#   assumption of equal variance.
#   Flat horizontal line with spread out residual points is a good thing

# Residuals vs Leverage GRAPH
#   Cook's distance is a measure of overall influence.
#   A measure of less than 0.5 of Leverage on x axis to your first data point is good.
#   All points are well within Cook's distance lines. Therefore we seem not to have any 
#   data points which might heavily influence the regression line whether they are 
#   excluded or not.



#   2.2  inferential plot (e.g., a coefficient plot, or something from emmeans 
# or effects)

## JD: One good way to look at an anova-style model should be drop1
drop1(fi_d3.lm, test="F")
## This shows the overall significance of type

#summary(allEffects(fi_d3.lm))
coef(fi_d3.lm)
plot(allEffects(fi_d3.lm))
#This plot did not work :-(
## JD: Because effect is looking for numeric predictors
## JD: Sorry, got that wrong. You just need to tell it that Type is a factor
## This seems like an infelicity (because effects thinks differently than lm)
## Fixed above (see COOL code)

## JD: You can plot model coefficients with dwplot
library(dotwhisker)
dwplot(fi_d3.lm)

## JD: or drop the intercept to get a parameter for each level
dwplot(lm <- lm(body_condition~Type-1, fi_d3))

#Examples to try to work with:
#fish.lm <- lm(log(conc) ~ source + factor(percent), data = fi_d3)
#fish.emm.s <- emmeans(fish.lm, "source")
#pairs(fish.emm.s)

# Another example to try to work with???
#warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#warp.emm <- emmeans(warp.lm, ~ tension | wool)
#plot(warp.emm)
#plot(warp.emm, by = NULL, comparisons = TRUE, adjust = "mvt", horizontal = FALSE)

#   I did a lot of research and tried a few different methods but I failed to be able to 
# make something successfully for the second part of the assignment
#  I have been working on this and researching a variety or approaches and script
# suggestions but have been unable to find something that I can make work with my data.
#   I am still trying to work through this to figure out how to get it to work!! 
#   Thanks so much again for the extention!!!

## Grade 1.8/3 (OK)

