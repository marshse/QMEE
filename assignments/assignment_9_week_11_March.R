## Assignment_9_week_11_March

###  Make a (possibly generalized) linear mixed model for one or more of your hypotheses.
## Discuss what choices you made about mixed effects (both grouping variables and related 
#  effects; i.e., what did you put on each side of a pipe)
## Identify the maximal model that you could have used, and explain what you left out 
# (if anything) and why
## Discuss your results



# HYPOTHESIS
##  In a fish species with three different male phenotypes we will hypothesize that 
#  we will find Mean Body Condition differences between Male Types (NM, Sat, sn)
#  in the following way:
#  High Body Condition:  Nm > Sat > sn   :Low Body Condition
#  and that females (due to high reproductive investment) will have a score close to NM
##  Fish have been collected in adjacent sites(left_far, left, main, main_right, right) 
#  within a large cove at the study site.


# Starting with my linear model (I haven't yet been able to get this to run before)

library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(lmPerm) # needed to run linear model

# Data
fish_info_data <- read_csv("Genetics_book_Corse_2018_v2.csv")

## BMB: ideally all of this clean-up stuff would be in a separate file,
## you would store the results and just read it in -- wouldn't have to keep
##  copying all of this cleaning code

# Clean data set and create body condition column
proc_fun <- function(data,types) {
  return(data
         %>%  filter(Type %in% types)
         %>% droplevels()
         %>% select(Type, SL, Wt, Date, Site)
         %>% mutate(body_condition = 100000 * (Wt/(SL^3)))
         %>% drop_na())
}     # Multiplying by 100000 is to get a human friendly number to look at 

fi_d2 <-fish_info_data %>% proc_fun(type = c("fem", "NM", "Sat", "sn"))
summary(fi_d2) #Checking proper class for each column

# Fix Type to be a factor instead of a character (needed so aov will run!)
fi_d2$Type <- factor(fi_d2$Type)
summary(fi_d2) # Fixed!

# Run ANOVA
fi_d2.aov <- aov(body_condition~Type, fi_d2)

# Check diagnostic plots
par(mfrow=c(2,2))  # show four graphs in one panel
plot(aov(fi_d2.aov), las = 1, col = "purple")  # no issues in these plots

# Look at summary statistics
summary(fi_d2.aov)  # shows significant difference between types. But which ones?

# TukeyHSD will compare between types and control for multiple comparisons
TukeyHSD(fi_d2.aov, ordered = FALSE, conf.level = 0.95)
# so only pairs that aren't significantly different from each other are Sat-fem

# Look at the the data 
print(ggplot(fi_d2.aov, aes(x=Type, y=body_condition))
      + geom_boxplot()
)

# Plot an effects plot 
library(effects)
plot(allEffects(fi_d2.aov))


# Now fiting the linear mixed model

library(lme4) # needed to run linear mixed models


# Design the mixed model to include these parameters:
# Fixed Effect of body_condition_score
# Random Effect of Site (left_far, left, main, main_right, right)

lm1 <-  lmer(body_condition ~ Type + (1|Site), data=fi_d2)

## BMB: you should at least consider (Type|Site) 

lm2 <- update(lm1, . ~ . - (1|Site) + (Type|Site))

## BMB: although this is indeed a singular fit ...

# Check summary stats for the model
summary(lm1)  
# Var and Std Dev is very low for Random Effects. 
# Therefore random effect of site has little impact
# on overall model.


# How does effects plot changes after running mixed model
plot(allEffects(lm1))

# P.S. Apologies for recent assignments that have had terrible formatting and 
# chunks of code all over the place.  I've been a little lost the past couple 
# of weeks but am working hard to get up to speed. Thanks!! Susan



