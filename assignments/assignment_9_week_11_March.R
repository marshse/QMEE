Assignment_9_week_11_March

###  Make a (possibly generalized) linear mixed model for one or more of your hypotheses.
# Discuss what choices you made about mixed effects (both grouping variables and related 
##  effects; i.e., what did you put on each side of a pipe)
# Identify the maximal model that you could have used, and explain what you left out 
## (if anything) and why
# Discuss your results



# HYPOTHESIS
#  In a fish species with three different male phenotypes we will hypothesize that 
#  we will find Mean Body Condition differences between Male Types (NM, Sat, sn)
#  in the following way:
#  High Body Condition:  Nm > Sat > sn   :Low Body Condition
#  and that females (due to resproductive invenstment) will have a score close to NM
#  Fish have been collected in adjacent sites(left_far, left, main, main_right, right)


# Start with my linear model

library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(lmPerm)

# Data
fish_info_data <- read_csv("Genetics_book_Corse_2018_v2.csv")

# Clean data set and create body condition column
proc_fun <- function(data,types) {
  return(data
         %>%  filter(Type %in% types)
         %>% droplevels()
         %>% select(Type, SL, Wt, Date, Site)
         %>% mutate(body_condition = 100000 * (Wt/(SL^3)))
         %>% drop_na())
}
# Multiplying by 100000 is to get a human friendly number to look at 

fi_d2 <-fish_info_data %>% proc_fun(type = c("fem", "NM", "Sat", "sn"))
summary(fi_d2)

# Fix Type to be a factor
fi_d2$Type <- factor(fi_d2$Type)

# Run ANOVA
fi_d2.aov <- aov(body_condition~Type, fi_d2)


# Check diagnostic plots
par(mfrow=c(2,2))
class(fi_d2.aov)
summary(fi_d2.aov)
plot(aov(fi_d2.aov), las = 1, col = "purple")



# Examine my data 
print(ggplot(fi_d2.aov, aes(x=Type, y=body_condition))
      + geom_boxplot()
)


# Now fiting the linear mixed model
library(lme4)

lm1 <-  lmer(body_condition ~ Type + (1+Type|Site), data=fi_d2)

# Design the mixed model to include these parameters:
# Fixed Effect of body_condition_score
# Random Effect of Site (left_far, left, main, main_right, right)

# I still stuck at this point. I understand what I want to aks the program,
# but do not yet know the string of incantations to use....


# NOTES:
# I am "less" lost.....I have read through everything but can't find any examples
# which seem to be close enough to my dataset to be helpful as examples.
# I backtracked to figure out how to do the LM before I did the LMM.
# I think what I am looking for is developing a protocol for how I handle data.
# ie. what are the intial steps when approaching a new dataset?





