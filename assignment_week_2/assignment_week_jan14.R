library(tidyverse)
library(readr)

## Import the data
fish_info_data <- read.csv("Corse_2016_Genetics_book.csv")

## ASSIGNMENT: Examine the structure and look for mistakes on class assignment!
# str(fish_info_data)
# summary(fish_info_data)

## ASSIGNMENT: Examine data for mistakes
# I examined and everything has the proper class assignment and 
# no errors generated in the summary.

## JD: nice

# Select data I want to plot (Type, SL, Wt)
# Need to exlude Type(rostratus and Sat/sn)
## JD: if the logic is excluding, it might be better to do it that way:
## `filter(!(Type %in% c("rostratus", …)))`
## The ! does logical negation

fish_info_data2 <- fish_info_data %>% 
  filter(Type %in% c("NM", "Sat", "sn", "f")) %>% 
  select(Type, SL, Wt)

## JD: It's probably good to drop levels here, now that you've gotten rid of things you don't like

# Re-examine summary after selecting columns
# summary(fish_info_data2)

## ASSIGNMENT: Make 1-2 plots to look for errors or anomalies

# I want to rename my x axis labels. This shows me the order of x axis labels by Type
# levels(fish_info_data2$Type)

## JD: This is probably bad code: you are just fiddling with the labels
## If it did anything, it would likely lose the true fish identities
levels(fish_info_data2$Type)[1:6] <- c("Female", "NM","rostratus", "SatM","Sat/sn", "SnM")
## A good way to control factor levels would be by explicitly constructing factors
## That is probably one of the reasons that read_csv does it that way
## … and you should consider using read_csv instead of read.csv

# Plot the Wt (weight) of fish by type (Fem, NM, SatM, SnM)
ggplot(data = fish_info_data2, mapping = aes(x = Type, y = Wt)) +
  geom_boxplot()
  
# But I have introduced something weird here for the rostratus. No value at ~5.3!
## JD: Confused. You got rid of all the rostratus

#  PLot the SL (standard length) of fish by type (Fem, NM, SatM, SnM)
ggplot(data = fish_info_data2, mapping = aes(x = Type, y = SL)) +
  geom_boxplot()


#  PLot the Body Condition of fish by type (Fem, NM, SatM, SnM)
fish_info_data3 <- fish_info_data2 %>% 
  mutate(body_condition = 100 * (Wt/(10*SL)^3))
## JD: There's probably a units problem here
## Since condition is more or less on an arbitrary scale, why not multiply by a million to get human-friendly numbers?

ggplot(data = fish_info_data3, mapping = aes(x = Type, y = body_condition)) +
  geom_boxplot()

# Look at Wt and SL in females only. 

fish_info_data4 <- fish_info_data3 %>%
  filter(Type %in% "Female")

ggplot(fish_info_data4,
       aes(y = Wt, x = SL)) +
  geom_point()

## JD: It would be more fun to look at everyone together, using either colors or facets (or both, since we're exploring)

## JD: score 2. (1=poor, 2=fine, 3=excellent)

