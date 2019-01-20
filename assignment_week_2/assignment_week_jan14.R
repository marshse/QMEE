library(tidyverse)
library(readr)

## Import the data
fish_info_data <- read.csv("assignment_week_2/Corse_2016_Genetics_book.csv")

## ASSIGNMENT: Examine the structure and look for mistakes on class assignment!
# str(fish_info_data)
# summary(fish_info_data)

## ASSIGNMENT: Examine data for mistakes
# I examined and everything has the proper class assignment and 
# no errors generated in the summary.


# Select data I want to plot (Type, SL, Wt)
# Need to exlude Type(rostratus and Sat/sn)! I can't get this to work yet.
## If I take out the comment here I get only some of the rows I want. Got only 19 obs.

fish_info_data2 <- fish_info_data %>% 
#  filter(Type == c("NM", "Sat", "sn", "f")) %>% 
  select(Type, SL, Wt)


# Re-examine summary after selecting columns
# summary(fish_info_data2)


## ASSIGNMENT: Make 1-2 plots to look for errors or anomalies

# I want to rename my x axis labels. This shows me the order of x axis labels by Type
# levels(fish_info_data2$Type)

levels(fish_info_data2$Type)[1:6] <- c("Female", "NM","rostratus", "SatM","Sat/sn", "SnM")


# Plot the Wt (weight) of fish by type (Fem, NM, SatM, SnM)

ggplot(data = fish_info_data2, mapping = aes(x = Type, y = Wt)) +
  geom_boxplot()
  
# But I have introduced something weird here for the rostratus. No value at ~5.3!


#  PLot the SL (standard length) of fish by type (Fem, NM, SatM,SnM)

ggplot(data = fish_info_data2, mapping = aes(x = Type, y = SL)) +
  geom_boxplot()


