library(tidyverse)
library(readr)
library(g.data)

## Import the data
fish_info_data <- read.csv("assignment_week_2/Corse_2016_Genetics_book.csv")

## ASSIGNMENT: Examine the structure and look for mistakes on class assignment!
str(fish_info_data)
summary(fish_info_data)


## ASSIGNMENT: Examine data for mistakes
# I examined and everything has the proper class assignment and 
# no errors generated in the summary.


# Select data I want to plot (Type, SL, Wt)
# Need to exlude Type(rostratus and Sat/sn)! I can't get this to work yet.
fish_info_data2 <- fish_info_data %>% 
  # select(Type = NM, Sat, sn, f) %>% 
  select(Type, SL, Wt)

# Re-examine summary after selecting columns
summary(fish_info_data2)


## ASSIGNMENT: Make 1-2 plots to look for errors or anomalies

# Plot the Wt (weight) of fish by type (Fem, NM, SatM,SnM)

ggplot(data = fish_info_data2, mapping = aes(x = Type, y = Wt)) +
  geom_boxplot()
  
#  PLot the SL (standard length) of fish by type (Fem, NM, SatM,SnM)

ggplot(data = fish_info_data2, mapping = aes(x = Type, y = SL)) +
  geom_boxplot()


##  I AM ALSO STILL WORKING ON FIGURING OUT HOW TO DO THIS:
## Change names of vectors (f to Fem, sn to SnM, Sat to SatM in TYPES)
#fish_info_data3 <- fish_info_data2
#sex <- fish_info_data2$sex
#head(sex)
#levels(sex)

# Get summary to check Type names has been changed
#summary(fish_info_data3)

