library(tidyverse)
library(readr)
library(g.data)

## Import the data
fish_info_data <- read.csv("assignment_week_2/Corse_2016_Genetics_book.csv")

## ASSIGNMENT: Examine the structure
str(fish_info_data)

## ASSIGNMENT: Examine data for mistakes

# Remove the NA's  ????


# Remove data not needed for summary
# Need to get rid of Types(rostratus and Sat/sn)!
fish_info_data2 <- fish_info_data

# Get summary
summary(fish_info_data2)

#Change names of vectors (f to Fem, sn to SnM, Sat to SatM)
fish_info_data3 <- fish_info_data2

# Get summary to check this has been changed
summary(fish_info_data3)

## ASSIGNMENT: Make 1-2 plots to look for errors or anomalies

# Plot the Wt (weight) of fish by type (Fem, NM, SatM,SnM)

fish_info_dat3 <- select(fish_info_dat3, Type, Wt)
 
ggplot(data = fish_info_dat3, mapping = aes(x = Type, y = Wt)) +
  geom_boxplot()
  
#  PLot the SL (standard length) of fish by type (Fem, NM, SatM,SnM)

fish_info_dat3 <- select(fish_info_dat3, Type, SL)

ggplot(data = fish_info_dat3, mapping = aes(x = Type, y = SL)) +
  geom_boxplot()



