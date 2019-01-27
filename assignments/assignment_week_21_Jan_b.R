library(tidyverse)
library(readr)

# I think I don't need to load this if I have already loaded tidyverse?
# library(ggplot2)


## ASSIGNMENT: Construct some (more!) ggplots using my data :-)

## Import the data
fish_info_data <- read.csv("Corse_2016_Genetics_book.csv")

# Select data I want to plot (Type, SL, Wt)
# Need to exlude Type(rostratus and Sat/sn)
fish_info_data2 <- fish_info_data %>% 
  filter(Type %in% c("NM", "Sat", "sn", "f")) %>% 
  select(Type, SL, Wt)

# I want to rename my x axis labels. This shows me the order of x axis labels by Type
# levels(fish_info_data2$Type)
levels(fish_info_data2$Type)[1:6] <- c("Female", "NM","rostratus", "SatM","Sat/sn", "SnM")


#  Create Body Condition column
fish_info_data3 <- fish_info_data2 %>% 
  mutate(body_condition = 100 * (Wt/(10*SL)^3))


# Look at distibution of Wt, SL and Body Condition by Type (Fem, NM, SatM, SnM) using 
# "jitter" versus "point"

ggplot(data = fish_info_data3, mapping = aes(x = Type, y = SL, colour=Type)) +
  geom_jitter()

ggplot(data = fish_info_data3, mapping = aes(x = Type, y = SL, colour=Type)) +
  geom_point()

ggplot(data = fish_info_data3, mapping = aes(x = Type, y = Wt, colour=Type)) +
  geom_jitter()

ggplot(data = fish_info_data3, mapping = aes(x = Type,y = Wt, colour = Type)) + 
  geom_point()

ggplot(data = fish_info_data3, mapping = aes(x = Type, y = body_condition, colour=Type)) +
  geom_jitter()

ggplot(data = fish_info_data3, mapping = aes(x = Type, y = body_condition, colour=Type)) +
  geom_point()


# How much does body condition decrease for each Type over the spawning season (Date?)

fish_info_data4 <- fish_info_data %>% 
  filter(Type %in% c("NM", "Sat", "sn", "f")) %>% 
  select(Type, SL, Wt, Date)

fish_info_data5 <- fish_info_data4 %>% 
  mutate(body_condition = 100 * (Wt/(10*SL)^3))

ggplot(data = fish_info_data5,mapping = aes(x = Date, y = body_condition, colour=Type)) + 
  geom_point()


# TO DO!
# Need to clean up dates so that if displays in the proper order
# Draw lines between each of the Types across the dates? General line?
# trying to upload to GitHub

