library(tidyverse)
library(readr)
library(ggsci)
library(lubridate)

## ASSIGNMENT_21_Jan: Construct some (more!) ggplots using my data :-)

## Import the data
fish_info_data <- read_csv("Corse_2016_Genetics_book.csv")


# Select data I want to use (Type, SL, Wt)
# Need to exlude Type(rostratus and Sat/sn)
fish_info_data2 <- fish_info_data %>% 
  filter(!(Type %in% c("rostratus","Sat/sn"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt)


#  Create Body Condition column
fish_info_data3 <- fish_info_data2 %>% 
  mutate(body_condition = 100000000 * (Wt/(10*SL)^3))


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


# Playing around to see how I can add geom commands together and change parameters :-)
ggplot(data = fish_info_data3, mapping = aes(x = Type, y = SL, colour=Type)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5) +
  geom_jitter(shape= 10, position = position_jitter(0.2))


# Look at relationship for Wt and SL in different Types

ggplot(fish_info_data5, aes(y = Wt, x = SL, colour = Type)) +
  geom_point() +
  theme_bw() + scale_color_hue()


# Does body condition decrease for each Type over the spawning season (Date?)

fish_info_data4 <- fish_info_data %>% 
  filter(!(Type %in% c("rostratus","Sat/sn"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt, Date) 

str(fish_info_data4)
fish_info_data5 <- fish_info_data4 %>% 
  mutate(body_condition = 100000000 * (Wt/(10*SL)^3))
 
# Have to change Date column from CHARACTER to DATE 
fish_info_data5$Date<-as.Date(fish_info_data5$Date, format="%d-%B")

# Need to fix that the year "2019" was added to the Date column
fish_info_data5$Date<-update(fish_info_data5$Date, year = 2016)

ggplot(data = fish_info_data5,mapping = aes(x = Date, y = body_condition, colour=Type)) + 
  geom_point() +
  theme_bw() + scale_color_futurama()



# TO WORK THROUGH STILL!

# Draw lines between each of the Types across the dates for last plot?


# My research group made these suggestions to help me format the Date column:
# WAS ONE BETTER THAN THE OTHER?

# Suzanne suggests:
# as.numeric(fish_info_data$Date) 

# Jenn suggests:
## fish_info_data5$Date<-as.Date(fish_info_data5$Date, format="%d-%B")







