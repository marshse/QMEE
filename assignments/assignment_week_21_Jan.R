library(tidyverse)
library(readr)

##ASSIGNMENT:Construct some ggplots using my data :-)

## Import the data
fish_info_data <- read.csv("Corse_2016_Genetics_book.csv")

## ASSIGNMENT: Examine the structure and look for mistakes on class assignment!

fish_info_data2 <- fish_info_data %>% 
  filter(Type %in% c("NM", "Sat", "sn", "f")) %>% 
  select(Type, SL, Wt)

