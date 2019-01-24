library(tidyverse)
library(readr)
library(g.data) ## ?? BMB: what's this for?? I don't see it used

fish_info_data <- read.csv("Corse_2016_Genetics_book.csv")

## BMB: these lines shouldn't be in your final code - they're only useful
##  when working interactively
## str(fish_info_data)
## View(fish_info_data)


fish_info_data %>% 
  group_by(Type) %>% 
  summarize(mean_weight = mean(Wt, na.rm = TRUE))


fish_info_data %>% 
  group_by(Type) %>% 
  summarize(mean_weight = mean(Wt, na.rm = TRUE),
            min_weight = min(Wt, na.rm = TRUE),
            max_weight = max(Wt, na.rm = TRUE),
            mean_length = mean(SL, na.rm = TRUE),
            min_length = min(SL, na.rm = TRUE),
            max_length = max(SL, na.rm = TRUE)

## BMB: you can use summarize_all() or summarize_at() to do this slightly
## more efficiently
            ## https://stackoverflow.com/questions/25759891/dplyr-summarise-each-with-na-rm

(fish_info_data
      %>%  group_by(Type)
      %>%  summarise_at(c("Wt", "SL"), funs(min, mean, max), na.rm=TRUE))
)
            
fish_info_data %>% 
  mutate(body_condition = 100 * (Wt/SL))
#  Note that Fulton's Body condition should have the cubed version of the Standard Length (SL) 
## TO BE FIXED!

## BMB: do you want mutate(body_condition = 100*Wt/SL^3) ??
fish_info_data %>% 
  mutate(body_condition = 100 * (Wt/SL))

## BMB: this looks fine. score=2
## (where 1=poor, 2=fine, 3=amazing)
       
