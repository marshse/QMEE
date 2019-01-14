library(tidyverse)
library(readr)
library(g.data)

fish_info_data <- read.csv("Corse_2016_Genetics_book.csv")

str(fish_info_data)

View(fish_info_data)


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
                      )


fish_info_data %>% 
  mutate(body_condition = 100 * (Wt/SL))
#  Note that Body condition should have the cubed version of the Standard Length 
## TO BE FIXED!

       

        