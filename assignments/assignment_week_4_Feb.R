## Formulate two different hypotheses about your data, and describe how you would 
## test them with two different permutation tests (In README_assignment_week_4_Feb)
## Implement one or both of these tests in R. 
## You can use permutation, or you can use a classic test if you explain clearly 
## how it corresponds to a permutation test. Best would be to use both.


#  Mean Body Condition will differ between Male Types (NM, Sat, sn) based on assumptions
#  of fitness investments.
#  Using data from 2018 with larger sample sizes
#  High Body Condition:  Nm > Sat > sn   :Low Body Condition

library(tidyverse)
library(ggplot2)
library(lmPerm)
library(coin)
library(gtools)

# Using our 2018 data set as there are more samples!
fish_info_data <- read_csv("Genetics book Corse 2018.csv")
print(fish_info_data)

# Select what data I want, add body condition column and drop NA values. 
# Date format is ok for this dataset
fish_info_data2 <- fish_info_data %>% 
  filter((Type %in% c("NM","Sat","sn"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt, Date) %>% 
  mutate(body_condition = 1e8 * (Wt/(10*SL)^3)) %>% 
  drop_na(
    )


# Visualize the data         
(ggplot(fish_info_data2, aes(Type, body_condition))
  + geom_boxplot()
  + stat_sum(colour="blue", alpha=0.3)
  + scale_size(breaks=0:3, range = c(3,6))
)  


# Permutation test to compute differences in means of Body Condition by Type
set.seed(101)
res <- numeric(1000)
for (i in 1:1000) {
  bdat <- transform(fish_info_data2,
                  body_condition=fish_info_data2$body_condition[sample(nrow(fish_info_data2))])
  res[i] <- mean(bdat[bdat$Type=="NM", "body_condition"])-
    mean((bdat[bdat$Type=="Sat", "body_condition"]))
}
# I haven't included the other male Type here (sn)


# Works until HERE! :-( 

(bdat
  %>% group_by(body_condition)
  %>% summarise(body_condition=mean(body_condition))
  %>% pull(body_condition)  ## extract a single column
  %>% diff()          ## difference between elements
)

obs <- mean(fish_info_data2[fish_info_data2$Type=="NM", "body_condition"])-
        mean(fish_info_data2[fish_info_data2$Type=="Sat", "body_condition"]
             )
# Not sure how to put in all three Types? Nm, Sat, sn  


hist(res,col = "gray", las=1, main="")
abline(v=obs, col="red"
       )


# One way ANOVA to examine differences of Mean of Body Condition by Type
fish_info_data2.aov<- aov(body_condition~Type, fish_info_data2)
class(fish_info_data2.aov)
summary(fish_info_data2.aov)
plot(aov(fish_info_data2.aov, conf.level = 0.99),las=1, col = "purple"
     )

  
  