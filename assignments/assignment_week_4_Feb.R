## Formulate two different hypotheses about your data, and describe how you would 
## test them with two different permutation tests (In README file)
## Implement one or both of these tests in R. 
## You can use permutation, or you can use a classic test if you explain clearly 
## how it corresponds to a permutation test. Best would be to use both.


#  Mean Body Condition will differ between Male Types (NM, Sat, sn)

library(tidyverse)
library(ggplot2)
library(lmPerm)
library(coin)
library(gtools)

fish_info_data <- read_csv("Corse_2016_Genetics_book.csv")
print(fish_info_data)

fish_info_data2 <- fish_info_data %>% 
  filter(!(Type %in% c("rostratus","Sat/sn", "f"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt, Date) %>% 
  mutate(body_condition = 1e8 * (Wt/(10*SL)^3)) %>% 
  mutate(Date=as.Date(paste0(Date,"-2016"), format="%d-%B-%Y"))         
         
(ggplot(fish_info_data2, aes(Type, body_condition))
  + geom_boxplot()
  + stat_sum(colour="blue", alpha=0.3)
  + scale_size(breaks=0:3, range = c(3,6))
)  


set.seed(101)
res <- numeric(1000)
for (i in 1:1000) {
  bdat <- transform(fish_info_data2,
                  body_condition=fish_info_data2$body_condition[sample(nrow(fish_info_data2))])
  res[i] <- mean(bdat[bdat$Type=="NM", "body_condition"])-
    mean((bdat[bdat$Type=="Sat", "body_condition"]))
}

# Works until HERE!

obs <- mean(fish_info_data2[fish_info_data2$Type=="NM", "body_condition"])-
        mean(fish_info_data2[fish_info_data2$Type=="Sat", "body_condition"])

hist(res,col = "gray", las=1, main="")
abline(v=obs, col="red")


## using tidyverse
(bdat
  %>% group_by(body_condition)
  %>% summarise(body_condition=mean(body_condition))
  %>% pull(body_condition)
  %>% diff()
  )



  
  