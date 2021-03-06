## Formulate two different hypotheses about your data, and describe how you would 
## test them with two different permutation tests 

## FIND THIS IN  README_assignment_week_4_Feb

## Implement one or both of these tests in R. 
## You can use permutation, or you can use a classic test if you explain clearly 
## how it corresponds to a permutation test. Best would be to use both.


#  Mean Body Condition will differ between Male Types (NM, Sat, sn) based on assumptions
#  of fitness investments.
#  Using data from 2018 field season with larger sample sizes
#  High Body Condition:  Nm > Sat > sn   :Low Body Condition

library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(lmPerm)
library(gtools) ## BMB: what for?? in my examples this was for
                ## doing explicit combinations

# Using our 2018 data set as there are more samples!

fish_info_data <- read_csv("Genetics book Corse 2018.csv")
# print(fish_info_data)


# Select what data I want, add body condition column and drop NA values. 
# Date format is ok for this dataset
fi_d2 <- fish_info_data %>% 
#  filter((Type %in% c("NM","Sat","sn"))) %>% 
  filter((Type %in% c("NM","Sat"))) %>%   
  droplevels() %>% 
  select(Type, SL, Wt, Date) %>% 
  mutate(body_condition = 1e8 * (Wt/(10*SL)^3)) %>% 
  drop_na(
  )

## BMB: I'm still curious what the multiplication by 1e8 is doing.
## (maybe you answered in edits to a previous assignment ...

# I am going to try to use this data frame later
fi_d3 <- fish_info_data %>% 
  filter((Type %in% c("NM","Sat","sn"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt, Date) %>% 
  mutate(body_condition = 1e8 * (Wt/(10*SL)^3)) %>% 
  drop_na(
  )

## BMB: you can encapsulate this processing in a function
## and repeat yourself less.
proc_fun <- function(data,types) {
    return(data
           %>%  filter(Type %in% types)
           %>% droplevels()
           %>% select(Type, SL, Wt, Date)
           %>% mutate(body_condition = 1e8 * (Wt/(10*SL)^3))
           %>% drop_na()
           )
}

## Alternatively, you could change the order of the processing a bit
## (I think if you do everything *but* the filtering first, save
## the results in an intermediate variable,
## then filter with one set of types or the other,
## you'll end up in the same place)


fi_d2 <- fish_info_data %>% proc_fun(types=c("NM","Sat"))    
fi_d3 <- fish_info_data %>% proc_fun(types=c("NM","Sat","sn"))    

# Visualize the data         
(ggplot(fi_d2, aes(Type, body_condition))
  + geom_boxplot()
  + stat_sum(colour="blue", alpha=0.3)
  + scale_size(breaks=0:3, range = c(3,6))
)  
## BMB: don't really need stat_sum() if there aren't any repeated/overlapping
## points ...

# Permutation test to compute differences in means of Body Condition by Type
set.seed(101)
nsim <- 1000
res <- numeric(nsim)
for (i in 1:nsim) {
  perm <- sample(nrow(fi_d2))
  bdat <- transform(fi_d2,body_condition=body_condition[perm])
  res[i] <- mean(bdat[bdat$Type=="NM", "body_condition"])-
    mean(bdat[bdat$Type=="Sat", "body_condition"])
}

# I haven't included the other male Type here (sn)
# I still have to figure out how to do a permutation test for more than 2 
# predictor variables

## yup. This is something where we should have given an example. Here is
## an idea
sumfun <- function(data) {
    grand_mean <- mean(data$body_condition)
    grp_means <- c(mean(data[data$Type=="NM","body_condition"]),
                   mean(data[data$Type=="Sat","body_condition"]),
                   mean(data[data$Type=="sn","body_condition"]))
    sum((grp_means-grand_mean)^2)
    ## or: compute overall median, then sum(abs(grp_mean-grand_median))
}

## another version, more compact/abstract ...
sumfun <- function(data, resp_var="body_condition", type_var="Type",
                   FUN=mean) {
    total <- FUN(data[[type_var]])
    agg <- aggregate(data[resp_var], by =list(data[[type_var]]), FUN=FUN)
    res <- sum((total-agg[[resp_var]])^2)
    return(res)
}

## We have a tibble; this changes how we can index
## obs <- mean(fi_d2[fi_d2$Type=="NM", "body_condition"])-
##   mean(fi_d2[fi_d2$Type=="Sat", "body_condition"])
obs <- mean(fi_d2[fi_d2$Type=="NM",][["body_condition"]])-
  mean(fi_d2[fi_d2$Type=="Sat",][["body_condition"]])
res <- c(res,obs)

# I can't get the red line to appear on my histogram :-(
hist(res,col = "blue", las=1, main="")
abline(v=obs, col="red")

## BMB: if you can't compute obs (due to tibble-indexing issues)
##  the line won't show up ...

# Not sure how to put in all three Types in a permutation test? Nm, Sat, sn  


# One way ANOVA to examine differences of Mean of Body Condition by Type (NM, Sat)
fi_d2.aov<- aov(body_condition~Type, fi_d2)
class(fi_d2.aov)
summary(fi_d2.aov)
plot(aov(fi_d2.aov, conf.level = 0.99),las=1, col = "purple"
     )


# One way ANOVA to examine differences of Mean of Body Condition by Type (NM, Sat, sn)
fi_d3.aov<- aov(body_condition~Type, fi_d3)
class(fi_d3.aov)
summary(fi_d3.aov)
plot(aov(fi_d3.aov, conf.level = 0.99),las=1, col = "purple"
     )

## BMB: this does seem to run for me ...
## could use these to extract summary statistic for perm comparison ...
summary(fi_d3.aov)[[1]]$`F value`[1]

a2 <- anova(lm(body_condition~Type, fi_d3))
a2$`F value`[1]

# This last bit of script doesn't want to seem to run?
# SHould I have made fi_d3 in further down the frame? Or is this something to debug?

# Thank you!! I struggled a lot with this assignment but just tried to run through it
# multiple times trying a number of different things.... some of it worked! 
# Some of it didn't... will continue working through this!

## score: 2.5
