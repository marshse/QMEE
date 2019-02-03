library(tidyverse)
library(readr)
library(ggsci)
library(lubridate)

## ASSIGNMENT_21_Jan: Construct some (more!) ggplots using my data :-)

## Import the data
fish_info_data <- read_csv("Corse_2016_Genetics_book.csv")


# Select data I want to use (Type, SL, Wt)
# Need to exclude Type(rostratus and Sat/sn)
fish_info_data2 <- fish_info_data %>% 
  filter(!(Type %in% c("rostratus","Sat/sn"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt)


#  Create Body Condition column
fish_info_data3 <- fish_info_data2 %>% 
    mutate(body_condition = 100000000 * (Wt/(10*SL)^3))
## BMB: hmmm.  I understand the Wt/SL^3 part, but what's the huge
##  constant doing? (might want to write 1e8 instead of 100000000 so
## you don't have to count zeros)

# Look at distribution of Wt, SL and Body Condition by Type (Fem, NM, SatM, SnM) using 
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

## BMB: can make this tidier/easier to read by saving the components, e.g.

gg0 <- ggplot(data = fish_info_data3,
              mapping = aes(x = Type, y = SL, colour=Type))

gg0 + geom_point()
gg0 + geom_jitter()

##
fi_d4 <- fish_info_data3 %>% gather(key=var,value=val,-Type)
gg1 <- ggplot(data = fi_d4, aes(x = Type, y = val, colour=Type)) +
    facet_wrap(~var,scale="free")
gg1 + geom_point()
## useful in this case to jitter only horizontally, and a bit less than
##  the default
gg1 + geom_jitter(width=0.2,height=0)

# Playing around to see how I can add geom commands together and change parameters :-)
ggplot(data = fish_info_data3, mapping = aes(x = Type, y = SL, colour=Type)) +
  geom_boxplot() +
    geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5,
                 aes(fill=Type), colour="black") +
  geom_jitter(shape= 10, position = position_jitter(0.2))
## BMB: nice. see my tweaks to geom_dotplot


# Does body condition decrease for each Type over the spawning season (Date?)

fish_info_data4 <- fish_info_data %>% 
  filter(!(Type %in% c("rostratus","Sat/sn"))) %>% 
  droplevels() %>% 
  select(Type, SL, Wt, Date) 

str(fish_info_data4)
fish_info_data5 <- fish_info_data4 %>% 
  mutate(body_condition = 100000000 * (Wt/(10*SL)^3))

## BMB: I might go upstream and retain Date in fish_info_data3
##  so you don't have to repeat these commands

## BMB: could use mutate for this and the next option
# Have to change Date column from CHARACTER to DATE 
fish_info_data5$Date<-as.Date(fish_info_data5$Date, format="%d-%B")

# Need to fix that the year "2019" was added to the Date column
fish_info_data5$Date<-update(fish_info_data5$Date, year = 2016)

## BMB: cleaner ...
## fish_info_data5 <- (fish_info_data5
##     %>% mutate(Date=as.Date(paste0(Date,"-2016"), format="%d-%B-%Y"))
## )

## BMB: this was out of order (uses fish_info_data5)
# Look at relationship for Wt and SL in different Types

ggplot(fish_info_data5, aes(y = Wt, x = SL, colour = Type)) +
  geom_point() +
  theme_bw() + scale_color_hue()

## BMB: try + scale_x_log10()  + scale_y_log10()
## morphometric data almost always belong on a log-log scale

gg2 <- ggplot(data = fish_info_data5,mapping = aes(x = Date, y = body_condition, colour=Type)) + 
  geom_point() +
  theme_bw() + scale_color_futurama()
print(gg2)
## BMB: maybe add geom_smooth() to this?

# TO WORK THROUGH STILL!

# Draw lines between each of the Types across the dates for last plot?
gg2 + geom_smooth()

## connect mean values 
gg2 + stat_summary(fun.y=mean, geom="line",
                   alpha=0.3, size=5)

## accidental art! bootstrap CIs - not very good when
##  there are only a few points ... and sometimes only one ... per date
(gg2
    + stat_summary(fun.y=mean, geom="line")
    + stat_summary(fun.data=mean_cl_boot, geom="ribbon",
                   aes(fill=Type), alpha=0.2,colour=NA)
    + scale_fill_futurama()
)
# My research group made these suggestions to help me format the Date column:
# WAS ONE BETTER THAN THE OTHER?

# Suzanne suggests:
# as.numeric(fish_info_data$Date) 

# Jenn suggests:
## fish_info_data5$Date<-as.Date(fish_info_data5$Date, format="%d-%B")

## BMB: I prefer the latter, but see above

## score: 2.5






