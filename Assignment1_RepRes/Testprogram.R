library(dplyr)
library(ggplot2)
setwd("~/Documents/RepData_PeerAssessment1")
df_act <- tbl_df(read.csv("activity.csv", header = TRUE, sep = ","))

mutate(df_act, date=as.Date(date))


steps_sum <- df_act %>% 
        group_by(date) %>%
        summarise(stepssum = sum(steps,na.rm=TRUE))



c_mean <- mean(steps_sum$stepssum, na.rm = TRUE)
c_med <- as.numeric(median(steps_sum$stepssum, na.rm = TRUE))

ggplot(steps_sum, aes(x=steps_sum$stepssum)) +
geom_histogram(colour="black", fill="red") + xlab("Total number of steps by day")


steps_day <-  df_act %>%
        group_by(interval) %>%
        summarise(mean_int=mean(steps, na.rm = TRUE))
        
c_max <- steps_day[which.max(steps_day$mean_int), ]

ggplot(steps_day, aes(interval, mean_int)) + geom_line() 
+ xlab("Interval") + ylab("Mean")
IMP_DF <- df_act %>%
        filter(!complete.cases) 



