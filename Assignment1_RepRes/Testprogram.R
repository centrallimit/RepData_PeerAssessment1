library(dplyr)
library(ggplot2)
library(Hmisc)
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

# Load library that contains the impute() function
library(Hmisc)

# Number of datasets that have missing values
count_c <- sum(!complete.cases(df_act))
# Imputing missing values of steps with the mean value
df_act_i <- df_act %>%
        mutate(steps = impute(df_act$steps, mean))

steps_sum_i <- df_act_i %>% 
        group_by(date) %>%
        summarise(stepssum_i=sum(steps,na.rm=TRUE))


c_mean_i <- mean(steps_sum_i$stepssum_i, na.rm = TRUE)

c_med_i <- median(steps_sum_i$stepssum_i, na.rm = TRUE)



WD <- weekdays(as.Date(df_act_i$date))


weekday <- c('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag')

mutate(df_act_i, df_act_i$wDay = factor((weekdays(as.Date(df_act_i$date)) %in% weekday),
                               levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))


       #df1$wDay <- factor((weekdays(df1$date) %in% weekdays1), 
 #                  levels=c(FALSE, TRUE), labels=c('weekend', 'weekday') 
