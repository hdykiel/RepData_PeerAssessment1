#Personaly Activity Data

#Set working directory
setwd("C:/Users/n0278855/Dropbox/Coursera Data Science/reproducibleResearch/assignment1/RepData_PeerAssessment1-master/")
setwd("C:/Users/Hadrien/Dropbox/Coursera Data Science/reproducibleResearch/assignment1/RepData_PeerAssessment1-master")

#Load required packages
library(ggplot2)
library(plyr)
library(mice) #used to impute missing values
library(chron)

df <- read.csv('Data/activity.csv') #Load Data

# #EDA
# head(df[!is.na(df$steps), ],50)
# head(subset(df, steps > 0),50)
# length(unique(df$date)) #print number of days included in data set

#TOTAL DAILY STEPS
df2 <- df[!is.na(df$steps), ] #remove NAs
df2_summarized <- ddply(df2, c("date"), summarize, sumSteps = sum(steps)) #summarize

#plot results
ggplot(data=df2_summarized, mapping = aes(x=date, y=sumSteps)) +
  geom_bar(stat="identity", fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90)) + #rotate x axis labels
  ggtitle('Steps Per Day') +
  theme(plot.title = element_text(face='bold', size=16))

#Calculate mean & median
mean(df2_summarized$sumSteps, na.rm = TRUE)
median(df2_summarized$sumSteps, na.rm = TRUE)

#DAILY ACTIVITY PATTERN
ggplot(data=df2, aes(interval, steps)) +
  stat_summary(fun.y = mean, geom = 'line') +
  ggtitle('Avg Steps Per 5 Minute Interval') +
  theme(plot.title = element_text(face='bold', size=16)) 

avg_steps_by_interval <- ddply(df2, c("interval"), summarize, avgSteps = sum(steps)) #summarize

#Print busiest time
busiestInterval <- (avg_steps_by_interval$interval[avg_steps_by_interval$avgSteps == max(avg_steps_by_interval$avgSteps, na.rm = T)])

#MISSING VALUES
sum(is.na(df$steps)) #calculate sum of NAs
tempData <- mice(df, seed = 500) #impute missing values
df3 <- complete(tempData, 1)
df3_summarized <- ddply(df3, c("date"), summarize, sumSteps = sum(steps)) #summarize

#Plot new data set
ggplot(data=df3_summarized, mapping = aes(x=date, y=sumSteps)) +
  geom_bar(stat="identity", fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90)) + #rotate x axis labels
  ggtitle('Steps Per Day (Imputed)') +
  theme(plot.title = element_text(face='bold', size=16))

ggplot(data=df3, aes(interval, steps)) +
  stat_summary(fun.y = mean, geom = 'line') +
  ggtitle('Avg Steps Per 5 Minute Interval (Imputed)') +
  theme(plot.title = element_text(face='bold', size=16)) 

#Calculate mean & median
mean(df3_summarized$sumSteps, na.rm = TRUE)
median(df3_summarized$sumSteps, na.rm = TRUE)

#WEEKDAY VS WEEKENDS
df3$weekend <- is.weekend(df3$date)
for (i in 1:nrow(df3)) {

  if (df3$weekend[i] == TRUE) {
    df3$dayType[i] <- 'weekend'
  } else {
    df3$dayType[i] <- 'weekday'
  }

}

#   if (is.weekend(df3$date[i])) {
#     
#     df3$dayType[i] <- as.factor('weekend')
#     
#   } else {
#     
#     df3$dayType[i] <- as.factor('weekday')
#   }






