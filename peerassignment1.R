
activitydata<-read.table("activity.csv",header=T,sep=",")
activitydata$date<-as.Date(activitydata$date,"%Y-%m-%d")

##Total number of steps per day
stepsbyday <- aggregate(steps~date, activitydata,sum)
##stepsbyday<-tapply(activitydata$steps,activitydata$date,sum)
barplot(stepsbyday,main="Steps by Day")

meanbyday<-aggregate(steps~date,activitydata,mean)
stepsbyday<-aggregate(steps~date,activitydata,sum)
median(stepsbyday$steps,na.rm=T)

##average number ofsteps by interval
avgstepsbytime<-aggregate(steps~interval,activitydata,mean)
plot(avgstepsbytime,type='l')

##  the 5 mins interval with max number of steps
avgstepsbytime$interval[which.max(avgstepsbytime$steps)]

##total number of rows with NA
sum(!complete.cases(activitydata))

## derive the mean for the day and replace it with missing values
activitydata_new<-activitydata
##activitydata_new<-transform(activitydata_new,steps=if(is.na(activitydata_new$steps))activitydata_new$steps<-avgstepsbytime$steps[match(activitydata_new$interval,avgstepsbytime$interval)])

for(i in seq_along(activitydata_new$steps))
if(is.na(activitydata_new$steps[i])){
  activitydata_new$steps[i]
  activitydata_new$steps[i]<-avgstepsbytime$steps[match(activitydata_new$interval[i],avgstepsbytime$interval)]
}

stepsbyday_n<-aggregate(steps~date,activitydata_new,sum)
hist(stepsbyday_n$steps, main = "Number of steps taken per day", col = "green", xlab="Steps taken per day")

meanbyday_n<-aggregate(steps~date,activitydata_new,mean)
mean(stepsbyday_n$steps)
median(stepsbyday_n$steps)

wknd<-factor(c("Mon","Tue","Wed","Thu","Fri"))
activitydata_new$weekday<-(weekdays(activitydata_new$date,abbreviate=T))
activitydata_new$weekday<-as.factor(ifelse(activitydata_new$weekday%in%wknd,"weekday","weekend"))

par(mfrow=c(2,1))
a<-aggregate(activitydata_new$steps,by=list(Interval= activitydata_new$interval,wknd = activitydata_new$weekday),mean)

b<-a[a$wknd=="weekday",]

plot(b$Interval,b$x,type="l")
b<-a[a$wknd=="weekend",]
plot(b$Interval,b$x,type="l")


