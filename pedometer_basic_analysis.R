library(lattice)
library(mice)

zipName<-"Motion.zip"
UnzipFile<-"activity.csv"
DURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

#Downloading and unzipping (if not done already)
if(!file.exists(zipName))
  download.file(DURL,zipName)
if(!file.exists(UnzipFile))
  unzip(zipName)

steps<-read.csv(UnzipFile)
steps$date<-as.Date(steps$date,"%Y-%m-%d")
totSteps<-tapply(steps$steps,steps$date,sum,na.rm=TRUE)
MeanStepsPerDay<-mean(totSteps)
MedianStepsPerDay<-median(totSteps)
hist(totSteps,xlab="Total Steps per day",col="red",main="Total Steps per day")
abline(v=MeanStepsPerDay,col="blue")
abline(v=MedianStepsPerDay,col="green")

totStepsInInterval<-tapply(steps$steps,steps$interval,mean,na.rm=TRUE)
plot(totStepsInInterval,type="l",col="red",xlab="5-min Interval", 
      ylab="Daily average of Steps",main="Daily Activity Pattern")
abline(h=mean(totStepsInInterval),col="blue")
abline(h=median(totStepsInInterval),col="green")
maxInt<-names(which.max(totStepsInInterval))

misValues<-nrow(steps[!is.na(steps$steps),])
impData<-steps
tempData<-impData
tempData$date<-as.numeric(impData$date)
invisible(tempData<-mice(tempData,method="pmm"))
impData<-complete(tempData,5)
rm(tempData)
impData$date<-steps$date
itotSteps<-tapply(impData$steps,impData$date,sum,na.rm=TRUE)
iMeanStepsPerDay<-mean(itotSteps)
iMedianStepsPerDay<-median(itotSteps)
hist(itotSteps,xlab="Total Steps per day after imputation",
      col="red",main="Total Steps per day")
abline(v=iMeanStepsPerDay,col="blue")
abline(v=iMedianStepsPerDay,col="green")


impData$Dtype<-ifelse(weekdays(impData$date) %in% c("Saturday","Sunday"),
                      "weekend","weekday")
impData$Dtype<-as.factor(impData$Dtype)
WimpData<-aggregate(steps~Dtype+interval,impData,mean)
xyplot(WimpData$steps~WimpData$interval|WimpData$Dtype,type="l",xlab="Interval",
        ylab="Average of steps over days")
