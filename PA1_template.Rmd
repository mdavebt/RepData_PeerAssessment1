#Reproducible Research: Peer Assessment 1
=========================================

## Loading and preprocessing the data
```{r}
#install.packages("reshape2")
library(reshape2)
#install.packages("ggplot2")
library(ggplot2)
```


```{r}
unzipfile <- function(){
	unzip("activity.zip")
	paste("This file unzipped on",Sys.time())
}
read_data <- function(){
	data <- read.csv("activity.csv")
	meltdata <- melt(data,id.vars = "date",measure.vars="steps",na.rm=TRUE,value.name="STEPS")
	dcastdata <- dcast(meltdata,date~variable,sum,na.rm=TRUE)
	dcastdata
}
```


## What is mean total number of steps taken per day?
```{r}
sumdata <- function(){
	dcastdata <- read_data()
	dcast_exp <- rep.int(dcastdata$date,dcastdata$steps)
	hist(as.Date(dcast_exp,"%Y-%m-%d"),breaks=length(dcastdata$steps),freq=TRUE,main="Total number of steps each day",xlab="Day",ylab="Number of Steps",col="blue")
}
```
```{r}
sumdata()
```

## What is the average daily activity pattern?
```{r}
meandata <- function(){
	dcastdata <- read_data()
	tsteps <- sum(dcastdata$steps)
	ndays <- length(dcastdata$steps)
	meansteps <- tsteps/ndays
	paste("Mean number of steps per day is:",as.integer(meansteps))
}
```
```{r}
meandata()
```
```{r}
mediandata <- function(){
	meltdata <- melt(read_data(),id.vars = "date",measure.vars="steps",na.rm=TRUE,value.name="STEPS")
	dcastdata <- dcast(meltdata,date~variable,sum,na.rm=TRUE)
	tsteps <- median(dcastdata$steps)
	paste("Median of steps in all days:",as.integer(tsteps))
}
```

```{r}
mediandata()
```
```{r}
average_steps <- function(){
	data <- read.csv("activity.csv")
	meltdata <- melt(data,id.vars = "date",measure.vars="steps",na.rm=TRUE,value.name="STEPS")
	dcastdata <- dcast(meltdata,date~variable,mean)
	ggplot(dcastdata,aes(x=as.Date(date,"%Y-%m-%d"),y=steps))+geom_line()+labs(title="Average Steps Taken Every 5 Minutes Each Day")+labs(y="Average Steps")+labs(x="Day")
}
```
```{r}
average_steps()
```
```{r}
max_avg_steps <- function(){
	data <- read.csv("activity.csv")
	meltdata <- melt(data,id.vars = "date",measure.vars="steps",na.rm=TRUE,value.name="STEPS")
	dcastdata <- dcast(meltdata,date~variable,max,na.rm=TRUE)
	maxsteps <- which(dcastdata$steps == max(dcastdata$steps,na.rm=TRUE))
	maxdata <- dcastdata[maxsteps,]
	paste("Maximum steps in 5 minute average is",maxdata$steps,"on",maxdata$date)
}
```	
```{r}
max_avg_steps()
```
## Imputing missing values
```{r}
complete_cases <- function(){
	data <- read.csv("activity.csv")
	complete_data <- complete.cases(data)
	data <- data[complete_data,]
	data
}

```{r}
imputed_total <- function(){
	meltdata <- melt(complete_cases(),id.vars = "date",measure.vars="steps",na.rm=TRUE,value.name="STEPS")
	dcastdata <- dcast(meltdata,date~variable,sum,na.rm=TRUE)
	dcast_exp <- rep.int(dcastdata$date,dcastdata$steps)
	hist(as.Date(dcast_exp,"%Y-%m-%d"),breaks=length(dcastdata$steps),freq=TRUE,main="Total number of steps each day after removing missing data",xlab="Day",ylab="Number of Steps",col="red")
	
}
```
```{r}
imputed_total()
```


## Are there differences in activity patterns between weekdays and weekends?
```{r}
panel_plot <- function(){
	data <- read.csv("activity.csv")
	meltdata <- melt(data,id.vars = "date",measure.vars="steps",na.rm=TRUE,value.name="STEPS")
	dcastdata <- dcast(meltdata,date~variable,mean)

	dcastdata$day <- weekdays(as.Date(dcastdata$date,"%Y-%m-%d"),abbreviate=TRUE)

	for(i in 1:length(dcastdata$day)){
		if(as.character(dcastdata$day[i]) == "Sun" | as.character(dcastdata$day[i]) == "Sat"){
		dcastdata$daytype[i] <- "Weekend"
		}else{dcastdata$daytype[i] <- "Weekday"}
	}
	ggplot(dcastdata,aes(x=as.Date(date,"%Y-%m-%d"),y=steps))+geom_line()+labs(title="Average Steps Taken Every 5 Minutes Each Day")+labs(y="Average Steps")+labs(x="Day")+facet_wrap(.~daytype,nrow=2)
}
```
```{r}
panel_plot()
```
