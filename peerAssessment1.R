## ---- peerAssessment1.R Housekeeping ----
setwd("D:/00 COURSERA/00 Reproducible Research/Peer Assessments/RepData_PeerAssessment1")

echo = TRUE
pkg.to.load.chr <- c ("ggplot2", "knitr")
lapply (pkg.to.load.chr, library, character.only = TRUE)

source.zipFileName.chr <- "activity.zip"
source.zipFile.url <-
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

extract.csvFileName.chr <- "activity.csv"
activityData.dfName.chr <- "activityData.df"


## ---- Download Activity Data zip Dataset as Needed ----
if (! (file.exists (source.zipFileName.chr))) {

    download.file (source.zipFile.url,
                   destfile = source.zipFileName.chr)
}
        

## ---- Extract Activity Data CSV File ----
unzip (source.zipFileName.chr)
        

## ---- Create Activity Data Frame from CSV File ----
activityData.df <- read.csv (extract.csvFileName.chr)


## ---- Calculate Total Number of Steps, ignoring Missing Data ----
totalSteps.int <- tapply (activityData.df $ steps,
                          activityData.df $ date,
                          FUN=sum,
                          na.rm = TRUE) 


## ---- Create Histogram of Total Number of Steps Taken per Day ----
hist (totalSteps.int,
      col = "cyan",
      breaks = 5,
      main = "Histogram of Total Number of Steps Taken Each Day",
      xlab = "Total number of Steps")


# ---- Calculate Mean and Median of Total Number of Steps Taken per Day ----
mean (totalSteps.int, na.rm=TRUE)
median (totalSteps.int, na.rm = TRUE)


## ---- Make Time Series Plot ----
averageSteps.df <- aggregate (x = list (steps = activityData.df $ steps), 
                              by = list (interval = activityData.df $ interval),
                              FUN=mean,
                              na.rm=TRUE)

ggplot (data = averageSteps.df,
        aes (x = interval, y = steps)) +
        geom_line (color = "blue") +
        xlab ("5-Minute Interval") +
        ylab ("Average Number of Steps Taken")


#---- Determine 5-Minute Interval with Maximum Number of Steps ----
averageSteps.df [which.max(averageSteps.df $ steps),] $ interval


## ---- Determine Number of Missing Values in Activity Dataset ----
sum (is.na (activityData.df))


## ---- Create New Activity Dataset with Missing Values as Mean of 5-Minute Interval ----
activityData.noNA.df <- activityData.df

for (i in 1 : nrow (activityData.noNA.df)) {
    if (is.na (activityData.noNA.df $ steps [i])) {
        activityData.noNA.df $ steps [i] <- 
            averageSteps.df [which (activityData.df $ interval [i] == 
                                        averageSteps.df $ interval), ] $ steps
    }
}


## ---- Calculate Total Number of Steps Using Activity Dataset with NO Missing Data ----
totalSteps.noNA.int <- tapply (activityData.noNA.df $ steps,
                          activityData.noNA.df $ date,
                          FUN=sum,
                          na.rm = TRUE) 


## ---- Create Histogram of Total Number of Steps Taken per Day (NO Missing Data) ----
hist (totalSteps.noNA.int,
      col = "cyan",
      breaks = 5,
      main = "Histogram of Total Number of Steps Taken Each Day 
      (NO Missing Data)",
      xlab = "Total number of Steps")


# ---- Calculate Mean and Median of Total Number of Steps Taken per Day (NO Missing Data) ----
mean (totalSteps.noNA.int)
median (totalSteps.noNA.int)


## ---- Create New Factor Variable Day as Weekday or Weekend, Based on Date ---- 
weekdayWeekend <- function (givenDate) {
    if (weekdays (givenDate) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (weekdays (givenDate) %in% c("Saturday", "Sunday"))
        return("weekend")
}

activityData.noNA.df $ day <- sapply (as.Date (activityData.noNA.df $ date),
                                      FUN=weekdayWeekend)
    

## ---- Make Panel Plot of Time Series of 5-Minute Intervals vs. Average Number of Steps ----
averageSteps.noNA.df <- aggregate (steps ~ interval + day,
                                   data = activityData.noNA.df,
                                   mean)
ggplot (averageSteps.noNA.df, aes(interval, steps)) + geom_line(color = "blue") + 
    facet_grid(day ~ .) + xlab ("5-Minute Interval") + ylab ("Average Number of Steps")

## --------------------------------------------------------------------------------------