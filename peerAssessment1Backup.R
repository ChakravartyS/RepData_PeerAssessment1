## ---- peerAssessment1.R Housekeeping ----
setwd("D:/00 COURSERA/00 Reproducible Research/Peer Assessments/RepData_PeerAssessment1")

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


qplot (totalSteps.int, binwidth=1000, xlab="total number of steps taken each day")
mean (totalSteps.int, na.rm=TRUE)
median (totalSteps.int, na.rm = TRUE)






## ---- Read Train & Test Files and Reference Files
        x.train.df <- read.table (x.train.file.name.chr)
        y.train.df <- read.table (y.train.file.name.chr)
        subject.train.df <- read.table (subject.train.file.name.chr)
        
        x.test.df <- read.table (x.test.file.name.chr)
        y.test.df <- read.table (y.test.file.name.chr)
        subject.test.df <- read.table (subject.test.file.name.chr)
        
        features.df <- read.table (features.file.name.chr)
        names (features.df) <- c ("Feature.Id", "Feature.Name")
        
        activity.labels.df <- read.table (activity.labels.name.chr)
        names (activity.labels.df) <- c("Activity.Id", "Activity.Name")
                
## ---- Merge Training and Test Datasets into Single Datasets
        x.data.df <- rbind (x.train.df, x.test.df)
        y.data.df <- rbind (y.train.df, y.test.df)

        subject.df <- rbind (subject.train.df, subject.test.df)
        names (subject.df) <- c("Subject.Id")

## ---- Replace Activity Ids in y.data.df with Corresponding Activity Names 
        y.data.df [, 1] <- activity.labels.df [y.data.df [, 1], 2]
        names (y.data.df) <- c ("Activity.Id")
                        
## ---- Scrub Names of Features for Use as Column Names
        features.df $ Feature.Name <- gsub ("-mean", "Mean", features.df $ Feature.Name)
        features.df $ Feature.Name <- gsub ("-std", "Std", features.df $ Feature.Name)
        features.df $ Feature.Name <- gsub ("[-()]", "", features.df $ Feature.Name)
        features.df $ Feature.Name <- gsub (",", ".", features.df $ Feature.Name)
        
## ---- Determine Mean and Standard Deviation Measurements
        mean.std.features.int <- grep ("Mean|Std\\(\\)", features.df [, 2])
        
## ---- Select Mean and Standard Deviation Measurements in x.data.df
        x.data.df <- x.data.df [, mean.std.features.int]
        
## ---- Assign Scrubbed Column Names in x.data.df
        names (x.data.df) <- features.df [mean.std.features.int, 2]
        
## ---- Combine "x", "y" and "subject" Data Sets by Columns
        all.data.df <- cbind (x.data.df, y.data.df, subject.df)


## ---- Create Independent Tidy Data Set with Mean of Variables for Each Activity & Subject
        tidy.data.df <- ddply (all.data.df, 
                               c ("Subject.Id","Activity.Id"), 
                               numcolwise (mean))

        tidy.data.file.name.chr <- "Run_Analysis_R_Tidy_Data.txt"
        write.table (tidy.data.df, 
                     file = tidy.data.file.name.chr, 
                     row.name = FALSE, 
                     sep = ",")
        
## --------------------------------------------------------------------------------------