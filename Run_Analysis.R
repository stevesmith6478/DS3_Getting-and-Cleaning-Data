#####################################################################   
## Project: Coursera: Data Science (Johns Hopkins University): 
##          Getting and Cleaning Data: Course Project
##
## Script purpose: to demonstrate your ability to collect, 
##                 work with, and clean a data set.
##
## Date:    August 12, 2019
## Author:  Steven Smith
#####################################################################

library(dplyr)

#####################################################################
# Prepare workspace by downloading and unzipping the dataset
#####################################################################

setwd("C:/Users/Steve/Documents/R_Projects")

dataaddress1 <- "https://d396qusza40orc.cloudfront.net/"
dataaddress2 <- "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataaddress <- paste(dataaddress1, dataaddress2, sep = "")
datadest = "./datafile.zip"

if(!file.exists(datadest)) {
    download.file(dataaddress, destfile = datadest, quiet = FALSE)
}

dataPath <- "UCI HAR Dataset"

if (!file.exists(dataPath)) {
    unzip("datafile.zip")
}

rm(list = c("dataaddress",
            "dataaddress1", 
            "dataaddress2", 
            "datadest"))

#####################################################################
# Read the data files
#####################################################################

trainingSubjects <- read.table(
    file.path(dataPath,"train","subject_train.txt"))

trainingValues <- read.table(
    file.path(dataPath,"train", "X_train.txt"))

trainingActivity <- read.table(
    file.path(dataPath,"train", "y_train.txt"))

testSubjects <- read.table(
    file.path(dataPath,"test", "subject_test.txt"))

testValues <- read.table(
    file.path(dataPath,"test","X_test.txt"))

testActivity <- read.table(
    file.path(dataPath, "test", "y_test.txt"))

features <- read.table(
    file.path(dataPath, "features.txt"), as.is = TRUE)

activities <- read.table(
    file.path(dataPath, "activity_labels.txt"))

colnames(activities) <- c("activityId", "activityLabel")

#####################################################################
# 1. Merge the training and the test sets to create one data set.
#####################################################################

# Merge the data
humanActivity <- rbind(
    cbind(trainingSubjects, trainingValues, trainingActivity),
    cbind(testSubjects, testValues, testActivity))

# remove unnecessary objects
rm(list = c("trainingSubjects",
            "trainingValues",
            "trainingActivity", 
            "testSubjects",
            "testValues",
            "testActivity"))

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

#####################################################################
# 2.  Extract only the measurements on the mean and standard
#     deviation for each measurement.
#####################################################################

# Extract columns related to mean and standard deviation
columnsToKeep <- grepl(
    "subject|activity|mean|std", colnames(
        humanActivity))

humanActivity <- humanActivity[, columnsToKeep]

#####################################################################
# 3.  Use descriptive activity names to name the activities in the
#     data set.
#####################################################################

humanActivity$activity <- factor(
    humanActivity$activity, 
    levels = activities[, 1],
    labels = activities[, 2])

#####################################################################
# 4.  Appropriately label the data set with descriptive variable
#     names.
#####################################################################

# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# clean up names
humanActivityCols <- gsub(
    "^f", "frequencyDomain", humanActivityCols)

humanActivityCols <- gsub(
    "^t", "timeDomain", humanActivityCols)

humanActivityCols <- gsub(
    "Acc", "Accelerometer", humanActivityCols)

humanActivityCols <- gsub(
    "Gyro", "Gyroscope", humanActivityCols)

humanActivityCols <- gsub(
    "Mag", "Magnitude", humanActivityCols)

humanActivityCols <- gsub(
    "Freq", "Frequency", humanActivityCols)

humanActivityCols <- gsub(
    "mean", "Mean", humanActivityCols)

humanActivityCols <- gsub(
    "std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub(
    "BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

#####################################################################
# 5.  From the data set in step 4, create a second, independent
#     tidy data set with the average of each variable for each
#     activity and each subject.
#####################################################################

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
    group_by(subject, activity) %>%
    summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(
    humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

# cleanup
rm(list = ls())
