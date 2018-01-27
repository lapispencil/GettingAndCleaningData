#LOAD PACKAGES
library(data.table)
library(reshape2)
library(dplyr)

#PATH/DIRECTORY

conPath <- file.path(getwd(), "UCI HAR Dataset")
list.files(conPath, recursive = TRUE)


#READ FILES

trainSubject <- fread(file.path(conPath, "train", "subject_train.txt"))
testSubject <- fread(file.path(conPath, "test", "subject_test.txt"))

trainActivity <- fread(file.path(conPath, "train", "Y_train.txt"))
testActivity <- fread(file.path(conPath, "test", "Y_test.txt"))


#CONVERT TO DATA TABLE

readfile <- function(file) {
  datafile <- read.table(file)
  datatable <- data.table(datafile)
}

traindata <- readfile(file.path(conPath, "train", "X_train.txt"))
testdata <- readfile(file.path(conPath, "test", "X_test.txt"))


#MERGE FILES

subject <- rbind(trainSubject, testSubject)
setnames(subject, "V1", "subject")
activity <- rbind(trainActivity, testActivity)
setnames(activity, "V1", "activityNum")
traintest <- rbind(traindata, testdata)

subject <- cbind(subject, activity)
traintest <- cbind(subject, traintest)

setkey(traintest, subject, activityNum)


#MEAN AND STANDARD DEVIATION

Features <- fread(file.path(conPath, "features.txt"))
setnames(Features, names(Features), c("Number", "featureDesc"))

#SUBSET FEATURENAME WITH KEYWORDS MEAN or STD

Fmeanstd <- Features[grepl("mean\\(\\)|std\\(\\)", featureDesc)]

Fmeanstd$featureCode <- Fmeanstd[, paste0("V", Number)]
head(Fmeanstd)

subs <- c(key(traintest), Fmeanstd$featureCode)
traintest <- traintest[, subs, with = FALSE]

#ADDING DESCRIPTIVE NAMES

activitynames <- fread(file.path(conPath, "activity_labels.txt"))
setnames(activitynames, names(activitynames), c("activityNum", "activityName"))

#MERGE LABELS

traintest <- merge(traintest, activitynames, by = "activityNum", all.x = TRUE)

setkey(traintest, subject, activityNum, activityName)

traintest <- data.table(melt(traintest, key(traintest), variable.name = "featureCode"))

#MERGE ACTIVITY NAME

traintest <- merge(traintest, Fmeanstd[, list(Number, featureCode, featureDesc)], by = "featureCode", all.x = TRUE)


#Create a new factor class variable, activity that is equivalent to activityName.
#Create a new factor class variable, feature that is equivalent to featureDesc.

traintest$activity <- factor(traintest$activityName)
traintest$feature <- factor(traintest$featureDesc)

#Seperate features from featureDesc.

grepFunc <- function(regex) {
  grepl(regex, traintest$feature)
}

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepFunc("^t"), grepFunc("^f")), ncol = nrow(y))
traintest$featDomain <- factor(x %*% y, labels = c("Time", "Frequency"))
x <- matrix(c(grepFunc("Acc"), grepFunc("Gyro")), ncol = nrow(y))
traintest$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepFunc("BodyAcc"), grepFunc("GravityAcc")), ncol = nrow(y))
traintest$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepFunc("mean()"), grepFunc("std()")), ncol = nrow(y))
traintest$featureVar <- factor(x %*% y, labels = c("Mean", "SD"))

## Features with 1 category
traintest$featJerk <- factor(grepFunc("Jerk"), labels = c(NA, "Jerk"))
traintest$featMagnitude <- factor(grepFunc("Mag"), labels = c(NA, "Magnitude"))

## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepFunc("-X"), grepFunc("-Y"), grepFunc("-Z")), ncol = nrow(y))
traintest$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))


#Create a tidy data set
#Create a data set with the average of each variable for each activity and each 
#subject.

setkey(traintest, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featureVar, featAxis)
TidyData <- traintest[, list(count = .N, average = mean(value)), by = key(traintest)]

