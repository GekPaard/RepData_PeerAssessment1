## The datasets used are from: 
## Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
## Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. 
## International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
##
## This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed 
## to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
##
## The dataset are delivered in a zipfile 
## (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
## and are unzipped with winrar in the work space
##
## The assignment stated that one has to extract the mean and the avarage of the measurements.
## These can be found in the files X_test.txt and X_train.txt. The other files don't contain such values.
##
setwd("J:Getting and Cleaning Data/project_workspace")
##
## step 1 : Merges the training and test sets to create one data set
##
## Get the test observations and 
## Get the train observations 
## Concatenate the two tables
## totalTable is the merge of the training and test data set
##
testTable     <- read.table("test/X_test.txt")
trainTable    <- read.table("train/X_train.txt")
totalTable    <- rbind(testTable, trainTable)
##
## step 2 : Extracts only the measurements on the mean and standard deviation for each measurement
##
## Select the columns with refer to measurements on mean and standard deviations.
## The documentation about the datasets states that column names with "mean()", "meanFreq() and "std()" 
## in it refer to the mean and standard deviation of the observations. 
## There are also column names "gravityMean", "~JerkMean" etc. These columns are not included in the result.
## The names are in the file "features.txt.
##
## Search for those names in this file and extract the row number
## Columns in the current dataset are named "V" followed by row number
## Select the columns with "V" followed with the found row number
## Selectedtable is the extract, which contains only the measurements of means and standard deviation 
##
namesTable          <- read.table("features.txt")
namesVector         <- as.vector(namesTable[,2])
searchColumnNumbers <- paste("mean()", "|", "std()", sep= "")
colNumbers          <- grep(searchColumnNumbers, namesVector)
nMax                <- length(colNumbers)
selectedColNumbers  <- vector(mode = "character", length = nMax)
n= 0
while (n < nMax)
{
  n= n+1
  selectedColNumbers[n] <- paste0("V", as.character(colNumbers[n]))
}
selectedTable       <- subset(totalTable, select=selectedColNumbers)
##
## step 3 : Uses descriptive activity names to name the activities in the data set
##
## get the activities
testActivity                <- read.table("test/y_test.txt")
trainActivity               <- read.table("train/y_train.txt")
totalActivity               <- rbind(testActivity, trainActivity)
colnames(totalActivity)     <- "activity_Name"
## change the activity codes into comprehensive texts
activityLabels              <- read.table("activity_labels.txt")
## all column must be of the same class
totalActivity$activity_Name <- as.character(totalActivity$activity_Name)
activityLabels$V1           <- as.character(activityLabels$V1)
activityLabels$V2           <- as.character(activityLabels$V2)
lengthLabels                <- length(activityLabels$V1)
n=0
while (n < lengthLabels)
{
  n = n + 1
  totalActivity$activity_Name[totalActivity$activity_Name == activityLabels$V1[n]] <- activityLabels$V2[n]
}
##
## step 4 : appropiately labels the observation data set with descriptive variable names
##
## the original column names are in namesVector (see step 2)
## colNumbers contain the column numbers, 
## with that find the column name out of namesVector
## create a vector for the appropiate column names
## select the column rownames 
## change these column names in (hopefully) human readable names
##
nMax                <- length(colNumbers) ##just to be sure
selectedColNames    <- vector(mode = "character", length = nMax)
n= 0
while (n < nMax)
{
  n = n+1
  x = colNumbers[n]
  selectedColNames[n] <- namesVector[x]
}
## first remove mistake in row names 
selectedColNames   <- gsub("BodyBody", "Body", selectedColNames)
## change row names
selectedColNames   <- gsub("tBodyAcc", "time_of_body_acceleration_signals_", selectedColNames)
selectedColNames   <- gsub("fBodyAcc", "frequency_of_body_acceleration_signals_", selectedColNames)
selectedColNames   <- gsub("tBodyGyro", "time_of_body_gyroscope_signals_", selectedColNames)
selectedColNames   <- gsub("fBodyGyro", "frequency_of_body_gyroscope_signals_", selectedColNames)
selectedColNames   <- gsub("tGravityAcc", "time_of_gravity_acceleration_signals_", selectedColNames)
selectedColNames   <- gsub("Jerk", "jerk_signal_", selectedColNames)
selectedColNames   <- gsub("Mag", "magnitude_", selectedColNames)
selectedColNames   <- gsub("-meanFreq()", "weighted_average_of_frequency", selectedColNames)
selectedColNames   <- gsub("-mean()", "mean", selectedColNames)
selectedColNames   <- gsub("-std()", "standard_deviation", selectedColNames)
selectedColNames   <- gsub("-X", "_X-axis", selectedColNames)
selectedColNames   <- gsub("-Y", "_Y-axis", selectedColNames)
selectedColNames   <- gsub("-Z", "_Z-axis", selectedColNames)
##
## 
colnames(selectedTable) <- selectedColNames
##
## Step 5 : Creates a second, independent tidy dataset with the average of each activity and each subject
##
## select columns with mean values (= "mean", "average") from the data set of step 4
## get the test and train subjects and concatenate them in the same order as the observations
## label it with clarifying variable name
## cbind the three datasets, subject, activity and observations into one dataset
## Create a data set with the means of all activities (with plyr)
## 
searchColumnNames           <- paste("mean", "|", "average", sep= "")
selectedTable               <- subset(selectedTable, select = grep(searchColumnNames, selectedColNames))
## get the subjects
testSubject                 <- read.table("test/subject_test.txt")
trainSubject                <- read.table("train/subject_train.txt")
totalSubject                <- rbind(testSubject, trainSubject)
colnames(totalSubject)      <- "subject_No"
selectedTable               <- cbind(totalSubject, totalActivity, selectedTable)
## calculate the mean of all observations by subject and activity
library(plyr)
meanofMeanSubject           <- ddply(selectedTable, .(subject_No, activity_Name), colwise(mean))
##
##
## write the data set created in step 5 to a text file
write.table(meanofMeanSubject, file="ProjectStep5.txt", row.names=FALSE)
##
## The end
##
