# 0. Download and unzip data
fileName <- "data_set.zip"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# 0.1 Download file
if(!file.exists(fileName)) {
    download.file(fileUrl, destfile = fileName, method = "curl")
}
# 0.2 Record download date
dateDownloaded <- date()
# 0.3 Unzip file
if(!file.exists("UCI HAR Dataset")) {
    unzip(fileName)
}

# Mission 1: Merge the training and the test sets to create one data set.

# 1.1 Read training data set into R
# 1.1.1 Read training set
trainSet <- read.table("UCI HAR Dataset/train/X_train.txt")
# 1.1.2 Read training subject data
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
# 1.1.3 Read training label data
trainLabel <- read.table("UCI HAR Dataset/train/Y_train.txt")
# 1.1.4 Combine subject, label and feature together
trainData <- cbind(trainSubject, trainLabel, trainSet)

# 1.2 Read test data set into R
# 1.2.1 Read test set
testSet <- read.table("UCI HAR Dataset/test/X_test.txt")
# 1.2.2 Read training subject data
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
# 1.2.3 Read training label data
testLabel <- read.table("UCI HAR Dataset/test/Y_test.txt")
# 1.2.4 Combine subject, label and feature together
testData <- cbind(testSubject, testLabel, testSet)

# 1.3 Combine training and test data together
data <- rbind(trainData, testData)

# 1.4 Set column names
# 1.4.2 Read feature list and extract feature names
featureList <- read.table("UCI HAR Dataset/features.txt")
featureNames <- as.character(featureList[,2])
featureNames <- c("subject", "activity", featureNames)
# 1.4.3 Set column names
colnames(data) <- featureNames

# Mission 2: Extracts only the measurements on the mean and 
# standard deviation for each measurement.

# 2.1 Find out the feature index which contains mean or std
index <- grep("mean\\(\\)|std\\(\\)", featureNames)
# 2.2 Add the index of subject and labels into the index list
index <- c(1, 2, index)
# 2.3 Subset the data
subData <- data[,index]
# 2.4 Format feature names
subFeatureNames <- names(subData)
subFeatureNames <- gsub("mean", "Mean", subFeatureNames)
subFeatureNames <- gsub("std", "Std", subFeatureNames)
subFeatureNames <- gsub("-|\\(\\)", "", subFeatureNames)
# 2.5 Reset the column name of sub data
colnames(subData) <- subFeatureNames

# Mission 3: Uses descriptive activity names to name the activities 
# in the data set

# 3.1 Load activity names
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
# 3.2 Format the label names
labelNames <- tolower(activityLabels[, 2])
labelNames <- gsub("upstairs", "Upstairs", labelNames)
labelNames <- gsub("downstairs", "Downstairs", labelNames)
labelNames <- gsub("_", "", labelNames)
activityLabels[, 2] <- labelNames

# Mission 4: Appropriately labels the data set with descriptive 
# variable names.

# 4.1 Create a factor vector coresponding to label column
labelFactor <- as.factor(activityLabels[subData$activity, 2])
# 4.2 Replace the label column in data set to factor vector
subData$activity <- labelFactor
# 4.3 Output clean subset data
# write.table(subData, "sub_data.txt") 

# Mission 5: From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for 
# each activity and each subject.

# 5.1 Import reshape2 library
library(reshape2)

# 5.2 Create a new data set and melt the data
secondData <- subData
dataMelt <- melt(secondData, id = c("subject", "activity"))

# 5.3 Reshape the data
avgData <- dcast(dataMelt, subject + activity ~ variable, mean)
write.table(avgData, "average_data.txt") 



