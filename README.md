---
title: "README.md"
output: html_document
---
## Getting and Cleaning Data - Course Project ##
***
This is the course project for the Getting and Cleaning Data Coursera course.

The data for the project comes from:
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

##### The R script, `run_analysis.R`, does the following: #####
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The `CodeBook.md` contains the description of the data.

The final results are shown in `average_data.txt`