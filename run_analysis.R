#Course Project: Tidying the Data

#Part 1: Here I'm downloading the file and unzipping it to extract the 
#files/folders inside. I'm downloading it as binary since zip file. 
#Finally I'm mergeing the training and test sets to create one data set

filename <- "Smartphones_Dataset.zip"
if (!file.exists(filename)) {
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                filename, mode = "wb")
  unzip(filename)
}

#read the data sets starting with features and activity labels
features      <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)

#read the training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_train       <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train       <- read.table("UCI HAR Dataset/train/y_train.txt")

#read the test data
subject_test  <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_test        <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test        <- read.table("UCI HAR Dataset/test/y_test.txt")

#I'm combining the training and test data sets across subject, train
#and test data sets
subjects <- rbind(subject_train, subject_test)
activities <- rbind(y_train, y_test)
measurements <- rbind(x_train, x_test)

#assign column names
colnames(measurements) <- features
colnames(subjects) <- "subject"
colnames(activities) <- "activity"

#Part 2: I'm extracting only the measurements on the mean and standard  
#deviation for each measurement from the whole data set
wanted_columns <- grepl("mean\\(\\)|std\\(\\)", features)
measurements <- measurements[, wanted_columns]

#Part 3: I'm replacing column names by descriptive activity names
activities$activity <- factor(activities$activity,
                              levels = activity_labels[,1],
                              labels = activity_labels[,2])

#Part 4: I'm using regular expressions for further clean up. This portion
#will appropriately label the data set with descriptive variable names
#the feature names have brackets (). Removing for better readability
readable_names <- colnames(measurements)
readable_names <- gsub("^t", "time", readable_names)
readable_names <- gsub("^f", "frequency", readable_names)
readable_names <- gsub("Acc", "acceleration", readable_names)
readable_names <- gsub("Gyro", "gyroscope", readable_names)
readable_names <- gsub("Mag", "magnitude", readable_names)
readable_names <- gsub("BodyBody", "body", readable_names)
readable_names <- gsub("[()]", "", readable_names)
colnames(measurements) <- tolower(readable_names)

#Part 5: I'm Column Combining all of the data to create ONE independent
#data set with the average of each variable for each activity and 
#each subject

tidy_dataset<-cbind(subjects, activities, measurements)

library(dplyr)
summary_data <- tidy_dataset %>%
  group_by(subject, activity) %>%
  summarise_all(mean) %>%
  ungroup()  

# Write the tidy dataset to a txt file (to be uploaded to github repo)
write.table(tidy_dataset, "tidy_dataset.txt", row.names = FALSE)

