##1.Merges the training and the test sets to create one data set
#Load libraries
library(dplyr)
library(data.table)
#download the dataset
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"
#check if archieve already exists
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
} 
#check if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}
#read data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
#merge 
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
S <- rbind(subject_train, subject_test)
merged_data <- cbind(S, Y, X)

##2.Extracts only the measurements on the mean and standard deviation for each measurement
tidyData <- merged_data %>% select(subject, code, contains("mean"), contains("std"))

##3.Uses descriptive activity names to name the activities in the data set
tidyData$code <- activities[tidyData$code, 2]

##4.Appropriately labels the data set with descriptive variable names
names(tidyData)[2] = "activity"
names(tidyData)<-gsub("Acc", "Accelerometer", names(tidyData))
names(tidyData)<-gsub("Gyro", "Gyroscope", names(tidyData))
names(tidyData)<-gsub("BodyBody", "Body", names(tidyData))
names(tidyData)<-gsub("Mag", "Magnitude", names(tidyData))
names(tidyData)<-gsub("^t", "Time", names(tidyData))
names(tidyData)<-gsub("^f", "Frequency", names(tidyData))
names(tidyData)<-gsub("tBody", "TimeBody", names(tidyData))
names(tidyData)<-gsub("-mean()", "Mean", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("-std()", "STD", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("-freq()", "Frequency", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("angle", "Angle", names(tidyData))
names(tidyData)<-gsub("gravity", "Gravity", names(tidyData))

##5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject
finalData <- tidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(finalData, "finalData.txt", row.name=FALSE)
str(finalData)
finalData

###code book
install.packages("codebook")
library(codebook)
new_codebook_rmd()

##export
?write.table
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
