#Libraries
library(data.table)
library(tidyverse)
#Read data
features <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/features.txt", col.names = c("n","fun"))
activities <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

subtest <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/test/subject_test.txt", col.names = "subj")
x_test <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/test/X_test.txt", col.names = features$fun)
y_test <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/test/y_test.txt", col.names = "code")

subtrain <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/train/subject_train.txt", col.names = "subj")
x_train <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/train/X_train.txt", col.names = features$fun)
y_train <- read.table("/Users/siyi/Coursera/w4_proj/w4_p/UCI HAR Dataset/train/y_train.txt", col.names = "code")

#1.Combine datasets into a single one
x_set <- rbind(x_test, x_train)
y_set <- rbind(y_test, y_train)
sub_set <- rbind(subtest, subtrain)
dim(x_set)
dim(y_set)
dim(sub_set)

#2.Extract only the measurements on the mean and standard deviation for each measurement
mean_set <- x_set[, grepl("mean()", names(x_set))]
std_set <- x_set[, grepl("std()", names(x_set))]
fin_set <- cbind(mean_set, std_set)
dim(fin_set)

#3.Use descriptive activity names to name the activities in the data set
y_set$code <- activities[y_set$code, 2]

#4.Appropriately labels the data set with descriptive variable names
names(y_set) = "activity"
merge_set <- cbind(fin_set, y_set, sub_set)
names(merge_set)<-gsub("Acc", "Accelerometer", names(merge_set))
names(merge_set)<-gsub("Gyro", "Gyroscope", names(merge_set))
names(merge_set)<-gsub("BodyBody", "Body", names(merge_set))
names(merge_set)<-gsub("Mag", "Magnitude", names(merge_set))
names(merge_set)<-gsub("^t", "Time", names(merge_set))
names(merge_set)<-gsub("^f", "Frequency", names(merge_set))
names(merge_set)<-gsub("tBody", "TimeBody", names(merge_set))
names(merge_set)<-gsub("-mean()", "Mean", names(merge_set), ignore.case = TRUE)
names(merge_set)<-gsub("-std()", "STD", names(merge_set), ignore.case = TRUE)
names(merge_set)<-gsub("-freq()", "Frequency", names(merge_set), ignore.case = TRUE)
names(merge_set)<-gsub("angle", "Angle", names(merge_set))
names(merge_set)<-gsub("gravity", "Gravity", names(merge_set))

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

final_set<-aggregate(. ~subj + activity, merge_set, mean)
final_set <- final_set[order(final_set$subj,final_set$activity),]

write.table(final_set, file = "tidydata.txt",row.name=FALSE)



