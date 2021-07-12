#loading corresponding package
library(data.table)
#Checking if archive exists & downloading the dataset 
if (!file.exists('./UCI HAR Dataset.zip')){
      download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
      unzip("UCI HAR Dataset.zip", exdir = getwd())
}

#assigning data frames
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

data_train_x <- read.table('./UCI HAR Dataset/train/X_train.txt')
data_train_activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
data_train_subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data_train <-  data.frame(data_train_subject, data_train_activity, data_train_x)
names(data_train) <- c(c('subject', 'activity'), features)

data_test_x <- read.table('./UCI HAR Dataset/test/X_test.txt')
data_test_activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
data_test_subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

data_test <-  data.frame(data_test_subject, data_test_activity, data_test_x)
names(data_test) <- c(c('subject', 'activity'), features)

#merging the Training and Testing Sets into 1 data set called data_all
data_all <- rbind(data_train, data_test)

#extracting only the measurements on the mean and standard deviation 
#for each measurement.
mean_std_select <- grep('mean|std', features)
data_sub <- data_all[,c(1,2,mean_std.select + 2)]

#Using descriptive activity names to name the activities in the data set
#by reading the labels from the activity_labels.txt file
activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_labels <- as.character(activity_labels[,2])
data_sub$activity <- activity_labels[data_sub$activity]

#Appropriately labels the data set with descriptive variable names
#by replacing the names in data set with names from activity labels
name_new <- names(data_sub)
name_new <- gsub("[(][)]", "", name_new)
name_new <- gsub("^t", "TimeDomain_", name_new)
name_new <- gsub("^f", "FrequencyDomain_", name_new)
name_new <- gsub("Acc", "Accelerometer", name_new)
name_new <- gsub("Gyro", "Gyroscope", name_new)
name_new <- gsub("Mag", "Magnitude", name_new)
name_new <- gsub("-mean-", "_Mean_", name_new)
name_new <- gsub("-std-", "_StandardDeviation_", name_new)
name_new <- gsub("-", "_", name_new)
names(data_sub) <- name_new

#From the data set in step 4, we'll create a second, 
# independent tidy data set with the average of each variable
# for each activity and each subject 
# by tidying data as output named data_tidy.txt file
data_tidy <- aggregate(data.sub[,3:81], by = list(activity = data_sub$activity, subject = data_sub$subject),FUN = mean)
write.table(x = data_tidy, file = "data_tidy.txt", row.names = FALSE)







