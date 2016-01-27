library(dplyr)
library(plyr)
feature <- read.table('./data/features.txt')
activity <- read.table('./data/activity_labels.txt')
colnames(activity) <- c('activity_label', 'activity')
x_test <- read.table('./data/test/x_test.txt')
y_test <- read.table('./data/test/y_test.txt')
subject_test <- read.table('./data/test/subject_test.txt')
colnames(y_test) <- 'activity_label'
colnames(subject_test) <- 'subject'
## Appropriately labels the data set with descriptive variable names.
colnames(x_test) <- feature$V2

x_train <- read.table('./data/train/x_train.txt')
y_train <- read.table('./data/train/y_train.txt')
subject_train <- read.table('./data/train/subject_train.txt')
colnames(y_train) <- 'activity_label'
colnames(subject_train) <- 'subject'
## Appropriately labels the data set with descriptive variable names.
colnames(x_train) <- feature$V2


test <- cbind(subject_test,y_test, x_test)
train <- cbind(subject_train,y_train, x_train)

## Merges the training and the test sets to create one data set.
data <- rbind(train,test)

## Uses descriptive activity names to name the activities in the data set
data <- merge(activity,data, by.x = 'activity_label', by.y = 'activity_label' )

## Extracts only the measurements on the mean and standard deviation for each measurement.
data <- data[,c('activity', 'subject', grep('mean', colnames(data), value = TRUE), grep('std',colnames(data), value = TRUE))]

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data2 <- group_by(data, activity, subject)
data2 <- summarise_each(data2, funs(mean))

##codebook <- data.frame(colnames(data2))
##write.table(codebook, file = 'codebook.txt', row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(data2, file = 'tidyData.txt', row.names = FALSE)
