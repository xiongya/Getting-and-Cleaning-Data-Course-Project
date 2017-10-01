
setwd("~/Desktop/Project")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "Dataset.zip")
unzip(zipfile = "Dataset.zip")

# read training data:
x.train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("./UCI HAR Dataset/train/y_train.txt")
s.train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# read test data:
x.test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y.test <- read.table("./UCI HAR Dataset/test/y_test.txt")
s.test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# read names
features <- read.table("./UCI HAR Dataset/features.txt")
names(y.train) <- "activity"; names(y.test) <- "activity"
names(s.train) <- "subject"; names(s.test) <- "subject"
names(x.train) <- features[,2]; names(x.test) <- features[,2]

# Merge the training and the test sets to one data set
train <- cbind(y.train, s.train, x.train)
test <- cbind(y.test, s.test, x.test)
data.all <- rbind(train, test)

# Extract only the measurements on the mean and standard deviation for each measurement.
extract.names <- features[,2][grep("mean\\(\\)|std\\(\\)",features[,2])]
selection <- c(as.character(extract.names), "subject", "activity")
data.sub <- subset(data.all, select=selection)

# Uses descriptive activity names to name the activities in the data set.
labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
data.sub$activity <- factor(data.sub$activity, levels = labels[,1], labels = labels[,2])

# Appropriately labels the data set with descriptive variable names.
data.names <- names(data.sub)
# Replace prefix "t", "f", "Acc" to "time", "frequency", "Accelerometer"; replace "Gyro", "Mag" to "Gyroscope", "Magnitude"
# prefix
data.names <- gsub("^t", "time", data.names)
data.names <- gsub("^f", "frequency", data.names)
data.names <- gsub("^Acc", "Accelerometer", data.names)
# full name
data.names <- gsub("Gyro", "Gyroscope", data.names)
data.names <- gsub("Mag", "Magnitude", data.names)
# Take off duplicate words
data.names <- gsub("BodyBody", "Body", data.names)
# Assign new names
names(data.sub) <- data.names

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)
data.sub2 <- aggregate(. ~ subject + activity, data.sub, mean)
data.sub2 <- data.sub2[order(data.sub2$subject, data.sub2$activity),]
write.table(data.sub2, "tidy.txt", row.name = FALSE)

