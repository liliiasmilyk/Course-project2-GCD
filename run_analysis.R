if (!file.exists ("c:/course/gettingANDcleaningDATA/course_project2")) {
        dir.create("c:/course/gettingANDcleaningDATA/course_project2")
}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "c:/course/gettingANDcleaningDATA/course_project2/project2.zip")
unzip(zipfile = "project2.zip", exdir = "c:/course/gettingANDcleaningDATA/course_project2")

###1. Merges the training and the test sets to create one data set.
#reading test set
setwd("c:/course/gettingANDcleaningDATA/course_project2/UCI_HAR_Dataset/test")
testlistx <- list.files(pattern = "X_test.txt")
testdfx <- read.table(file = testlistx, header = FALSE)
testlisty <- list.files(pattern = "y_test.txt")
testdf1y <- read.table(file = testlisty, header = FALSE)
testlistsub <- list.files(pattern = "subject_test.txt")
testdf1sub <- read.table(file = testlistsub, header = FALSE)
library(dplyr)
testdfy <- rename(testdf1y, activity = V1)
testdfsub <- rename(testdf1sub, subject = V1)
testdf1 <- cbind(testdfy, testdfx)
testdf <- cbind(testdfsub, testdf1)

#reading train set
setwd("c:/course/gettingANDcleaningDATA/course_project2/UCI_HAR_Dataset/train")
trainlistx <- list.files(pattern = "X_train.txt")
traindfx <- read.table(file = trainlistx, header = FALSE)
trainlisty <- list.files(pattern = "y_train.txt")
traindf1y <- read.table(file = trainlisty, header = FALSE)
trainlistsub <- list.files(pattern = "subject_train.txt")
traindf1sub <- read.table(file = trainlistsub, header = FALSE)
library(dplyr)
traindfy <- rename(traindf1y, activity = V1)
traindfsub <- rename(traindf1sub, subject = V1)
traindf1 <- cbind(traindfy, traindfx)
traindf <- cbind(traindfsub, traindf1)

#merging train and test datasets
df1 <- rbind(testdf, traindf)
setwd("c:/course/gettingANDcleaningDATA/course_project2/UCI_HAR_Dataset")
feat <- list.files(pattern = "features.txt")
feature <- read.table(file = feat, header = FALSE)
names <- c("subject", "activity", as.character(feature$V2))
colnames(df1) <- names
dim(df1)
head(df1[,1:6])


###2. Extracts only the measurements on the mean and standard deviation for each measurement.

#there are columns with duplicated names. need to make it unique.
names_freq <- as.data.frame(table(names(df1)))
names_freq[names_freq$Freq > 1, ]
colnames(df1) <- make.unique(colnames(df1))

#extract only mean and std
m <- grep("mean()", colnames(df1))
s <- grep("std()", colnames(df1))
library(dplyr)
df2 <- select(df1, subject, activity, m, s)
colnames(df2)


###3. Uses descriptive activity names to name the activities in the data set.
#reading activity labels from file
setwd("c:/course/gettingANDcleaningDATA/course_project2/UCI_HAR_Dataset")
act <- list.files(pattern = "activity_labels.txt")
active <- read.table(file = act, header = FALSE)

#change numbers in activity column to descriptive names
library(dplyr)
df1 <- arrange(df1, activity)
for (i in 1:6) {
        df1$activity[df1$activity == i] = as.character(active$V2[active$V1 == i])
}
head(df1[1:6,1:4])
tail(df1[,1:4], n = 6)
unique(df1$activity)

###4. Appropriately labels the data set with descriptive variable names.
df4 <- df1
#change only first abbreviated letter and dashes
colnames(df4) <- sub("^t", "time", colnames(df4))
colnames(df4) <- sub("^f", "frequency", colnames(df4))
colnames(df4) <- gsub("-", ".", colnames(df4))

###5. From the data set in step 4, creates a second, independent tidy data set with the average of 
###   each variable for each activity and each subject.
tidydata <- df4 %>% 
        group_by(subject, activity) %>%
        summarise_all(mean)
dim(tidydata)
head(tidydata[1:10, 1:6])
tail(tidydata[,1:6], n = 10)

#saving tidy dataset to text file
write.table(tidydata, "tidy_data.txt", row.name = FALSE)
