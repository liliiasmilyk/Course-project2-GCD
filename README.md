Course project 2, course “Getting and cleaning data”
====================================================

A goal of this project is to prepare a tidy data set that can be used
for later analysis. Data for project are data collected from the
accelerometers from the Samsung Galaxy S smartphone. This dataset
contains data from experiment - a group of 30 volunteers within an age
from 19 to 48 years performed in six activities (walking, walking
upstairs, walking downstairs, sitting, standing, laying) wearing a
smartphone (Samsung Galaxy S II) on the waist. The obtained dataset was
randomly partitioned into two sets - training set(70% of original data)
and test set (30% of original data). For each observation it is
provided:  
- an identifier of the subject who carried out the experiment(from 1 to
30)  
- its activity label (from 1 to 6)  
- a 561-feature vector with time and frequency domain variables.

To get the tidy data it is required to make 5 steps. But first thing
first - we need to create a working directory and download files.

    if (!file.exists ("c:/course/gettingANDcleaningDATA/course_project2")) {  
            dir.create("c:/course/gettingANDcleaningDATA/course_project2")  
    }  
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
    download.file(fileURL, destfile = "c:/course/gettingANDcleaningDATA/course_project2/project2.zip")  
    unzip(zipfile = "project2.zip", exdir = "c:/course/gettingANDcleaningDATA/course_project2")

Now we can work to get tidy data.

**Step 1. Merges the training and the test sets to create one data
set.**  
Train and test data sets have severals files each that need to be merged
before binding these two sets.  
For reading test dataset:

    setwd("c:/course/gettingANDcleaningDATA/course_project2/UCI_HAR_Dataset/test")  
    testlistx <- list.files(pattern = "X_test.txt")  
    testdfx <- read.table(file = testlistx, header = FALSE)  
    testlisty <- list.files(pattern = "y_test.txt")  
    testdf1y <- read.table(file = testlisty, header = FALSE)  
    testlistsub <- list.files(pattern = "subject_test.txt")  
    testdf1sub <- read.table(file = testlistsub, header = FALSE)  
    library(dplyr)  

    ## Warning: package 'dplyr' was built under R version 3.6.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    testdfy <- rename(testdf1y, activity = V1)  
    testdfsub <- rename(testdf1sub, subject = V1)  
    testdf1 <- cbind(testdfy, testdfx)  
    testdf <- cbind(testdfsub, testdf1)  

For reading train dataset:

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

Now we can merge two datasets to one.

    df1 <- rbind(testdf, traindf)  
    setwd("c:/course/gettingANDcleaningDATA/course_project2/UCI_HAR_Dataset")  
    feat <- list.files(pattern = "features.txt")  
    feature <- read.table(file = feat, header = FALSE)  
    names <- c("subject", "activity", as.character(feature$V2))  
    colnames(df1) <- names  
    dim(df1)  

    ## [1] 10299   563

    head(df1[,1:6])  

    ##   subject activity tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
    ## 1       2        5         0.2571778       -0.02328523       -0.01465376
    ## 2       2        5         0.2860267       -0.01316336       -0.11908252
    ## 3       2        5         0.2754848       -0.02605042       -0.11815167
    ## 4       2        5         0.2702982       -0.03261387       -0.11752018
    ## 5       2        5         0.2748330       -0.02784779       -0.12952716
    ## 6       2        5         0.2792199       -0.01862040       -0.11390197
    ##   tBodyAcc-std()-X
    ## 1       -0.9384040
    ## 2       -0.9754147
    ## 3       -0.9938190
    ## 4       -0.9947428
    ## 5       -0.9938525
    ## 6       -0.9944552

Now we have one dataset that contains subject, activity label, 561
variable for 10299 observations.

**Step2. Extracts only the measurements on the mean and standard
deviation for each measurement.**  
Columns should have unique names so we can make manipulations with
them.  
First, we need to get rid of duplicated column names.

    names_freq <- as.data.frame(table(names(df1)))  
    names_freq[names_freq$Freq > 1, ]  

    ##                                 Var1 Freq
    ## 9        fBodyAcc-bandsEnergy()-1,16    3
    ## 10       fBodyAcc-bandsEnergy()-1,24    3
    ## 11        fBodyAcc-bandsEnergy()-1,8    3
    ## 12      fBodyAcc-bandsEnergy()-17,24    3
    ## 13      fBodyAcc-bandsEnergy()-17,32    3
    ## 14      fBodyAcc-bandsEnergy()-25,32    3
    ## 15      fBodyAcc-bandsEnergy()-25,48    3
    ## 16      fBodyAcc-bandsEnergy()-33,40    3
    ## 17      fBodyAcc-bandsEnergy()-33,48    3
    ## 18      fBodyAcc-bandsEnergy()-41,48    3
    ## 19      fBodyAcc-bandsEnergy()-49,56    3
    ## 20      fBodyAcc-bandsEnergy()-49,64    3
    ## 21      fBodyAcc-bandsEnergy()-57,64    3
    ## 22       fBodyAcc-bandsEnergy()-9,16    3
    ## 60   fBodyAccJerk-bandsEnergy()-1,16    3
    ## 61   fBodyAccJerk-bandsEnergy()-1,24    3
    ## 62    fBodyAccJerk-bandsEnergy()-1,8    3
    ## 63  fBodyAccJerk-bandsEnergy()-17,24    3
    ## 64  fBodyAccJerk-bandsEnergy()-17,32    3
    ## 65  fBodyAccJerk-bandsEnergy()-25,32    3
    ## 66  fBodyAccJerk-bandsEnergy()-25,48    3
    ## 67  fBodyAccJerk-bandsEnergy()-33,40    3
    ## 68  fBodyAccJerk-bandsEnergy()-33,48    3
    ## 69  fBodyAccJerk-bandsEnergy()-41,48    3
    ## 70  fBodyAccJerk-bandsEnergy()-49,56    3
    ## 71  fBodyAccJerk-bandsEnergy()-49,64    3
    ## 72  fBodyAccJerk-bandsEnergy()-57,64    3
    ## 73   fBodyAccJerk-bandsEnergy()-9,16    3
    ## 163     fBodyGyro-bandsEnergy()-1,16    3
    ## 164     fBodyGyro-bandsEnergy()-1,24    3
    ## 165      fBodyGyro-bandsEnergy()-1,8    3
    ## 166    fBodyGyro-bandsEnergy()-17,24    3
    ## 167    fBodyGyro-bandsEnergy()-17,32    3
    ## 168    fBodyGyro-bandsEnergy()-25,32    3
    ## 169    fBodyGyro-bandsEnergy()-25,48    3
    ## 170    fBodyGyro-bandsEnergy()-33,40    3
    ## 171    fBodyGyro-bandsEnergy()-33,48    3
    ## 172    fBodyGyro-bandsEnergy()-41,48    3
    ## 173    fBodyGyro-bandsEnergy()-49,56    3
    ## 174    fBodyGyro-bandsEnergy()-49,64    3
    ## 175    fBodyGyro-bandsEnergy()-57,64    3
    ## 176     fBodyGyro-bandsEnergy()-9,16    3

    colnames(df1) <- make.unique(colnames(df1))  

It is time to extract measurements on mean and standart deviation for
each measurement.

    m <- grep("mean()", colnames(df1))  
    s <- grep("std()", colnames(df1))  
    library(dplyr)  
    df2 <- select(df1, subject, activity, m, s)  
    colnames(df2)  

    ##  [1] "subject"                         "activity"                       
    ##  [3] "tBodyAcc-mean()-X"               "tBodyAcc-mean()-Y"              
    ##  [5] "tBodyAcc-mean()-Z"               "tGravityAcc-mean()-X"           
    ##  [7] "tGravityAcc-mean()-Y"            "tGravityAcc-mean()-Z"           
    ##  [9] "tBodyAccJerk-mean()-X"           "tBodyAccJerk-mean()-Y"          
    ## [11] "tBodyAccJerk-mean()-Z"           "tBodyGyro-mean()-X"             
    ## [13] "tBodyGyro-mean()-Y"              "tBodyGyro-mean()-Z"             
    ## [15] "tBodyGyroJerk-mean()-X"          "tBodyGyroJerk-mean()-Y"         
    ## [17] "tBodyGyroJerk-mean()-Z"          "tBodyAccMag-mean()"             
    ## [19] "tGravityAccMag-mean()"           "tBodyAccJerkMag-mean()"         
    ## [21] "tBodyGyroMag-mean()"             "tBodyGyroJerkMag-mean()"        
    ## [23] "fBodyAcc-mean()-X"               "fBodyAcc-mean()-Y"              
    ## [25] "fBodyAcc-mean()-Z"               "fBodyAcc-meanFreq()-X"          
    ## [27] "fBodyAcc-meanFreq()-Y"           "fBodyAcc-meanFreq()-Z"          
    ## [29] "fBodyAccJerk-mean()-X"           "fBodyAccJerk-mean()-Y"          
    ## [31] "fBodyAccJerk-mean()-Z"           "fBodyAccJerk-meanFreq()-X"      
    ## [33] "fBodyAccJerk-meanFreq()-Y"       "fBodyAccJerk-meanFreq()-Z"      
    ## [35] "fBodyGyro-mean()-X"              "fBodyGyro-mean()-Y"             
    ## [37] "fBodyGyro-mean()-Z"              "fBodyGyro-meanFreq()-X"         
    ## [39] "fBodyGyro-meanFreq()-Y"          "fBodyGyro-meanFreq()-Z"         
    ## [41] "fBodyAccMag-mean()"              "fBodyAccMag-meanFreq()"         
    ## [43] "fBodyBodyAccJerkMag-mean()"      "fBodyBodyAccJerkMag-meanFreq()" 
    ## [45] "fBodyBodyGyroMag-mean()"         "fBodyBodyGyroMag-meanFreq()"    
    ## [47] "fBodyBodyGyroJerkMag-mean()"     "fBodyBodyGyroJerkMag-meanFreq()"
    ## [49] "tBodyAcc-std()-X"                "tBodyAcc-std()-Y"               
    ## [51] "tBodyAcc-std()-Z"                "tGravityAcc-std()-X"            
    ## [53] "tGravityAcc-std()-Y"             "tGravityAcc-std()-Z"            
    ## [55] "tBodyAccJerk-std()-X"            "tBodyAccJerk-std()-Y"           
    ## [57] "tBodyAccJerk-std()-Z"            "tBodyGyro-std()-X"              
    ## [59] "tBodyGyro-std()-Y"               "tBodyGyro-std()-Z"              
    ## [61] "tBodyGyroJerk-std()-X"           "tBodyGyroJerk-std()-Y"          
    ## [63] "tBodyGyroJerk-std()-Z"           "tBodyAccMag-std()"              
    ## [65] "tGravityAccMag-std()"            "tBodyAccJerkMag-std()"          
    ## [67] "tBodyGyroMag-std()"              "tBodyGyroJerkMag-std()"         
    ## [69] "fBodyAcc-std()-X"                "fBodyAcc-std()-Y"               
    ## [71] "fBodyAcc-std()-Z"                "fBodyAccJerk-std()-X"           
    ## [73] "fBodyAccJerk-std()-Y"            "fBodyAccJerk-std()-Z"           
    ## [75] "fBodyGyro-std()-X"               "fBodyGyro-std()-Y"              
    ## [77] "fBodyGyro-std()-Z"               "fBodyAccMag-std()"              
    ## [79] "fBodyBodyAccJerkMag-std()"       "fBodyBodyGyroMag-std()"         
    ## [81] "fBodyBodyGyroJerkMag-std()"

As we can see, df2 contains only measurements on the mean and standart
deviation for each measurement with identifier (subject and activity).

**3. Uses descriptive activity names to name the activities in the data
set.**  
First, we need to download file that contains descriptive activity
labels.

    setwd("c:/course/gettingANDcleaningDATA/course_project2/UCI_HAR_Dataset")  
    act <- list.files(pattern = "activity_labels.txt")  
    active <- read.table(file = act, header = FALSE)  

Using dplyr package to change activity labels to descriptive labels.

    library(dplyr)  
    df1 <- arrange(df1, activity)  
    for (i in 1:6) {  
            df1$activity[df1$activity == i] = as.character(active$V2[active$V1 == i])  
    }  
    head(df1[1:6,1:4])  

    ##   subject activity tBodyAcc-mean()-X tBodyAcc-mean()-Y
    ## 1       2  WALKING         0.2039624      -0.032344520
    ## 2       2  WALKING         0.2493432      -0.003410981
    ## 3       2  WALKING         0.3250505      -0.029803725
    ## 4       2  WALKING         0.3088081      -0.022128064
    ## 5       2  WALKING         0.2655992      -0.015936305
    ## 6       2  WALKING         0.1874192      -0.011533248

    tail(df1[,1:4], n = 6)  

    ##       subject activity tBodyAcc-mean()-X tBodyAcc-mean()-Y
    ## 10294      30   LAYING         0.2793965       -0.01806404
    ## 10295      30   LAYING         0.2891902       -0.02162851
    ## 10296      30   LAYING         0.2888633       -0.02142656
    ## 10297      30   LAYING         0.2824093       -0.01805115
    ## 10298      30   LAYING         0.2827711       -0.01604002
    ## 10299      30   LAYING         0.2766110       -0.01721851

    unique(df1$activity)  

    ## [1] "WALKING"            "WALKING_UPSTAIRS"   "WALKING_DOWNSTAIRS"
    ## [4] "SITTING"            "STANDING"           "LAYING"

As we can see, modified df1 has descriptive activity labels for each of
6 activity.

**4. Appropriately labels the data set with descriptive variable
names.**  
To make column names descriptive, we can use sub() and gsub() functions.

    df4 <- df1  
    colnames(df4) <- sub("^t", "time", colnames(df4))  
    colnames(df4) <- sub("^f", "frequency", colnames(df4))  
    colnames(df4) <- gsub("-", ".", colnames(df4))  

Changed only first abbreviated letter and dashes in column names.  
Im my opinion, it is better to use abbreviated names for columns and
codebook if variables have long names. It is difficult and takes time to
write long column names during analysis.

**5. From the data set in step 4, creates a second, independent tidy
data set with the average of each variable for each activity and each
subject.**  
Finally, to get tidy data set, with help of dplyr package, we need to
group data by each subject and group it by each activity and then
calculate mean for each variable.

    tidydata <- df4 %>%   
            group_by(subject, activity) %>%  
            summarise_all(mean)  
    dim(tidydata)  

    ## [1] 180 563

    head(tidydata[1:10, 1:6])  

    ## # A tibble: 6 x 6
    ## # Groups:   subject [1]
    ##   subject activity `timeBodyAcc.me~ `timeBodyAcc.me~ `timeBodyAcc.me~
    ##     <int> <chr>               <dbl>            <dbl>            <dbl>
    ## 1       1 LAYING              0.222         -0.0405           -0.113 
    ## 2       1 SITTING             0.261         -0.00131          -0.105 
    ## 3       1 STANDING            0.279         -0.0161           -0.111 
    ## 4       1 WALKING             0.277         -0.0174           -0.111 
    ## 5       1 WALKING~            0.289         -0.00992          -0.108 
    ## 6       1 WALKING~            0.255         -0.0240           -0.0973
    ## # ... with 1 more variable: `timeBodyAcc.std().X` <dbl>

    tail(tidydata[,1:6], n = 10)  

    ## # A tibble: 10 x 6
    ## # Groups:   subject [2]
    ##    subject activity `timeBodyAcc.me~ `timeBodyAcc.me~ `timeBodyAcc.me~
    ##      <int> <chr>               <dbl>            <dbl>            <dbl>
    ##  1      29 STANDING            0.278         -0.0173           -0.109 
    ##  2      29 WALKING             0.272         -0.0163           -0.107 
    ##  3      29 WALKING~            0.293         -0.0149           -0.0981
    ##  4      29 WALKING~            0.265         -0.0299           -0.118 
    ##  5      30 LAYING              0.281         -0.0194           -0.104 
    ##  6      30 SITTING             0.268         -0.00805          -0.0995
    ##  7      30 STANDING            0.277         -0.0170           -0.109 
    ##  8      30 WALKING             0.276         -0.0176           -0.0986
    ##  9      30 WALKING~            0.283         -0.0174           -0.1000
    ## 10      30 WALKING~            0.271         -0.0253           -0.125 
    ## # ... with 1 more variable: `timeBodyAcc.std().X` <dbl>

As we can see, tidy data set contains 180 observation (for each pair
subject-activity) and 563 variable. Tidy data presents the average of
each variable for each activity and each subject.  
And now we can save final dataset as text file.

    write.table(tidydata, "tidy_data.txt", row.name = FALSE)  

It is saved in working directory and ready for later analysis.
