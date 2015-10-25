## 1.Get & List the files
data_path <- file.path(getwd() , "UCI HAR Dataset")
files<-list.files(data_path, recursive=TRUE)
files

## 2.Read the files

### 2.1 Activity files
readActivityTest  <- read.table(file.path(data_path, "test" , "Y_test.txt" ),header = FALSE)
readActivityTrain <- read.table(file.path(data_path, "train", "Y_train.txt"),header = FALSE)

### 2.2 Subject files
readSubjectTest  <- read.table(file.path(data_path, "test" , "subject_test.txt"),header = FALSE)
readSubjectTrain <- read.table(file.path(data_path, "train", "subject_train.txt"),header = FALSE)

### 2.3 Fearures files
readFeaturesTest  <- read.table(file.path(data_path, "test" , "X_test.txt" ),header = FALSE)
readFeaturesTrain <- read.table(file.path(data_path, "train", "X_train.txt"),header = FALSE)

### 2.4 Names of Varibles
readFeaturesNames <- read.table(file.path(data_path, "features.txt"),head=FALSE)

### 2.5 levels of Varibles
readactivityLabels <- read.table(file.path(data_path, "activity_labels.txt"),header = FALSE)


## 3.Merges the training and the test sets to create one data set.

### 3.1 Combine by rows
mergeActivity <- rbind(readActivityTest, readActivityTrain)
mergeSubject <- rbind(readSubjectTest, readSubjectTrain)
mergeFeatures <- rbind(readFeaturesTest, readFeaturesTrain)

### 3.2 Set names
names(mergeActivity)<- c("activity")
names(mergeSubject)<-c("subject")
names(mergeFeatures)<- readFeaturesNames$V2

### 3.3 Combine by columns
mergedata <- cbind(mergeSubject, mergeActivity)
data <- cbind(mergeFeatures, mergedata)

## 4.Extracts only the measurements on the mean and standard deviation for each measurement.

### 4.1 Select the measurements by grep FeaturesNames with mean and std
subsetFeaturesNames<-readFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", readFeaturesNames$V2)]

### 4.2 Create a subset by selected featuresnames
selecteddata<-c(as.character(subsetFeaturesNames), "subject", "activity" )
data<-subset(data,select=selecteddata)

## 5.Uses descriptive activity names to name the activities in the data set

  SetActivityNames = function(df){
    
  activity.Labels = readactivityLabels

  activity.ID = 1
  for (ActivityLabel in activity.Labels$V2) {
    df$activity <- gsub(activity.ID, ActivityLabel, df$activity)
    activity.ID <- activity.ID + 1
  }
  
  df
}

data <- SetActivityNames(data)

## 6.Appropriately labels the data set with descriptive variable names.

###  t  > time
### Acc > Accelerometer
### Gyro > Gyroscope
###  f > frequency
### Mag > Magnitude
### BodyBody > Body

names(data)<-gsub("^t", "time", names(data))
names(data)<-gsub("^f", "frequency", names(data))
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))

## 7.From the data set in step 4, creates a second, independent tidy data 
##   set with the average of each variable for each activity and each subject.

library(plyr)
tidydata<-aggregate(data[,(1:66)], by=list(activity = data$activity, subject=data$subject), mean, na.rm=TRUE)
write.table(tidydata, file = "tidydata.txt",row.name=FALSE)

## 8. Create CodeBook

library("memisc", lib.loc="~/R/win-library/3.2")
codebook(tidydata)

