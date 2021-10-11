# load libraries
library(data.table)
library(dplyr)

# set directory
setwd("UCI HAR Dataset")

#Assigning data frames:
# read train data 
subjectTrain <- read.table("./train/subject_train.txt")
activityTrain <- read.table("./train/y_train.txt")
featuresTrain <- read.table("./train/X_train.txt")

# read test data 
subjectTest <- read.table("./test/subject_test.txt")
activityTest <- read.table("./test/y_test.txt")
featuresTest <- read.table("./test/X_test.txt")

# read features description 
featureNames <- read.table("./features.txt")

# read activity labels 
activityLabels <- read.table("./activity_labels.txt")

# merge of training and test sets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#name the columns
colnames(features) <- t(featureNames[2])

#merge the datasets
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Extracts only the the mean and standard deviation
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*|.*Activity.*|.*Subject.*", names(completeData), ignore.case=TRUE)

# ... and keep data in these columns only
extractedData <- completeData[, columnsWithMeanSTD]

#Use descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity) 
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

#Factor the activity variable once activity names are updated
extractedData$Activity <- as.factor(extractedData$Activity)

#Appropriately label the data set with descriptive variable names

#Get Column names
names(extractedData)

#Expand Acronyms
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

# Create a second, independent tidy set with the average of each variable for each 
# activity and each subject

#group by subject and activity and summarise using mean
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# create output file "Tidy.txt"
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

