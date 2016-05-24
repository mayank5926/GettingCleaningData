library(dplyr)

## ================= Task 1 Starts Here ===============================
## TASK 1: Merges the training and the test sets to create one data set.

## Part A. Read training subjects' data

## Read subjects' training data from file.
trainSubjects=read.table("UCI HAR Dataset/train/subject_train.txt")
## Read number of rows in this data. Number of rows in this data should be
## same as rows in training feature data and activities data. 
numTrainingSubjectsReadings<-dim(trainSubjects)[1]
## Read number of unique subjects. This should not be more than 30 (given in
## assignment).
numUniqueTrainingSubjects<-dim(unique(trainSubjects))[1]



## Part B. Read training activities' data.

## Read training activities' data from file.
trainActivities=read.table("UCI HAR Dataset/train/y_train.txt")
## Read number of rows in this data. Number of rows in this data should be
## same as rows in training feature data and subjects data.
numTrainingActivitiesReadings<-dim(trainActivities)[1]
## Read number of unique training activities. This should not be more than
## 6 (given in assignment).
numUniqueTrainingActivities<-dim(unique(trainActivities))[1]



## Part C. Read training feature set

## Read training feature set from file.
trainData=read.table("UCI HAR Dataset/train/X_train.txt")
## Read number of rows in this data. This should be same as rows in activities
## data and subjects data
numTrainDataReadings<-dim(trainData)[1]
## Number of columns in this data should be 561 (same as number of features
## given as part of assignment)



## Part D. Assign feature names to training data columns

## Read the feature names from file.
featureNames=read.table("UCI HAR Dataset/features.txt")
## Make a name vector of type 'character'
nameVector<-featureNames[,"V2"]
nameVector<-as.character(nameVector)
## Assign feature names to training data columns
names(trainData)<-nameVector



## Part E. Bring all training data together - by putting training data together
## with feature, activity and TRAIN/TEST data information together.

## Bind subject ids info to train data set
trainData<-cbind(trainData,trainSubjects)
## Name the column appropriately
colnames(trainData)[562]<-"subjectId"
## Bind activity ids info to train data set
trainData<-cbind(trainData,trainActivities)
## Name the column appropriately
colnames(trainData)[563]<-"activityId"
## Define this data as training data - "TRAIN"
trainData<-cbind(trainData,c("TRAIN"))
## Name the column appropriately
colnames(trainData)[564]<-"dataType"


## Part F. Read test subjects' data

## Read subjects' test data from file.
testSubjects=read.table("UCI HAR Dataset/test/subject_test.txt")
## Read number of rows in this data. Number of rows in this data should be
## same as rows in test feature data and activities data. 
numTestSubjectsReadings<-dim(testSubjects)[1]
## Read number of unique subjects. This should not be more than 30 (given in
## assignment).
numUniqueTestSubjects<-dim(unique(testSubjects))[1]



## Part G. Read test activities' data.

## Read test activities' data from file.
testActivities=read.table("UCI HAR Dataset/test/y_test.txt")
## Read number of rows in this data. Number of rows in this data should be
## same as rows in test feature data and subjects data.
numTestActivitiesReadings<-dim(testActivities)[1]
## Read number of unique training activities. This should not be more than
## 6 (given in assignment).
numUniqueTestActivities<-dim(unique(testActivities))[1]


## Part H. Read test feature set

## Read test feature set from file.
testData=read.table("UCI HAR Dataset/test/X_test.txt")
## Read number of rows in this data. This should be same as rows in activities
## data and subjects data
numTestDataReadings<-dim(testData)[1]
## Number of columns in this data should be 561 (same as number of features
## given as part of assignment)



## Part I. Assign feature names to training data columns

## Assign feature names to training data columns, by using calculated name
## vector above
names(testData)<-nameVector



## Part J. Bring all test data together - by putting test data together
## with feature, activity and TRAIN/TEST data information together.

## Bind subject ids info to train data set
testData<-cbind(testData,testSubjects)
## Name the column appropriately
colnames(testData)[562]<-"subjectId"
## Bind activity ids info to train data set
testData<-cbind(testData,testActivities)
## Name the column appropriately
colnames(testData)[563]<-"activityId"
## Define this data as training data - "TEST"
testData<-cbind(testData,c("TEST"))
## Name the column appropriately
colnames(testData)[564]<-"dataType"



## Part K. Combined training and test data into one, by putting one after another
combinedData<-rbind(trainData,testData)
## Write code book to file
## write.csv(combinedData,"Part1.csv")

## Print progress message
print("Task 1 completed.");

## ================= Task 1 Completes Here ===============================




## ================= Task 2 Starts Here ===============================
## TASK 2: Extracts only the measurements on the mean and standard deviation
## for each measurement.

## Make a vector of columns who contain "mean" or "std" in their names
selCol<-grepl("mean|std", names(combinedData),ignore.case=TRUE)
meanStdData<-combinedData[,selCol]

## Write code book to file
## write.csv(meanStdData,"Part2.csv")

## Print progress message
print("Task 2 completed.");

## ================= Task 2 Completes Here ===============================



## ================= Task 3 Starts Here ===============================
## TASK 3: Uses descriptive activity names to name the activities in the data set.

## Read activity labels from file
activityLabels=read.table("UCI HAR Dataset/activity_labels.txt")

## Add new empty column to add activity labels
combinedData<-cbind(combinedData,combinedData$dataType)
## Name the newly added column
colnames(combinedData)[565]<-"activityLabel"
## Fill the column with dummy values (activity ids)
combinedData$activityLabel<-combinedData$activityId
## Replace them with labels based on activity ids
combinedData$activityLabel<-factor(combinedData$activityLabel,
                                   levels = c(activityLabels$V1),
                                   labels = c(as.character(activityLabels$V2)))

## Write code book to file
## write.csv(combinedData,"Part3.csv")

## Print progress message
print("Task 3 completed.");

## ================= Task 3 Completes Here ===============================



## ================= Task 4 Starts Here ===============================
## TASK 4: Appropriately labels the data set with descriptive variable names.

## First correct names of columns - remove (), - and commans
featureNames<-names(combinedData)
featureNames<-gsub("\\(\\)-","-",featureNames)
featureNames<-gsub("\\(\\)","",featureNames)
featureNames<-gsub(",","-",featureNames)
featureNames<-gsub("\\)$","",featureNames)
featureNames<-gsub("\\(","-",featureNames)
featureNames<-gsub("\\)-","-",featureNames)
## Rename the columns in combined data
names(combinedData) <- featureNames

## In end, ensure that all column names contain valid characters allowed in
## column naming. Clean up names of columns and rename all columnss
validColNames <- make.names(names=names(combinedData), unique=TRUE, allow_ = TRUE)
names(combinedData) <- validColNames

## Write code book to file
## write.csv(combinedData,"Part4.csv")

## Print progress message
print("Task 4 completed.");

## ================= Task 4 Completes Here ===============================



## ================= Task 5 Starts Here ===============================
## TASK 5: From the data set in step 4, creates a second, independent tidy
## data set with the average of each variable for each activity and each subject.


## Bring subject ids, data type (train/test), activity ids and activity labels
## towards leftmost (first) columns in data frame.
combinedData<-combinedData[,c(562,564,563,565,1:561)]

## Order combined data by subject, activities, data type - test/train
## x<-combinedData
combinedData<-arrange(combinedData,subjectId,activityId,dataType)
## Group data now, on basis of subject id and activity id
bySubjectIdActivityId <- group_by(combinedData,subjectId,activityLabel)
## Now summarize the data and 561 feature set columns
summarizedData<-summarize_each(bySubjectIdActivityId,funs(mean),c(5:565))

## Write code book to file
## write.csv(summarizedData,"Part5.csv")
write.table(summarizedData,"TidyData.txt",row.names=FALSE)

## Print progress message
print("Task 5 completed. Tidy data set saved as TidyData.txt for submission.")

## ================= Task 5 Completes Here ===============================