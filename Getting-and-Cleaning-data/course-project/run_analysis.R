
#######################################

## Course Project 1
## Submission date: 24th Jan, 2015

#######################################

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd("F:\\Learning_Resources\\Online_Courses\\Getting_and_Cleaning_Data\\Week_3\\course_project_1\\UCI_HAR_Dataset");


##############################################################################
# 1. Merge the training and the test sets to create one data set.
##############################################################################

# Read the label datasets
activityType <- read.table("activity_labels.txt", header=FALSE);  # Activity labels for each type
features     <- read.table("features.txt", header=FALSE);   # Feature names of x_train dataset

# Read the training datasets from the files
subjectTrain <- read.table("train/subject_train.txt", header=FALSE);
xTrain       <- read.table("train/x_train.txt", header=FALSE);
yTrain       <- read.table("train/y_train.txt", header=FALSE);

# Read the test datasets from the files
subjectTest <- read.table("test/subject_test.txt", header=FALSE);
xTest       <- read.table("test/x_test.txt", header=FALSE);
yTest       <- read.table("test/y_test.txt", header=FALSE);


# Assign the appropriate column labels
colnames(activityType)  <- c('activityId','activityType');

colnames(subjectTrain)  <- "subjectId";
colnames(xTrain)        <- features[,2];
colnames(yTrain)        <- "activityId";

colnames(subjectTest) <- "subjectId";
colnames(xTest)       <- features[,2]; 
colnames(yTest)       <- "activityId";


# Merge all the datasets together to create the master data set
trainingData <- cbind(yTrain, subjectTrain, xTrain);
testData <- cbind(yTest, subjectTest, xTest);
masterData <- rbind(trainingData, testData);


##############################################################################
# 2. Extract only the measurements on the mean and standard deviation
#    for each measurement. 
##############################################################################

# Create a vector for the column names from the masterData, which will be used
# to select only the desired mean() & stddev() columns
colNames <- colnames(masterData);

# Create a logicalVector that contains select the columns with mean() & stddev() in their names
logicalVector <- (grepl("activityId",colNames) | grepl("subjectId",colNames) | grepl("mean\\(\\)",colNames) | grepl("std\\(\\)", colNames));

# Subset masterData table to choose only desired columns
masterData <- masterData[logicalVector == TRUE];


##############################################################################
# 3. Use descriptive activity names to name the activities in the data set
##############################################################################

# Merge the masterData set with the acitivityType table to include descriptive activity names
masterData <- merge(masterData, activityType, by='activityId', all.x=TRUE);  # left outer join


##############################################################################
# 4. Appropriately label the data set with descriptive activity names.
##############################################################################

# Cleaning up the variable names
colNames  <- colnames(masterData);
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassign the new descriptive column names to the masterData set
colnames(masterData) <- colNames;


##############################################################################
# 5. Create a second, independent tidy data set with the average of each 
#    variable for each activity and each subject. 
##############################################################################

# Create a new table, masterDataNoActivityType without the activityType column
masterDataNoActivityType <- masterData[ , names(masterData) != 'activityType'];

# Summarize the masterDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData <- aggregate(masterDataNoActivityType[ , names(masterDataNoActivityType) != c('activityId','subjectId')], by=list(activityId = masterDataNoActivityType$activityId, subjectId = masterDataNoActivityType$subjectId), mean);

# Merge the tidyData with activityType again
tidyData <- merge(tidyData, activityType, by='activityId', all.x=TRUE);

# Export the tidyData set to a tabbed delimited file format
write.table(tidyData, "tidyData.txt", row.names= FALSE, sep='\t');
