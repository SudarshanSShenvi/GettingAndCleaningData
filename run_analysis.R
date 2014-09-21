# Data Set for the project downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# Downloaded to local directory
# **************************** run_analysis.R Script Expectation ************************************
# a. Merges the training and the test sets to create one data set.
# b. Extracts only the measurements on the mean and standard deviation for each measurement. 
# c. Uses descriptive activity names to name the activities in the data set
# d. Appropriately labels the data set with descriptive variable names. 
# e. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
# **************************** run_analysis.R Script Expectation ************************************

# Step 1 --> Merge Train and Test Data for each item (subject, X, Y)
subjectTrainData <- read.table("UCI HAR Dataset/train/subject_train.txt")
subjectTestData <- read.table("UCI HAR Dataset/test/subject_test.txt")
subjectCombinedData <- rbind(subjectTrainData, subjectTestData)

xTrainData <- read.table("UCI HAR Dataset/train/X_train.txt")
xTestData <- read.table("UCI HAR Dataset/test/X_test.txt")
xCombinedData <- rbind(xTrainData, xTestData)

yTrainData <- read.table("UCI HAR Dataset/train/y_train.txt")
yTestData <- read.table("UCI HAR Dataset/test/y_test.txt")
yCombinedData <- rbind(yTrainData, yTestData)

# Step 2 --> Get Mean and SD for each measurement.
allFeatures <- read.table("UCI HAR Dataset/features.txt")
#######################################################################
# allFeature variable content as below
#    1 tBodyAcc-mean()-X
#    2 tBodyAcc-mean()-Y
#    3 tBodyAcc-mean()-Z
#    4 tBodyAcc-std()-X
#    5 tBodyAcc-std()-Y
#######################################################################
# Now we only need items which have mean and std from the feature set
# The mean and standard deviation features are represented 
# as xxxx-mean() and xxxx-std(). So need to pick only those features
# which have mean and std in them. grep the feature list and Pick the 
# indices of our interest.
#######################################################################
meanStd_Indices <- grep("-mean\\(\\)|-std\\(\\)", allFeatures[, 2])
xCombinedData <- xCombinedData[, meanStd_Indices]
# Now xCombinedData has the content of our interest but without column headers
names(xCombinedData) <- allFeatures[meanStd_Indices, 2]
# remove paranthesis to improve appearance
names(xCombinedData) <- gsub("\\(|\\)", "", names(xCombinedData))
# update data with uniform case - lower case
names(xCombinedData) <- tolower(names(xCombinedData))
# Now xCombinedData has the content of our interest but with appropriate column headers

# Step 3. Read activity list to get descriptive activity names
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
# remove underscore and convert activity name to lower case
activityLabels[, 2] = gsub("_", "", tolower(as.character(activityLabels[, 2])))
# now replcae activity labels instead of activity numbers
yCombinedData[,1] = activityLabels[yCombinedData[,1], 2]
# Provide appropriate column name
names(yCombinedData) <- "activities"

# Step 4 - Appropriately labels the data set with descriptive activity names.
names(subjectCombinedData) <- "subject"
# Consolidate all data in a single variable.
consolidatedHARData <- cbind(subjectCombinedData, yCombinedData, xCombinedData)
write.table(consolidatedHARData, "UCI HAR Dataset/Human_Activity_Recognition_consolidated.txt")

# Step 5 compute data set with averages of each variable for each activity and each subject.

consolidatedHARData$activities <- as.factor(consolidatedHARData$activities)
consolidatedHARData$subject <- as.factor(consolidatedHARData$subject)
# compute average data for each variable
averageData = aggregate(consolidatedHARData, by=list(activities = consolidatedHARData$activities, subject = consolidatedHARData$subject), mean)
# remove Subject and Activities columns as values are NA
averageData[,4] = NULL
averageData[,3] = NULL
write.table(averageData, "UCI HAR Dataset/HAR_average_data.txt")