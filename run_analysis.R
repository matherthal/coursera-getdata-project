##working directory set to "UCI HAR Dataset" folder


##Read X measurements in train and test data sets
train_x <-read.table("train/X_train.txt")
test_x <-read.table("test/X_test.txt")
##Bind X measurements from train and test together
x_meas <- rbind(train_x, test_x)

##Read Y measurements (the activities) in train and test data sets 
train_y <- read.table("train/Y_train.txt")
test_y <- read.table("test/Y_test.txt")
##Bind Y measurements from train and test together
y_meas <- rbind(train_y, test_y)

##Read the subjects in train and test
train_sbj <- read.table("train/subject_train.txt")
test_sbj <- read.table("test/subject_test.txt")
##Bind the subjects together
sbjs <- rbind(train_sbj, test_sbj)


##Step 4 - Appropriately labels the data set with descriptive variable names. 
##Below the columns are named with the fixed names come from "features.txt" file

##Get variable names
names <- read.table("features.txt")
names <- names[,2]
##Fix variable names
names <-tolower(names)
names <-gsub("\\-", "", names)
names <-gsub("\\(", "", names)
names <-gsub("\\)", "", names)
names <-gsub("\\,", "", names)

##Set variable names to X measurements data set
colnames(x_meas) <- names 
##Set variable name to Y measurements
colnames(y_meas) <- "activity"
##Set variable name to subjects
colnames(sbjs) <- "subject"


##Step 2 - Extracts only the measurements on the mean and standard 
##deviation for each measurement. 

##Vector of means and standard deviations taken from "features.txt"
selectedCols <- c(1, 2, 3, 4, 5, 6,
	41, 42, 43, 44, 45, 46,
	81, 82, 83, 84, 85, 86,
	121, 122, 123, 124, 125, 126,
	161, 162, 163, 164, 165, 166,
	201, 202, 214, 215, 227, 228, 
	240, 241, 253, 254, 
	266, 267, 268, 269, 270, 271,
	294, 295, 296, 
	345, 346, 347, 348, 349, 350, 
	373, 374, 375,
	424, 425, 426, 427, 428, 429,
	452, 453, 454, 503, 504,
	513, 516, 517, 526, 529, 530,
	539, 542, 552)

selectedCols <- c(1, 2, 3, 4, 5, 6,
	41, 42, 43, 44, 45, 46,
	81, 82, 83, 84, 85, 86,
	121, 122, 123, 124, 125, 126,
	161, 162, 163, 164, 165, 166,
	201, 202, 214, 215, 227, 228, 
	240, 241, 253, 254, 
	266, 267, 268, 269, 270, 271,
	294, 295, 296, 
	345, 346, 347, 348, 349, 350, 
	373, 374, 375,
	424, 425, 426, 427, 428, 429,
	452, 453, 454, 503, 504,
	513, 516, 517, 526, 529, 530,
	539, 542, 552)
x_meas <- x_meas[,selectedCols]


##Step 1 - Merges the training and the test sets to create one data set.
##Below the step 1 is finished, since the data sets are merged into "data"

##Join together the measurements and the subjects
data <- x_meas
data$activity <- y_meas
data$subject <- sbjs


##Step 3 - Uses descriptive activity names to name the activities in the 
##data set
##Below a function is applied to activity column to convert it from integer
##to its corresponding text

nameActivity <- function(x) {
	switch(as.character(x), "1"="WALKING",
		"2"="WALKING_UPSTAIRS",
		"3"="WALKING_DOWNSTAIRS",
		"4"="SITTING",
		"5"="STANDING",
		"6"="LAYING",
		x)
}

##Convert activity codes into activity names
##activity <-data$activity 
##activity$activity <- sapply(activity$activity, nameActivity)
##Convert activity names to factor
##activity$activity <- as.factor(activity$activity)
##data$activity <- activity$activity

##Convert activity codes into activity names
activity<-sapply(y_meas$activity, nameActivity)
activity <- data.frame(activity)
data <- cbind(data, activity)
data <- cbind(data, sbjs)


##Step 5 - Creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.

##Measures de mean for each variable related to activity and subject
data2<-aggregate(data[1:78], by=list(data$activity, data$subject), mean)


write.table(data2, file="./step5.txt", row.names=FALSE)
