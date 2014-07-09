#Downloading data 
url<- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# loading downloaded data 
setwd("./UCI HAR Dataset")

# Using descriptive activity names to name the activities in the data set
features <- read.table("features.txt")
names(features) <- c("feature_label","feature_name")
activity_labels <- read.table("activity_labels.txt")
names(activity_labels) <- c("label","activityname")
  
# loading and combining test dataset 
setwd("./test/")

subject_test <- read.table("subject_test.txt")
names(subject.test) <- "subject"

X_test <- read.table("X_test.txt")
names(X_test) <- features$feature_name

y_test <- read.table("y_test.txt")
names(y_test) <- "label"

test_dataset <- cbind(y_test,X_test, subject_test)

# loading and combining training datasetset 

setwd("../")
setwd("./train")

subject_train <- read.table("subject_train.txt")
names(subject_train) <- "subject"

X_train <- read.table("X_train.txt")
names(X_train) <- features$feature_name

y_train <- read.table("y_train.txt")
names(y_train) <- "label"

train_dataset <- cbind(y_train,X_train, subject_train)


# merging test and training datasets

if(any(names(test_dataset) != names(train_dataset))) stop("The column names are not equal.")
global_dataset <- rbind(test_dataset, train_dataset)


#Extracting the measurements on the mean and standard deviation 
global_std <- grep("std\\(\\)", names(global_dataset), value = TRUE)
global_mean <- grep("mean\\(\\)",names(global_dataset), value = TRUE)


#Creating a second, independent tidy data set with the average of each variable for each activity and each subject. 
vars_to_include <- c(global_std,global_mean)
cols_to_include <- c("label","subject", vars_to_include)

global_tidydata <- global_dataset[  , cols_to_include]


# Renaming columns

names(global_tidydata) <- gsub("\\(", ""  , names(global_tidydata))
names(global_tidydata) <- gsub("\\)", ""  , names(global_tidydata))
names(global_tidydata) <- gsub("-", ""  , names(global_tidydata))
names(global_tidydata) <- gsub("std", "StanDev"  , names(global_tidydata))
names(global_tidydata) <- gsub("mean", "Mean"  , names(global_tidydata))


# Setting difference between variables to include and "subject", "lebel"
library(plyr)
vars_to_include <- setdiff(names(global_tidydata),"label")
vars_to_include <- setdiff(vars_to_include,"subject")

# calculating mean of each variable for each activity and each subject

global_tidydata_avg <- aggregate(global_tidydata[ vars_to_include], by = list(global_tidydata$label,global_tidydata$subject), mean )


# Renaming Group.1 and Group.2 and merging activity labels

names(global_tidydata_avg) <- gsub( "Group.1", "label", names(global_tidydata_avg) )
names(global_tidydata_avg) <- gsub( "Group.2", "subject", names(global_tidydata_avg) )

global_tidydata_avg<- merge(activity_labels, global_tidydata_avg)

# Writing and saving the final tidy data table
setwd("../")
setwd("../")

write.table(global_tidydata_avg, "average_variable_activity_subject.txt", row.names = FALSE)

#checking the independent tidy data set
independent_data<- read.table("average_variable_activity_subject.txt", header=TRUE)
independent_data

