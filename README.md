Getting-and-Cleaning-Data
=========================

Project Repository 

# You should create one R script called run_analysis.R that does the following.
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load required packages: data.table and reshape2

if (!require("data.table")) {
        install.packages("data.table")
}

if (!require("reshape2")) {
        install.packages("reshape2")
}

require("data.table")
require("reshape2")



# Read activity names into variable activity_labels variable 
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

#Read y_test.txt into y_test.  2947 obs of 1 variable
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

#Append a column to y_test to store the name of the activity 
y_test[,2] = activity_labels[y_test[,1]]

# Provide variable names to y_test 
names(y_test) = c("ActivityID", "ActivityName")

#Read y_train.txt into y_train.  7352 obs of 1 variable
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

#Append a column to y_train to store the name of the activity
y_train[,2] = activity_labels[y_train[,1]]

# Names the variaable in y_train  
names(y_train) = c("ActivityID", "ActivityName")

# Read subject_test.txt into subject_test variable 2947 observations of 1 variable
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Name subject_test variable SubjectID
names(subject_test) = "SubjectID"

# Read subject_train.txt into subject_train variable, 7352 observations of 1 variable
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Name subject_train variable SubjectID
names(subject_train) = "SubjectID" 

# Read features.txt into feature variable, factor with 477 levels 
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

# Identify which features variables contain the the text sd or mean
# Assign to sd_mean a logical vector containing the result of text comparison from grepl on features
# sdmean_features is a logical vector of length 561
# sum(sdmean_features, na.rm = TRUE) shows that 79 instances of mean or std have been identifed in features var 
sdmean_features <- grepl("mean|std", features)

#Read x_test.txt into table x_test, 2947 observations of 561 variables 
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

#Apply names to x_test table from the feature variable
names(X_test) = features

# Use logical vector sdmean_features to subset the x_test variable 
X_test = X_test[,sdmean_features]

#Read x_train.txt into table x_train, 7352 observations of 561 variables 
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

#Apply names to x_test table from the feature variable
names(X_train) = features

# Use logical vector sdmean_features to subset the x_test variable 
X_train = X_train[,sdmean_features]

# Assemble train_combined table and test_combined table from elements    

test_combined <- cbind(as.data.table(subject_test), y_test,X_test)
train_combined <- cbind(as.data.table(subject_train), y_train, X_train)

# Assemble data_combined table from test_combined and train_combined tables 
# 10299 obs of 82 variables
# length(unique(data_combined$SubjectID)) shows 30 unique SubjectID
data_combined = rbind(test_combined, train_combined)

#Prepare to melt data_combined
# Summarizing Variables from data_combined are SubjectID, AcitivtyID and ActivityName
id_melt   = c("SubjectID", "ActivityID", "ActivityName")

# Identify the other variables in the data_combined table as the difference 
# between id_melt and all other columns in data_combined table   
data_labels = setdiff(colnames(data_combined), id_melt)

#Apply the melt to data_combined with id = id_melt a
melt_data = melt(data_combined, id = id_melt, measure.vars = data_labels)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, SubjectID + ActivityName ~ variable, mean)

# Write the tidy_data file
# 180 observations of 81 variables
# we have 30 unique ids and 6 distince Activities of 180 rows
# Because the final summary drops not include 
write.table(tidy_data, file = "./tidy_data.txt")
