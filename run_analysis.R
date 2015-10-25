###1.Merges the training and the test sets to create one data set

X_train <- read.table("train\\X_train.txt")
X_test <- read.table("test\\X_test.txt")

merged_data <- rbind(X_train,X_test)
featureNames <- read.table("features.txt")[, 2] 
names(merged_data) <- featureNames 
#head(names_merged)


###2.Extracts only the measurements on the mean and standard deviation for each measurement. 

Mean_SD <- grep(".*mean.*|.*std.*", names(merged_data))
filter <- merged_data[,Mean_SD]
 	

###3.Uses descriptive activity names to name the activities in the data set
  Y_Train <- read.table("train\\y_train.txt") 
  Y_Test  <- read.table("test\\y_test.txt") 
  Merged_Y <- rbind(Y_Train, Y_Test)[, 1] 
 
 
   activityNames <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying") 
   activities <- activityNames[Merged_Y] 


###4.Appropriately labels the data set with descriptive variable names. 
	names(filter) <- gsub("^t", "Time", names(filter)) 
      names(filter) <- gsub("^f", "Frequency", names(filter)) 
	names(filter) <- gsub("-mean\\(\\)", "Mean", names(filter)) 
	names(filter) <- gsub("-std\\(\\)", "StdDev", names(filter)) 
	names(filter) <- gsub("-", "", names(filter)) 
	names(filter) <- gsub("BodyBody", "Body", names(filter)) 

 
         subjectTrain <- read.table("train\\subject_train.txt") 
	   subjectTest  <- read.table("test\\subject_test.txt") 
	   subjects <- rbind(subjectTrain, subjectTest)[, 1] 


tidy <- cbind(Subject = subjects, Activity = activities, filter)

###5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr) 
# Column means for all but the subject and activity columns 
  limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) } 
  tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans) 
  names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)]) 
 
  write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE) 
  tidyMeans 






