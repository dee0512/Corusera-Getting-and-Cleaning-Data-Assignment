subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")

#Appropriately labels the data set with descriptive variable names. 
colnames(subject_test) <- "Subject"
colnames(subject_train) <- "Subject"
colnames(Y_train) <- "Activity"
colnames(Y_test) <- "Activity"
features <- read.table("UCI HAR Dataset/features.txt")
colnames(X_test) <- features$V2
colnames(X_train) <- features$V2


#Merges the training and the test sets to create one data set.
test_dataset <- cbind(subject_test,Y_test,X_test)
train_dataset <- cbind(subject_train,Y_train,X_train)
HAR_dataset <- rbind(test_dataset,train_dataset)
Har_dataset

#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Check is the variable name contains mean() or std() as given in the features_info.txt:
#mean(): Mean value
#std(): Standard deviation
meancols <- grep("mean()",colnames(HAR_dataset))
stdcols <- grep("std()",colnames(HAR_dataset))
meansAndStd <- cbind(HAR_dataset[,meancols],HAR_dataset[,stdcols])
meansAndStd

#Uses descriptive activity names to name the activities in the data set
HAR_dataset$Activity <- sapply(HAR_dataset$Activity,function(a) activity_names[activity_names$V1 == a,2])
HAR_dataset$Activity

#From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
tidy_dataset <- data.frame(c(rep(1:30,each=6)))
tidy_dataset <- cbind(tidy_dataset,rep(activity_names[,2],30))
means<-sapply(1:180,function(a){
  sapply(3:563, function(b) 
    mean(HAR_dataset[HAR_dataset[,1] == tidy_dataset[a,1] & HAR_dataset[,2] == tidy_dataset[a,2],b]))})
newmeans <- t(means) #transpose the matrix
tidy_dataset <- cbind(tidy_dataset,newmeans)
colnames(tidy_dataset) <- colnames(HAR_dataset)