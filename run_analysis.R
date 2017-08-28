
###

#The R script "run_analysis" does the following:

# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

###

#set the working directory

setwd("C:/Users/Rahul/Desktop") 


# 1)

# Load the given data set that include features, activity, train and test records.

Features <- read.table('./features.txt',header=FALSE); 
Activity <- read.table('./activity_labels.txt',header=FALSE);  

Subject_Train <- read.table('./subject_train.txt',header=FALSE);  
X_Train <- read.table('./x_train.txt',header=FALSE);  
Y_Train <- read.table('./y_train.txt',header=FALSE);


Subject_Test <- read.table('./subject_test.txt',header=FALSE);
X_Test <- read.table('./x_test.txt',header=FALSE); 
Y_Test <- read.table('./y_test.txt',header=FALSE);  

# Assignment of column names to the Train and Test sets.

colnames(Activity) <- c('Label_ID','Activity'); 
colnames(Subject_Train) <- "Subject_ID"; 
colnames(X_Train) <- Features[,2];  
colnames(Y_Train) <- "Label_ID"; 

colnames(Subject_Test) <- "Subject_ID"; 
colnames(X_Test) <- Features[,2];  
colnames(Y_Test) <- "Label_ID"; 

# Combine the Train data sets using cbind function.

Train_Data <- cbind(Y_Train,Subject_Train,X_Train);

# Combine the Test data sets using cbind function.

Test_Data <- cbind(Y_Test,Subject_Test,X_Test);

# Combine both the Train and Test data sets using rbind function.

Result <- rbind(Train_Data,Test_Data);

# 2)

# Obtain all the variable names in order to extract both the mean and standard deviation variables. 

Var_Names  <- colnames(Result);

# Use grep function to extract only the required variables.

Join <- (grepl("Label..",Var_Names) | grepl("Subject..",Var_Names) | grepl("-mean..",Var_Names) & !grepl("-meanFreq..",Var_Names) | grepl("-std..",Var_Names));

Result <- Result[Join==TRUE];

# 3)

# Assign the activity names for each subjects based on the label id.

Result <- merge(Result,Activity,by='Label_ID');

# 4)

# Provide description to the variable names.

names(Result) <- gsub("-mean","Mean", names(Result)) 
names(Result) <- gsub("-std","Std_Dev", names(Result))
names(Result) <- gsub("^t", "time", names(Result))
names(Result) <- gsub("Mag", "Magnitude", names(Result))
names(Result) <- gsub("^f", "frequency", names(Result))
names(Result) <- gsub("Acc", "Accelerometer", names(Result))
names(Result) <- gsub("Gyro", "Gyroscope", names(Result))

# 5)

# Obtain the average values of each variables for each activity and subject.

Result <- aggregate(. ~Subject_ID + Label_ID, Result, mean)

# Create a tidy data set from the obtained values.

write.table(Result, file = "tidydata.txt",row.name=FALSE)


