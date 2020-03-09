 #############################################################################################
 #
 #                    PEER GRADED ASSIGNMENT FOR GETTING AND CLEANING DATA 
 #
 # This R script:
 # 1-Unzip the files downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
 # 2-Loads the files needed for the assigment in R
 # 3-Merges the training and test datasets with labels
 # 4-Selects only the means the standard deviation for each measument
 # 5-Create a tidy dataset with the average of each variable for each activity and each subject
 #
 # The following libraries are needed to run the script:
 # - readr
 # - stringr
 # -dplyr
 ############################################################################################

 ###########################################################################################
 # General settings
 ############################################################################################
 # clean environment
  rm(list=ls())
 #set working directory
  setwd("C:/Documents/Getting and cleaning data/GettingCleaningDataProject")
 # load libraries
  library(readr)
  library(stringr)
  library(dplyr)

 ############################################################################################
 # (1) unzip file
 #############################################################################################

  unzip("getdata_projectfiles_UCI HAR Dataset.zip")

 ############################################################################################
 # (2) load files
 ############################################################################################

 # activity labels
  a.df <- read_table2("UCI HAR Dataset/activity_labels.txt", 
                               col_names = c("activity_id","activity_label"))
  a.df$activity_label<- tolower(a.df$activity_label)

 # features file
  features_header <- read_table2("UCI HAR Dataset/features.txt", 
                        col_names = c("features_id", "features"))
  features_header$features<- sapply(features_header$features, function(x) {gsub("[()]",'',x)})
 # test files
  subject_test_file <- "./UCI HAR Dataset/test/subject_test.txt"
  x_test_file <- "./UCI HAR Dataset/test/X_test.txt"
  y_test_file <- "./UCI HAR Dataset/test/Y_test.txt"

  subject_test <- read_table2(subject_test_file,  col_names = FALSE)
  x_test <- read_table2(x_test_file,  col_names = FALSE)
  y_test <- read_table2(y_test_file,  col_names = FALSE)
  
 # train files
  subject_train_file <- "./UCI HAR Dataset/train/subject_train.txt"
  x_train_file <- "./UCI HAR Dataset/train/X_train.txt"
  y_train_file <- "./UCI HAR Dataset/train/Y_train.txt"
  
  subject_train <- read_table2(subject_train_file,  col_names = FALSE)
  x_train <- read_table2(x_train_file,  col_names = FALSE)
  y_train <- read_table2(y_train_file,  col_names = FALSE) 
 
 ############################################################################################
 # (3) merge test and train dataframes
 ############################################################################################
 #merge
  s.df<-rbind(subject_test, subject_train)
  x.df <-rbind(x_test,x_train)
  y.df <-rbind(y_test,y_train)
  
 #labels and var names
  colnames(s.df) <-c("subject_id")
  colnames(x.df) <- features_header$features
  names(x.df)<-gsub("^t", "time", names(x.df))
  names(x.df)<-gsub("^f", "frequency", names(x.df))
  names(x.df)<-gsub("Acc", "Accelerometer", names(x.df))
  names(x.df)<-gsub("Gyro", "Gyroscope", names(x.df))
  names(x.df)<-gsub("Mag", "Magnitude", names(x.df))
  names(x.df)<-gsub("BodyBody", "Body", names(x.df))
  colnames(y.df) <- c("activity_id")
  
  
 ############################################################################################
 # (4) select mean and s.d. 
 ############################################################################################
   
  mean_sd_list <-grep("mean|sd", features_header$features, ignore.case = TRUE)
  x.df<- x.df[,mean_sd_list]
  
 ############################################################################################
 # (5) tidy datframe
 ############################################################################################
  
  #create tidy dataframe
   tidy.df <- cbind(s.df, y.df, x.df)
   tidy.df <- left_join(tidy.df,a.df, by = "activity_id")
   
 #average of each variable for each activity and each subject.
  tidy.df <- tidy.df %>% 
             group_by(activity_label,subject_id) %>% 
             summarise_each(mean)
  
  # Write the data out
  write.table(tidy.df, "tidy_dataset.txt", row.names = FALSE)
  
  