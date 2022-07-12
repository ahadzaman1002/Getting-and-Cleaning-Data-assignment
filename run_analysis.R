# SETTING WORKING DIR
setwd("D:/Coursera/Data_cleaning_processing")

library(tidyverse)

# IDENTIFY FILE PATHS AND COLUMN NAMES-
list.files()
list.files("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train")



test_path <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/"
train_path <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/"

col_names <- read_lines( "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")


# READING TEST DATA---
test_data <- read.table( paste(test_path, "X_test.txt", sep = ""), header = FALSE)

# ADDING COLUMN NAMES TO THE TEST DATA --
names(test_data)<- col_names

# ADDING SUBJECT ID TO TEST DATA - 
test_data$subject_id <- read_lines(paste(test_path, "subject_test.txt", sep = ""))


# ADDING ACTIVITY ID TO TEST DATA -
test_data$activity_id <- read_lines(paste(test_path, "y_test.txt", sep = ""))




# READING TRAIN DATA ---

train_data <- read.table(paste(train_path, "X_train.txt", sep = ""), header = FALSE )

# ADDING COLUMN NAMES TO TRAIN DATA

names(train_data) <- col_names

# ADDING SUBJECT ID TO TRAIN DATA --

train_data$subject_id <- read_lines(paste(train_path, "subject_train.txt", sep = ""))

# ADDING ACTIVITY ID TO TRAIN DATA -

train_data$activity_id <- read_lines(paste(train_path, "y_train.txt", sep = ""))



# STEP 1: Merges the training and the test sets to create one data set.

# MERGING TRAINING  AND TEST DATA SETS TO CREATE ONE DATA SET

dta <- bind_rows(train_data, test_data)


# STEP 2: Extracts only the measurements on the mean and 
#         standard deviation for each measurement. 

dta <- dta %>% 
  select(contains("mean", ignore.case = TRUE), 
         contains("std", ignore.case = TRUE),
         c(562, 563) )

# removing test and train data from working directory 
rm(test_data, train_data)



# STEP :3
# Uses descriptive activity names to name the activities in the data set
dta <- dta %>%
  mutate(
    activity_id = case_when(
      activity_id == 1 ~ "WALKING",
      activity_id == 2 ~ "WALKING_UPSTAIRS",
      activity_id == 3 ~ "WALKING_DOWNSTAIRS",
      activity_id == 4 ~ "SITTING",
      activity_id == 5 ~ "STANDING",
      activity_id == 6 ~ "LAYING"
    )
  )




# STEP 4: 
# Appropriately labels the data set with descriptive variable names.

dta <- dta %>% 
  janitor::clean_names()


names(dta)<- str_remove(names(dta), pattern = "x\\d*_")




# STEP: 5
# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.


tdy_dta <- dta %>% 
  group_by(subject_id, activity_id) %>% 
  summarise_at(c(1:86),  .funs = mean) %>% 
  ungroup()


# WRITING THIS DATA TO DISK --

write.table(tdy_dta, file = "tidy_dta.txt", row.names = FALSE)











