# Getting and Cleaning Data: Assignment steps
- Submitted script has been tested to run in RStudio using 'source("run_analysis.R")' command. It saves one file TidySet.txt in working directory. Given data is assumed to be present inside "UCI HAR Dataset" folder in current directory.
- Script run_analysis.R has been well documented to explain steps given below as well.

# Steps
- Task 1
  - Reading training data (Part A - E in script for Task 1)
    - Read training data for subjects, from file "UCI HAR Dataset/train/subject_train.txt"
    - Read training data for activities, from file "UCI HAR Dataset/train/y_train.txt"
    - Read training data for 561 features, from file "UCI HAR Dataset/train/X_train.txt"
    - Read feature names (for all 561 features) and assign column name to 561 features data read above, from file "UCI HAR Dataset/features.txt"
    - Add columns for subject ids, activity ids, and "TRAIN" label in training feature set. Columns are added on right. Total 564 columns now.
  - Reading testing data (Part F - J in script for Task 1)
    - Read testing data for subjects, from file "UCI HAR Dataset/test/subject_test.txt"
    - Read testing data for activities, from file "UCI HAR Dataset/test/y_test.txt"
    - Read testing data for 561 features, from file "UCI HAR Dataset/test/X_test.txt"
    - Assign column name to 561 features data read above
    - Add columns for subject ids, activity ids, and "TEST" label in training feature set. Columns are added on right. Total 564 columns now.  
  - Combine training and test data now (Part K in script for Task 1)
- Task 2
  - Make a vector of all column names which have either 'mean' or 'std' in combined data, using grep command
  - Make another data frame containing only columns in vector
- Task 3
  - Read the activity names, from file "UCI HAR Dataset/activity_labels.txt"
  - Add an extra column to keep activity labels in combined data
  - Assign labels as factors (activity labels), based on activity ids
- Task 4
  - Make a vector of all existing columns names in combined data
  - Correct the names in the vector by replacing phrases such as "()-" and "," with "-"; removing "()" and trailing ")"
  - So that names contain only '-' and characters
- Task 5
  - Use dplyr library for this part
  - Rearrange columns such that newly added columns are shifted to left, and order by subject id and activity id (not really required for assignment)
  - Create a group_by variable based on subject and activity id
  - Summarize each of feature data columns by using mean function
  - Save the resultant data set into a text file for submission
