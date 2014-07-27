xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

test_set <- cbind(ytest,subject_test,xtest)
names(test_set)[1] <- "Test_Type"
names(test_set)[2] <- "Subject_num"

xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

train_set <- cbind(ytrain,subject_train,xtrain)
names(train_set)[1] <- "Test_Type"
names(train_set)[2] <- "Subject_num"

total_set <- rbind(test_set,train_set)
#Now we have both the test set and training set into one table

#Now we have to find the means and standard deviations of the all rows of data
means <- numeric(dim(total_set)[1])
stdev <- numeric(dim(total_set)[1])

for(i in 1:dim(total_set)[1]){
  q <- total_set[i,3:dim(total_set)[2]]
  q <- q[!is.na(q)]
  means[i] <- mean(q)
  stdev[i] <- sd(q)
}
test_type <- total_set[,1]
subject_num <- total_set[,2]

table_names <- c("Test_Type","Subject_num","mean","stdev")
fin_table <- cbind(test_type,subject_num,means,stdev)
mytable <- as.data.frame(fin_table)
#Putting the activity names into the data set
for(i in 1:dim(mytable)[1]){
  u <- match(mytable[i,1],activity_labels[,1])
  mytable[i,1] <- as.character(activity_labels[u,2])
}
#split tables up by activity
tables_list_activity <- split(mytable,mytable$test_type)

#split tables up further by subject number
tables_list_laying <- split(tables_list_activity[[1]],tables_list_activity[[1]]$subject_num)
tables_list_sitting <- split(tables_list_activity[[2]],tables_list_activity[[2]]$subject_num)
tables_list_standing <- split(tables_list_activity[[3]],tables_list_activity[[3]]$subject_num)
tables_list_walking <- split(tables_list_activity[[4]],tables_list_activity[[4]]$subject_num)
tables_list_walking_downstairs <- split(tables_list_activity[[5]],tables_list_activity[[5]]$subject_num)
tables_list_walking_upstairs <- split(tables_list_activity[[6]],tables_list_activity[[6]]$subject_num)

master_list <- list(tables_list_laying,tables_list_sitting,tables_list_standing,tables_list_walking,tables_list_walking_downstairs,tables_list_walking_upstairs)

#get the mean of each subject's activity
Activity <- character(length(tables_list_activity)*length(tables_list_laying))
Subject <- numeric(length(tables_list_activity)*length(tables_list_laying))
Average <- numeric(length(tables_list_activity)*length(tables_list_laying))

for(i in 1:length(master_list)){
  for(j in 1:length(master_list[[i]])){
    tempData <- as.data.frame(master_list[[i]][j])
    tempMean <- mean(tempData[,3])
    tempAct <- tempData[1,1]
    index_number <- (i-1)*30 + j
    Activity[index_number] <- tempAct
    Subject[index_number] <- tempData[1,2]
    Average[index_number] <- tempMean
  }
}

final_data <-data.frame(Activity,Subject,Average)