library ("dplyr")
library("reshape") # Thank's a lot to Mr. Robert Kabakoff - Step # 5 will be so easy!

########################## 1.  Merges the training and the test sets to create one data set.###############################################################################

a<-read.table("C:/KIWORK/Dataset/train/y_train.txt") #read Activitys tables 
b<-read.table("C:/KIWORK/Dataset/test/y_test.txt")
j<-rbind(a,b) #Join train and test data by Activity


c<-read.table("C:/KIWORK/Dataset/train/subject_train.txt") #read subjects tables 
d<-read.table("C:/KIWORK/Dataset/test/subject_test.txt")
k<-rbind(c,d) #Join train and test data by Subject

g<-read.table("C:/KIWORK/Dataset/train/x_train.txt") #read measurements tables 
h<-read.table("C:/KIWORK/Dataset/test/x_test.txt")
l<-rbind(g,h) #Join train and test data by measurement

f<- read.table("C:/KIWORK/Dataset/features.txt") #Create column names for each variable
#write.csv(f,"table f.csv") # Figure out what is it
f1<-t(f) #transpose 
f2<-f1[2,]

colnames(l)<-f2
colnames(j)<-"Activity"
colnames(k)<-"Subject"

############################################# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. #################################
merge<-dplyr::bind_cols(k,j,l) #create one data.frame 

merge2 <- merge[ , !duplicated(colnames(merge))] #exclude duplicated names whereas  has 'mean' as a parameter to a function.

merge3<-select(merge2,contains("()")) #exclude all names whereas  has 'mean' as a parameter to a function.

merge4<-select(merge3,contains("mean")) #include all names with mean only

merge5<-select(merge3,contains("std")) #include all names with std only

mergeF<-dplyr::bind_cols(k,j,merge4,merge5) #create one data.frame

glimpse(mergeF)#Check what happened
dim(mergeF) #Check what happened

############################################# 3.  Uses descriptive activity names to name the activities in the data set  #################################################
Act<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

V<-mergeF$Activity

V<-sub(1,Act[1],V)# I know, i know that exist cycle code, but as usually I have no time...
V<-sub(2,Act[2],V)
V<-sub(3,Act[3],V)
V<-sub(4,Act[4],V)
V<-sub(5,Act[5],V)
V<-sub(6,Act[6],V)

mergeF$Activity<-V

########################################### 4.  Appropriately labels the data set with descriptive variable names #########################################################
W1<-colnames(mergeF[1:2])
W2<-paste("Group-Mean",colnames(mergeF[3:81]))
#W3<-paste("Group-Std",colnames(mergeF[49:81]))
colnames(mergeF)<-c(W1,W2) # All new names reflected FUTURES values of tidy Data

#########5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.################

md <- reshape::melt(mergeF, id=(c("Subject", "Activity"))) #Instead Hot-Metal we will melt and cast ...data
final<-reshape::cast(md,Subject+Activity~variable,mean)
write.table(final,"C:/KIWORK/final_tidy_data.txt", row.name=FALSE) # Now we have real tidy data 
#test<-read.table("C:/KIWORK/final_tidy_data.txt")
################################################# THE END #################################################################################################################