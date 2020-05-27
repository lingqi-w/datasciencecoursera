#Merges the training and the test sets to create one data set.
path1<-file.path(getwd(),"train/X_train.txt")
train_x<-read.table(file = path1)
path2<-file.path(getwd(),"train/y_train.txt")
train_y<-read.table(file = path2)

path3<-file.path(getwd(),"test/X_test.txt")
test_x<-read.table(file = path3)
path4<-file.path(getwd(),"test/y_test.txt")
test_y<-read.table(file = path4)

train<-cbind.data.frame(train_y,train_x)
test<-cbind.data.frame(test_y,test_x)

train[,1]<-as.numeric(train[,1])
test[,1]<-as.numeric(test[,1])

data<-merge(train,test,by.x = train[,1],by.y = test[,1],all.y = TRUE)

#Extracts only the measurements on the mean and standard deviation for each measurement.
means<-sapply(data, mean, na.rm=TRUE)
sd<-sapply(data, sd, na.rm=TRUE)

#Uses descriptive activity names to name the activities in the data set
data1<-gsub(1,"WALKING",data[,4])
data1<-gsub(2,"WALKING_UPSTAIRS",data1)
data1<-gsub(3,"WALKING_DOWNSTAIRS",data1)
data1<-gsub(4,"SITTING",data1)
data1<-gsub(5,"STANDING",data1)
data1<-gsub(6,"LAYING",data1)

#Appropriately labels the data set with descriptive variable names.
data[,4]<-data1

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
na_flag <- apply(is.na(data), 2, sum)
data <- data[,which(na_flag == 0)]
s<-split(data,data[,4])
names(s)
means1<-matrix(0,nrow = 6,ncol = 562)
sd1<-matrix(0,nrow = 6,ncol = 562)
for (i in 1:6) {
  means1[i,]<-sapply(s[[i]], mean, na.rm=TRUE)
  sd1[i,]<-sapply(s[[i]], sd, na.rm=TRUE)
}








