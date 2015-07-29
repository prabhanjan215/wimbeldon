#install the package prior cleaning data
#install.packages("Rcpp")#use it if you dont have the package and to install it

##cleaning the rawdata file of shots

#reading rawdata file into R
rawdata<-read.table("shots_sakke33_vesavee.txt",header=F,sep=":")
#dummy$V1<-gsub("\\[|\\]", "\"", dummy$V1)

#Separating shuffeled data rows from rawdata
library(Rcpp)
library(plyr)

#Please ensure all libraries are loaded properly.
Rowcount<- count(rawdata,"V3")
Diffrows <- subset(Rowcount,Rowcount$freq==1)
rawdata1<-subset(rawdata,rawdata$V3 %in% Diffrows$V3)
rawdata2<-subset(rawdata,!(rawdata$V3 %in% Diffrows$V3))

##Arranging the shuffeled Data set
#removing unnecessary columns

rawdata1$V1<-NULL
rawdata1<-rawdata1[,c("V2","V11","V10","V9","V8","V14","V6","V15","V12","V7","V13","V5","V3","V4")]
names(rawdata1)<-c(1:14)

##cleaning rawdata2 dataset

#removing unnecessary columns
rawdata2$V1<-NULL
rawdata2$Count<-NULL

#arranging notdelimited columns at last to clean them separately

names(rawdata2)<-c(1:14)
rawdata2<-rawdata2[,c(1:4,6,9:11,13,5,7,8,12,14)]
names(rawdata2)<-c(1:14)

#rowbinding splitted datasets

rawdata<-rbind(rawdata1,rawdata2)

#names(rawdata2)<-c("shotid","sessionid","rallieid","player","netpos","nettime","startpos","endpos","starttime","endtime","type","ballmark","speed","call")
cleaned1<-data.frame(rep("dummy",nrow(rawdata)))

for(j in c(1:7,9)){
test <- ldply(strsplit(as.character(rawdata[,j]),","))
cleaned1 <- data.frame(cbind(cleaned1,test$V1))
}
cleaned1[,1]<-NULL
names(cleaned1)<-c("shotid","sessionid","rallieid","player","nettime","starttime","endtime","speed")
save1<-cleaned1

#cleaning nondelimited columns
cleaned2<-data.frame(rep("dummy",nrow(rawdata)))

for(i in 10:13){
test<-gsub("[^[:digit:],.-]", "", rawdata[,i])
cleaned2 <- data.frame(cbind(cleaned2,test))
}
cleaned2[,1]<-NULL
names(cleaned2)<-c(1:4)

rawdata[,"8"]<-gsub(" }","",rawdata[,"8"])
rawdata[,"14"]<-gsub(" }","",rawdata[,"14"])
rawdata[,"8"]<-gsub(", ball_mark","",rawdata[,"8"])
rawdata[,"14"]<-gsub(", end_pos","",rawdata[,"14"])

cleaned1$type<-rawdata[,"8"]
cleaned1$call<-rawdata[,"14"]

list1<-strsplit(as.character(cleaned2[,2]),",")
test<-ldply(list1)

cleaned1$sposX<-test$V1
cleaned1$sposY<-test$V2
cleaned1$sposZ<-test$V3

list1<-strsplit(as.character(cleaned2[,3]),",")
test<-ldply(list1)

cleaned1$eposX<-test$V1
cleaned1$eposY<-test$V2
cleaned1$eposZ<-test$V3

cleaned1$netpos<-cleaned2[,"1"]
cleaned1$ballmark<-cleaned2[,"4"]


shots<-cleaned1[,c("sessionid","rallieid","shotid","player","netpos","nettime","sposX","sposY","sposZ","eposX","eposY","eposZ","starttime","endtime","type","ballmark","speed","call")]

##convering meters into feet to apply standards of distance as per rules
shots$sposX<-as.numeric(shots$sposX)*3.2808399
shots$sposY<-as.numeric(shots$sposY)*3.2808399
shots$sposZ<-as.numeric(shots$sposZ)*3.2808399
shots$eposX<-as.numeric(shots$eposX)*3.2808399
shots$eposY<-as.numeric(shots$eposY)*3.2808399
shots$eposZ<-as.numeric(shots$eposZ)*3.2808399

#ensuring that there are no space or any unwanted charecetrs in data

for(i in 1:ncol(shots)){
  
  shots[,i]<-gsub(" ","",shots[,i],fixed=T)
}

##sorting the data in order of shot id

shots<-shots[with(shots, order(shotid)), ]

##writing it into file

write.csv(shots,"shots.csv")



