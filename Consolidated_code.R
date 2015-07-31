#arranging working directory as user documents(default)

setwd("~/")

#install the package prior cleaning data
#install.packages(c("Rcpp","ggplot2","plyr","dplyr","Rmisc","gridExtra"),dependencies=TRUE)#use it if you dont have the package and to install it

##cleaning the rawdata file of shots

#reading rawdata file into R
rawdata<-read.table("shots_sakke33_vesavee.txt",header=F,sep=":")
#dummy$V1<-gsub("\\[|\\]", "\"", dummy$V1)

#Separating shuffelled data rows from rawdata
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

list1<-strsplit(as.character(cleaned2[,"2"]),",")
test<-ldply(list1)

cleaned1$sposX<-test$V1
cleaned1$sposY<-test$V2
cleaned1$sposZ<-test$V3

list1<-strsplit(as.character(cleaned2[,"3"]),",")
test<-ldply(list1)

cleaned1$eposX<-test$V1
cleaned1$eposY<-test$V2
cleaned1$eposZ<-test$V3

cleaned1$netpos<-cleaned2[,"1"]
cleaned1$ballmark<-cleaned2[,"4"]


shots<-cleaned1[,c("sessionid","rallieid","shotid","player","netpos","nettime","sposX","sposY","sposZ","eposX","eposY","eposZ","starttime","endtime","type","ballmark","speed","call")]

##convering meters into feet to apply standards of distance as per rules

for (i in 7:12){
  shots[,i] = as.numeric(shots[,i])*3.2808399
}

#ensuring that there are no space or any unwanted charecetrs in data

for(i in 1:ncol(shots)){
  
  shots[,i]<-gsub(" ","",shots[,i],fixed=T)
}

##sorting the data in order of shot id

shots<-shots[with(shots, order(shotid)), ]

##writing it into file

write.csv(shots,"shots.csv")

##############################################################################################################################################
##############################################################################################################################################

################################ <<<<Metric 1>>> #############################################

##Loading dataset shots into R Environment

shots<-read.csv("shots.csv",header=TRUE)

# Loading the libraries needed for exploratory analysis

library(ggplot2)
library(plyr)
library(gridExtra)

#Please ensure all libraries are loaded properly.

##constructing Standard Dimensional tennis court in a plot. with the below lines

df <- data.frame()
x <- c(-21,-21)
y <- c(13.5,-13.5)
line1 <- data.frame(x,y)

x <- c(21,21)
y <- c(13.5,-13.5)
line2 <- data.frame(x,y)

x <- c(21,-21)
y <- c(0,0)
line3 <- data.frame(x,y)

x <- c(39,-39)
y <- c(18,18)
outerline1 <- data.frame(x,y)

x <- c(-39,39)
y <- c(-18,-18)
outerline2 <- data.frame(x,y)

x <- c(-39,-39)
y <- c(18,-18)
outerline3 <- data.frame(x,y)

x <- c(39,39)
y <- c(18,-18)
outerline4 <- data.frame(x,y)

x <- c(39,-39)
y <- c(13.5,13.5)
innerline1 <- data.frame(x,y)

x <- c(39,-39)
y <- c(-13.5,-13.5)
innerline2 <- data.frame(x,y)

#function for plotting using ggplot

cgplot <-function(d,a){
  ggplot(d,a)+
    xlim(-60,60) + ylim(-30,30) +
    geom_line(data = outerline1,aes(x,y)) +
    geom_line(data = outerline2,aes(x,y)) +
    geom_line(data = outerline3,aes(x,y)) +
    geom_line(data = outerline4,aes(x,y)) +
    geom_line(data = innerline1,aes(x,y),size = 1) +
    geom_line(data = innerline2,aes(x,y),size = 1) +
    geom_vline(xintercept = 0, color = "Grey", size=1.4) + 
    geom_line(data = line1, aes(x, y)) +
    geom_line(data = line2, aes(x, y)) +
    geom_line(data = line3, aes(x,y)) + 
    geom_hline(yintercept=0, linetype=3) +
    theme(panel.background = element_rect(fill = 'light green', colour = 'Black'))+
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) + 
    theme(plot.title=element_text(size=18)) +
    theme(legend.title = element_text(size=14, face="bold"))+
    theme(legend.text = element_text(size = 12, face = "bold"))
}
##########################################################################################################

##extracting aces in total match

count <- count(shots,"rallieid")
sub <- subset(count,count$freq==1)
test <- subset(shots,shots$rallieid %in% sub$rallieid)
aces<- subset(test,test$call %in% c("in","est"))

#############################################################################################################
##Serve analysis for sakke33

#splitting dataset for player1
player1<-shots[shots$player=="sakke33",]
#collecting only servs data

servs<-player1[(player1$type == "first_serve" | player1$type == "second_serve"),]

##creating a column serv_result,which gives the actual result of that shot (specially includes Ace's in the respective "in" calls)
servs$serv_result<-as.character(servs$call)
servs$serv_result[servs$shotid %in% aces$shotid]<- "ace"
servs$serv_result<-as.factor(servs$serv_result)

##transposing coordinates

pX <- servs$sposX 
pY <- servs$sposY

servs$quadrant <- ifelse(pX>0 & pY>0, "I",ifelse(pX<0 & pY>0, "II", ifelse(pX<0 & pY<0,"III","IV")))

for(i in 1:nrow(servs)){
  if(servs[i,"quadrant"]=="II" || servs[i,"quadrant"]=="III"){ servs[i,"sposX"] = - servs[i,"sposX"];  servs[i,"sposY"] = - servs[i,"sposY"]; servs[i,"eposX"] = - servs[i,"eposX"];  servs[i,"eposY"] = - servs[i,"eposY"] } 
}

##shots$zone <- ifelse(shots$sposX <21,"0 to 21",ifelse(shots$sposX >21 & shots$sposX <= 30, "21 to 30", ifelse(shots$sposX >30 & shots$sposX  <=39, "30 to 39","Beyond 39")))

##splitting the servs dataset according to quadrents of court(rhf,lhf)
rhfsak<-servs[(servs$sposX>0 & servs$sposY>0),]
lhfsak<-servs[(servs$sposX>0 & servs$sposY<0),]


############################################################################################################################################################################

##Serve analysis for vesavee

#splitting dataset for player2
player2<-shots[shots$player=="vesavee",]
#collecting only servs data
servs<-player2[(player2$type == "first_serve" | player2$type == "second_serve"),]

##creating a column serv_result,which gives the actual result of that shot (specially includes Ace's in the respective "in" calls)
servs$serv_result<-as.character(servs$call)
servs$serv_result[servs$shotid %in% aces$shotid]<- "ace"
servs$serv_result<-as.factor(servs$serv_result)

##transposing coordinates

pX <- servs$sposX 
pY <- servs$sposY

servs$quadrant <- ifelse(pX>0 & pY>0, "I",ifelse(pX<0 & pY>0, "II", ifelse(pX<0 & pY<0,"III","IV")))

for(i in 1:nrow(servs)){
  if(servs[i,"quadrant"]=="II" || servs[i,"quadrant"]=="III"){ servs[i,"sposX"] = - servs[i,"sposX"];  servs[i,"sposY"] = - servs[i,"sposY"]; servs[i,"eposX"] = - servs[i,"eposX"];  servs[i,"eposY"] = - servs[i,"eposY"] } 
}

##shots$zone <- ifelse(shots$sposX <21,"0 to 21",ifelse(shots$sposX >21 & shots$sposX <= 30, "21 to 30", ifelse(shots$sposX >30 & shots$sposX  <=39, "30 to 39","Beyond 39")))

##splitting the servs dataset according to quadrents of court(rhf,lhf)
rhfvesa<-servs[(servs$sposX>0 & servs$sposY>0),]
lhfvesa<-servs[(servs$sposX>0 & servs$sposY<0),]

############################################################################################################################################################################################

##plotting the datasets of both the players, which are performance of their servs from left and right halfs.

p1<-cgplot(rhfsak,aes(eposX,eposY))+labs(title="Sakke33,Served From Right Half ")+
  geom_point(aes(color=factor(serv_result)),size=5,alpha=3.5/4)+
  xlab("") +
  ylab("") +
  labs(colour = "Result Of Serve")##righthalfplot
p2<-cgplot(lhfsak,aes(eposX,eposY))+geom_point(aes(color=factor(serv_result)),size=5,alpha=3.5/4)+
  xlab("") +
  ylab("") +
  labs(colour = "Result Of Serve")+labs(title="Sakke33,Served From Left Half ")##lefthalfplot
p3<-cgplot(rhfvesa,aes(eposX,eposY))+geom_point(aes(color=factor(serv_result)),size=5,alpha=3.5/4)+
  xlab("") +
  ylab("") +
  labs(colour = "Result Of Serve")+ggtitle("Vesavee,Served From Right Half ")##Righthalf polt
p4<-cgplot(lhfvesa,aes(eposX,eposY))+geom_point(aes(color=factor(serv_result)),size=5,alpha=3.5/4)+
  xlab("") +
  ylab("") +
  labs(colour = "Result Of Serve")+labs(title="Vesavee,Served From Left Half ")##lefthalfplot

# Plots will be saved Automatically to your corresponding working directories
png("~/ServAnalysisSakke_full_view.png",width=900,height=485) 
grid.arrange(p1,nrow=1,ncol=1)
dev.off()

png("~/ServAnalysisSakke33.png",width=900,height=485) 
grid.arrange(p1, p2,nrow=2,ncol=1)
dev.off()
png("~/ServAnalysisVesavee.png",width=900,height=485) 
grid.arrange(p3, p4,nrow=2,ncol=1)
dev.off()

########################################################################################################################################
########################################################################################################################################
################### <<<< Metric 2 >>>> ########################

#setwd("~/R/ATP/Data")
shots <- read.csv("shots.csv")

#Loading required libraries
library(ggplot2)
#install.packages("gridExtra")
#library(gridExtra)
library(dplyr)


#Transposing start points
x <- shots$sposX 
y <- shots$sposY


shots$quadrant <- ifelse(x>0 & y>0, "I",ifelse(x<0 & y>0, "II", ifelse(x<0 & y<0,"III","IV")))

for(i in 1:nrow(shots)){
  if(shots[i,"quadrant"]=="II" || shots[i,"quadrant"]=="III"){ shots[i,"sposX"] = - shots[i,"sposX"];  shots[i,"sposY"] = - shots[i,"sposY"]} 
}

# Classifying shot zones
shots$zone <- ifelse(shots$sposX <21,"0 to 21",ifelse(shots$sposX >21 & shots$sposX <= 30, "21 to 30", ifelse(shots$sposX >30 & shots$sposX  <=39, "30 to 39","Beyond 39")))

#Finding last shots of each rally
gpByRally <- group_by(shots, rallieid)
lastShots <- summarize(gpByRally, max.pt = max(shotid))
# Extracting complete shot details of winning shots
winShots <- subset(shots,shots$shotid %in% lastShots$max.pt)
# Subsetting out the winning shots that were not serves
noServWins <- winShots[!(winShots$type %in% c("first_serve","second_serve")),]


# Analysing only the subset where Sakke is serving

sakke33 <- subset(shots, shots$player == "sakke33")

# FOR SERVE GAMES
# Finding rallies where he served
serving <- subset(sakke33, sakke33$type %in% c("first_serve","second_serve"))
# Subsetting all the rallies of the games where he served
serveGames <- subset(sakke33, sakke33$rallieid %in% serving$rallieid)
# Subsetting winning shots where he did not score an ace
ServeWins <- na.omit(subset(serveGames, serveGames$shotid %in% noServWins$shotid & serveGames$call %in% c("in","est")))
ServeLoss <- na.omit(subset(serveGames, serveGames$shotid %in% noServWins$shotid & serveGames$call %in% c("out","net")))

# FOR Non-SERVE GAMES

# Finding all the rallies of the games where he did not serve
nonserveGames <- subset(sakke33, !(sakke33$rallieid %in% serveGames$rallieid))
# Subsetting winning shots where he did not score an ace
NonServeWins <- na.omit(subset(nonserveGames, nonserveGames$shotid %in% noServWins$shotid & nonserveGames$call %in% c("in","est")))
NonServeLoss <- na.omit(subset(nonserveGames, nonserveGames$shotid %in% noServWins$shotid & nonserveGames$call %in% c("out","net")))

#plotting data

#setwd("~/R/ATP/Data/Plots")

plot1 <- cgplot(ServeWins,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ggtitle("Serving Games")+
  ylab("Wins")+xlab("")+
  labs(colour = "Shot type")
plot2 <- cgplot(NonServeWins,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ggtitle("Receiving Games") +
  ylab("Wins")+xlab("")+
  labs(colour = "Shot type")
plot3 <- cgplot(ServeLoss,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ylab("Losses")+xlab("")+
  labs(colour = "Shot type")
plot4 <- cgplot(NonServeLoss,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ylab("Losses")+xlab("")+
  labs(colour = "Shot type")

png("~/SakkeServing.png",width = 900, height = 485)
grid.arrange(plot1,plot3,nrow=2,ncol=1, top = ("Sakke"))
dev.off()
png("~/SakkeReceiving.png",width = 900, height = 485)
grid.arrange(plot2,plot4,nrow=2,ncol=1, top = ("Sakke"))
dev.off()

#sample Density plot

scatter <- cgplot(ServeWins,aes(sposX,sposY))+
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  xlab("")+ylab("")+
  theme(legend.position=c(0,1), legend.justification=c(0,1),legend.title = element_blank())

xdensity <- ggplot(ServeWins,aes(sposX)) +
  xlim(-60,60) +
  geom_density()

ydensity <- ggplot(ServeWins,aes(sposY)) +
  xlim(-30,30) +
  geom_density() +
  coord_flip()

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )


png("~/SampleDensityPlot.png",width = 900, height = 485)

grid.arrange(xdensity, blankPlot, scatter, ydensity, 
             ncol=2, nrow=2, widths=c(4, 0.7), heights=c(1.4, 4), top = ("Density Profile of Sakke Winning Shots while Serving"))

dev.off()


# Analysing only the subset where Vesavee is serving

vesavee <- subset(shots, shots$player == "vesavee")

# FOR SERVE GAMES
# Finding rallies where he served
serving <- subset(vesavee, vesavee$type %in% c("first_serve","second_serve"))
# Subsetting all the rallies of the games where he served
serveGames <- subset(vesavee, vesavee$rallieid %in% serving$rallieid)
# Subsetting winning shots where he did not score an ace
ServeWins <- na.omit(subset(serveGames, serveGames$shotid %in% noServWins$shotid & serveGames$call %in% c("in","est")))
ServeLoss <- na.omit(subset(serveGames, serveGames$shotid %in% noServWins$shotid & serveGames$call %in% c("out","net")))

# Finding all the rallies of the games where he did not serve
nonserveGames <- subset(vesavee, !(vesavee$rallieid %in% serveGames$rallieid))
# Subsetting winning shots where he did not score an ace
NonServeWins <- na.omit(subset(nonserveGames, nonserveGames$shotid %in% noServWins$shotid & nonserveGames$call %in% c("in","est")))
NonServeLoss <- na.omit(subset(nonserveGames, nonserveGames$shotid %in% noServWins$shotid & nonserveGames$call %in% c("out","let")))


#plotting data

#setwd("~/R/ATP/Data/Plots")

plot1 <- cgplot(ServeWins,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ggtitle("Serving Games")+
  ylab("Wins")+xlab("")+
  labs(colour = "Shot type")
plot2 <- cgplot(NonServeWins,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ggtitle("Receiving Games") +
  ylab("Wins")+xlab("")+
  labs(colour = "Shot type")
plot3 <- cgplot(ServeLoss,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ggtitle("Receiving Games") +
  ylab("Wins")+xlab("")+
  labs(colour = "Shot type")
plot4 <- cgplot(NonServeLoss,aes(sposX,sposY)) +
  geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
  ylab("Losses")+xlab("")+
  labs(colour = "Shot type")

png("~/VesaveeServing.png",width = 900, height = 485)
grid.arrange(plot1,plot3,nrow=2,ncol=1, top = ("Vesavee"))
dev.off()
png("~/VesaveeReceiving.png",width = 900, height = 485)
grid.arrange(plot2,plot4,nrow=2,ncol=1, top = ("Vesavee")) 
dev.off()

########################################################################################################################################
########################################################################################################################################

############################## <<<<< METRIC 3 >>>>> #######################################


#setwd("~/R/ATP/Data")
shots <- read.csv("shots.csv",header=TRUE)
unloadNamespace("dplyr")
library(plyr)


#Transposing points

#Transposing start positions
x <- shots$sposX 
y <- shots$sposY

shots$quadrant <- ifelse(x>0 & y>0, "I",ifelse(x<0 & y>0, "II", ifelse(x<0 & y<0,"III","IV")))

for(i in 1:nrow(shots)){
  if(shots[i,"quadrant"]=="II" || shots[i,"quadrant"]=="III"){ shots[i,"sposX"] = - shots[i,"sposX"];  shots[i,"sposY"] = - shots[i,"sposY"]} 
}

#Transposing end positions
x <- shots$eposX 
y <- shots$eposY

shots$quadrant <- ifelse(x>0 & y>0, "I",ifelse(x<0 & y>0, "II", ifelse(x<0 & y<0,"III","IV")))

for(i in 1:nrow(shots)){
  if(shots[i,"quadrant"]=="II" || shots[i,"quadrant"]=="III"){ shots[i,"eposX"] = - shots[i,"eposX"];  shots[i,"eposY"] = - shots[i,"eposY"]} 
}

# Adding speed zone factors

shots$speedZone <- ifelse(shots$speed <20,"Slow",ifelse(shots$speed >=20 & shots$sposX <= 40, "Medium", ifelse(shots$sposX >40 & shots$sposX  <=55, "Fast","NA")))
shots$speedZone <- as.factor(shots$speedZone)

# Splitting rallies into data frames and ignoring rallies with only serves
rallieCounts <- count(shots,"rallieid")
valid <- rallieCounts[rallieCounts$freq == 1,] 
data <- subset(shots,!(shots$rallieid %in% valid$rallieid))

ralliesplit <- split(data,data$rallieid)

# Creating data frame with distance
DistDF <- data.frame()

for (i in 1:length(ralliesplit)){
  
  n = nrow(ralliesplit[[i]])
  
  for (j in 2:n){
    
    startPosX <- ralliesplit[[i]][j-1,"sposX"]
    startPosY <- ralliesplit[[i]][j-1,"sposY"]
    endPosX <- ralliesplit[[i]][j,"eposX"]
    endPosY <- ralliesplit[[i]][j,"eposY"]
    
    xdiff <- 39 - startPosX
    ydiff <- -startPosY
    
    endPosX <- endPosX + xdiff
    endPosY <- endPosY + ydiff
    startPosX <- 39
    startPosY <- 0
    
    d = sqrt((endPosX - startPosX)^2+(endPosY - startPosY)^2)
    
    rallieid <- as.character(ralliesplit[[i]][j-1,"rallieid"])
    shotid <- (ralliesplit[[i]][j-1,"shotid"])
    player <- as.character(ralliesplit[[i]][j-1,"player"])
    speed <- ralliesplit[[i]][j,c("speed","speedZone")]
    call <- as.character(ralliesplit[[i]][j,"call"])
    DistDF <- rbind(DistDF,(cbind(rallieid,shotid,player,startPosX,startPosY,endPosX,endPosY,d,call,speed)))
    
    
  }
}

#Splitting distance data frame to find out failure distances

DistSplit <- split(DistDF,DistDF$rallieid)
infailureD <- data.frame()

for (i in 1:length(ralliesplit)){
  if(ralliesplit[[i]][nrow(ralliesplit[[i]]),"call"] %in% c("in","est")){
    ref <- DistSplit[[i]][nrow(DistSplit[[i]]),]
    infailureD <- rbind(infailureD,ref)    
  }
}

sakkeInFailure <- subset(infailureD,infailureD$player == "sakke33")
vesaveeInFailure <- subset(infailureD,infailureD$player == "vesavee")

insuccessD <- subset(DistDF,DistDF$call == "in" & !(DistDF$shotid %in% infailureD$shotid))

sakkeInSuccess <- subset(insuccessD,insuccessD$player == "sakke33")
vesaveeInSuccess <- subset(insuccessD,insuccessD$player == "vesavee")

plot1 <- cgplot(vesaveeInFailure,aes(endPosX,endPosY)) +
  geom_point(aes(39,0),color="red",shape = 13,size=5) +
  geom_point(aes(color=factor(speedZone)),size=4,alpha=3/4) +
  ylab("Failure")+
  xlab("")+
  theme(legend.position=c(0,1), legend.justification=c(0,1)) +
  labs(colour = "Speed Zone")

plot2 <- cgplot(sakkeInFailure,aes(endPosX,endPosY)) +
  geom_point(aes(39,0),color="red",shape = 13,size=5) +
  geom_point(aes(color=factor(speedZone)),size=4,alpha=3/4) +
  ylab("Failure")+
  xlab("")+
  theme(legend.position=c(0,1), legend.justification=c(0,1)) +
  labs(colour = "Speed Zone")

plot3 <- cgplot(vesaveeInSuccess,aes(endPosX,endPosY)) +
  geom_point(aes(39,0),color="red",shape = 13,size=5) +
  geom_point(aes(color=factor(speedZone)),size=4,alpha=3/4) +
  ylab("Success")+
  xlab("")+
  theme(legend.position=c(0,1), legend.justification=c(0,1)) +
  labs(colour = "Speed Zone")

plot4 <- cgplot(sakkeInSuccess,aes(endPosX,endPosY)) +
  geom_point(aes(39,0),color="red",shape = 13,size=5) +
  geom_point(aes(color=factor(speedZone)),size=4,alpha=3/4) +
  ylab("Success")+
  xlab("")+
  geom_point(aes(color=factor(speedZone)),size=4,alpha=3/4) +
  theme(legend.position=c(0,1), legend.justification=c(0,1)) +
  labs(colour = "Speed Zone")


png("~/SakkeM3.png",width = 900, height = 485)
grid.arrange(plot4,plot2,nrow=2,ncol=1, top = ("Sakke"))
dev.off()
png("~/VesaveeM3.png",width = 900, height = 485)
grid.arrange(plot3,plot1,nrow=2,ncol=1, top = ("Vesavee"))
dev.off()

###################################################################################################################################################################################################
###################################################################################################################################################################################################

################################ <<<<Metric 4>>> #############################################

##read dataset from working directory
#setwd("~/R/ATP/Data")
shots <- read.csv("shots.csv")
library(dplyr)

#install.packages("aspace",dependencies = TRUE)
library(aspace)


gpByRally <- group_by(shots, rallieid)
lastShots <- summarize(gpByRally, max.pt = max(shotid))

shots$lastShot <- ifelse(shots$shotid %in% lastShots$max.pt, "yes","no")
# Splitting rallies into data frames and ignoring rallies with only serves
unloadNamespace("dplyr")
library(plyr)
rallieCounts <- count(shots,"rallieid")
valid <- rallieCounts[rallieCounts$freq == 1,] 
data <- subset(shots,!(shots$rallieid %in% valid$rallieid))
ralliesplit <- split(data,data$rallieid)

# Creating data frame with angle(theta)
angleDF <- data.frame()

for (i in 1:length(ralliesplit)){
  
  n = nrow(ralliesplit[[i]])
  
  for (j in 2:n){
    
    rallieid <- ralliesplit[[i]][j,"rallieid"]
    shotid <- ralliesplit[[i]][j,"shotid"]
    player <- as.character(ralliesplit[[i]][j,"player"])
    ep1X <- ralliesplit[[i]][j-1,"eposX"]
    ep1Y <- ralliesplit[[i]][j-1,"eposY"]
    spX <- ralliesplit[[i]][j,"sposX"]
    spY <- ralliesplit[[i]][j,"sposY"]    
    ep2X <- ralliesplit[[i]][j,"eposX"]
    ep2Y <- ralliesplit[[i]][j,"eposY"]
    
    m1 <- (spY-ep1Y)/(spX-ep1X)
    m2 <- (ep2Y-spY)/(ep2X-spX)
    
    theta <- atan_d((m1-m2)/(1+m1*m2)) 
    
    call <- as.character(ralliesplit[[i]][j,"call"])
    lastshot <- as.character(ralliesplit[[i]][j,"lastShot"])
    nextcall <- as.character(ralliesplit[[i]][j+1,"call"])
    status <- ifelse(call %in% c("in","est") & (lastshot == "yes" || nextcall %in% c("out","net")),"Point","No Point")
    
    angleDF <- rbind(angleDF,(cbind(rallieid,shotid,player,ep1X,ep1Y,spX,spY,ep2X,ep2Y,m1,m2,theta,call,status)))
    
    
  }
}

#converting the datatypes of the terms,which we need to plot.

for(i in c(1,2,4:9)){
  
  angleDF[,i]<-as.numeric(as.character(angleDF[,i]))
}

sapply(angleDF,class)


##transposing the co ordinates assuming the play from one side of the court

pX <- angleDF$spX
pY <- angleDF$spY

angleDF$quadrant <- ifelse(pX>0 & pY>0, "I",ifelse(pX<0 & pY>0, "II", ifelse(pX<0 & pY<0,"III","IV")))

for(i in 1:nrow(angleDF)){
  if(angleDF[i,"quadrant"]=="II" || angleDF[i,"quadrant"]=="III"){ angleDF[i,"spX"] = - angleDF[i,"spX"];  angleDF[i,"spY"] = - angleDF[i,"spY"]; angleDF[i,"ep1X"] = - angleDF[i,"ep1X"];  angleDF[i,"ep1Y"] = - angleDF[i,"ep1Y"] ;angleDF[i,"ep2X"] = - angleDF[i,"ep2X"];  angleDF[i,"ep2Y"] = - angleDF[i,"ep2Y"]} 
}

##interploating all the points assuming that the player stand at one position of the court

angleDF$pep1X <- 0
angleDF$pep2X <- 0
angleDF$pep1Y <- 0 
angleDF$pep2Y <- 0 


for (i in 1:nrow(angleDF)){
  
  xdiff <- 39 - angleDF$spX[i] 
  ydiff <- - angleDF$spY[i]
  
  
  angleDF$pep1X[i] <- angleDF$ep1X[i] + xdiff 
  angleDF$pep2X[i] <- angleDF$ep2X[i] + xdiff
  angleDF$pep1Y[i] <- angleDF$ep1Y[i] + ydiff 
  angleDF$pep2Y[i] <- angleDF$ep2Y[i] + ydiff 
  angleDF$pspX[i] <- 39 
  angleDF$pspY[i] <- 0 
  
}

#splitting the data for each player

sakke<-angleDF[angleDF$player =="sakke33",]
vesavee<-angleDF[angleDF$player =="vesavee",]
#developing Gplots

png("~/sakke_theta_analysis.png",width=900,height=485)
cgplot(sakke,aes(pep1X,pep1Y))+ylim(-40,40)+
  geom_point(col="red", size=2) +
  geom_point(aes(pspX,pspY),col="orange", size=5) + 
  geom_point(aes(pep2X,pep2Y),col="yellow", size=2) +
  geom_segment(data = sakke,aes(x=pep1X, y=pep1Y, xend=pspX, yend=pspY)) + 
  geom_segment(data = sakke,aes(x=pspX, y=pspY, xend=pep2X, yend=pep2Y))+
  labs(title="sakke theta analysis") + 
  facet_grid( status ~ .)+xlab("")+ylab("")
dev.off()
png("~/vesavee_theta_analysis.png",width=900,height=485)
cgplot(vesavee,aes(pep1X,pep1Y))+ylim(-40,40)+
  geom_point(col="red", size=2) +
  geom_point(aes(pspX,pspY),col="orange", size=5) + 
  geom_point(aes(pep2X,pep2Y),col="yellow", size=2) +
  geom_segment(data = vesavee,aes(x=pep1X, y=pep1Y, xend=pspX, yend=pspY)) + 
  geom_segment(data = vesavee,aes(x=pspX, y=pspY, xend=pep2X, yend=pep2Y))+
  labs(title="vesavee theta analysis") + 
  facet_grid(status ~ .)+xlab("")+ylab("")
dev.off()
