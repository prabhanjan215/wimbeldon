##Loading dataset shots into R Environment

shots<-read.csv("shots.csv",header=TRUE)

# Loading the libraries needed for exploratory analysis
#install.packages("Rmisc")#use it if you dont have the package and to install it
library(ggplot2)
library(plyr)
library(Rmisc)

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
extras<-

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
png("~/ServAnalysisSakke33sam.png",width=900,height=485) 
multiplot(p1,cols=1)
dev.off()

png("~/ServAnalysisSakke33.png",width=900,height=485) 
multiplot(p1, p2,cols=1)
dev.off()
png("~/ServAnalysisVesavee.png",width=900,height=485) 
multiplot(p3, p4,cols=1)
dev.off()
