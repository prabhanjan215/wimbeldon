##read dataset from working directory
#setwd("~/R/ATP/Data")
shots <- read.csv("shots.csv")
library(ggplot2)
library(plyr)
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
oneralliedata<-shots[shots$rallieid %in% valid$rallieid,]
separatingaces<-oneralliedata[oneralliedata$call == "in",]
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
    theta = atan_d((m2-m1)/(1+m1*m2))
    
    type<-as.character(ralliesplit[[i]][j,"type"])
    call <- as.character(ralliesplit[[i]][j,"call"])
    lastshot <- as.character(ralliesplit[[i]][j,"lastShot"])
    #nextcall <- as.character(ralliesplit[[i]][j+1,"call"])
    status <- ifelse(call %in% c("in","est") & (lastshot == "yes"),"Point","continued")
    angleDF <- rbind(angleDF,(cbind(rallieid,shotid,player,ep1X,ep1Y,spX,spY,ep2X,ep2Y,m1,m2,theta,call,type,status,lastshot)))
    
    
  }
}

#converting the datatypes of the terms,which we need to plot.

for(i in c(1,2,4:12)){
  
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

#saving copy

copy<-angleDF

#plottingfor test

cgplot(copy,aes(ep1X,ep1Y))+
  geom_point(col="red", size=2) +
  geom_point(aes(spX,spY),col="orange", size=5) + 
  geom_point(aes(ep2X,ep2Y),col="yellow", size=2) +
  geom_segment(data = copy,aes(x=ep1X, y=ep1Y, xend=spX, yend=spY)) + 
  geom_segment(data = copy,aes(x=spX, y=spY, xend=ep2X, yend=ep2Y))+
  labs(title="sakke theta analysis") + 
  facet_grid( status ~ .)+xlab("")+ylab("")


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

sapply(angleDF,class)

angleDF$theta<-as.numeric(angleDF$theta)
angleDF$theta<-replace(angleDF$theta,is.nan(angleDF$theta),0)
#splitting the data for each player

sakke<-angleDF[angleDF$player =="sakke33",]
vesavee<-angleDF[angleDF$player =="vesavee",]

#categorizing the data for in and made point or no point

sakkeintheta<-sakke[sakke$call %in% c("in","est"),]

vesaveeintheta<-vesavee[vesavee$call %in% c("in","est"),]

##rotating the shots


main<-sakkeintheta[,c(1:2,12:16,18,20,19,21,22,23)]

px1<-39
py1<-0

#p12<-sqrt((px1 - px2)^2 + (py1 - py2)^2)

for(i in 1:nrow(main)){
  
  px3<-main$pep1X[i]
  py3<-main$pep1Y[i]
  
  p13<-sqrt((px1 - px3)^2 + (py1 - py3)^2)
  
  #p23<-sqrt((px2 - px3)^2 + (py2 - py3)^2)
  
  main$nep1X[i]<-(39-p13*cos_d(0))
  main$nep1Y[i]<-(p13*sin_d(0))
  
  
}



main$nep2X <- 0
main$nep2Y <- 0

for (i in 1: nrow(main)){
  
  xdiff <- main$nep1X - main$pep1X 
  ydiff <- main$nep1Y - main$pep1Y
  
  
  main$nep2X <- main$pep2X + xdiff
  main$nep2Y <- main$pep2Y + ydiff
  
  
}

#developing Gplots

#plot function

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

#plotting using the function cgplot

png("~/sakke_theta_analysis.png",width=900,height=485)
cgplot(main,aes(nep1X,nep1Y))+ylim(-40,40)+
  geom_point(col="red", size=5)+
  geom_point(aes(pspX,pspY),col="orange", size=5) + 
  geom_point(aes(nep2X,nep2Y),col="yellow", size=3) +
  geom_segment(data = main,aes(x=nep1X, y=nep1Y, xend=pspX, yend=pspY)) + 
  geom_segment(data = main,aes(x=pspX, y=pspY, xend=nep2X, yend=nep2Y,color = factor(type)))+
  labs(title="sakke theta analysis") + 
  facet_grid( status ~ .)+xlab("")+ylab("")+labs(colour = "Type of Shot")
dev.off()

thetadf<-main[main$type == "stroke_left",]
 

##plotting theta of vesavee

main<-vesaveeintheta[,c(1:2,12:16,18,20,19,21,22,23)]

px1<-39
py1<-0

#p12<-sqrt((px1 - px2)^2 + (py1 - py2)^2)

for(i in 1:nrow(main)){
  
  px3<-main$pep1X[i]
  py3<-main$pep1Y[i]
  
  p13<-sqrt((px1 - px3)^2 + (py1 - py3)^2)
  
  #p23<-sqrt((px2 - px3)^2 + (py2 - py3)^2)
  
  main$nep1X[i]<-(39-p13*cos_d(0))
  main$nep1Y[i]<-(p13*sin_d(0))
  
}

main$nep2X <- 0
main$nep2Y <- 0

for (i in 1: nrow(main)){
  
  xdiff <- main$nep1X - main$pep1X 
  ydiff <- main$nep1Y - main$pep1Y
  
  
  main$nep2X <- main$pep2X + xdiff
  main$nep2Y <- main$pep2Y + ydiff
  
  
}

#plotting using the function cgplot
png("~/vesavee_theta_analysis.png",width=900,height=485)
cgplot(main,aes(nep1X,nep1Y))+ylim(-40,40)+
  geom_point(col="red", size=5) +
  geom_point(aes(pspX,pspY),col="orange", size=5) + 
  geom_point(aes(nep2X,nep2Y),col="yellow", size=3) +
  geom_segment(data = main,aes(x=nep1X, y=nep1Y, xend=pspX, yend=pspY)) + 
  geom_segment(data = main,aes(x=pspX, y=pspY, xend=nep2X, yend=nep2Y,color = factor(type)))+
  labs(title="vesavee theta analysis") + 
  facet_grid(status ~ .)+xlab("")+ylab("")+labs(colour = "Type of Shot")
dev.off()


###bootstrapping

#exp1:
install.packages("UsingR",dependencies=TRUE)
require(UsingR)

y<-main$theta

n<-length(y)

b<-10000

resamples<-matrix(sample(y,n*b,replace=TRUE),b,n)
dim(resamples)
resampleMean<-apply(resamples,1,mean)
length(resampleMean)
hist(resampleMean)
sd(resampleMean)
quantile(resampleMean, c(.025,.5,.975))

ggplot(data.frame(resampleMean),aes(resampleMean))+geom_histogram(color="black",fill="lightblue",binwidth=0.05)
