##read dataset from working directory
shots <- read.csv("shots.csv")
#detach("package:dplyr", unload=TRUE)
library(plyr)
#install.packages("aspace",dependencies = TRUE)
library(aspace)

# Splitting rallies into data frames and ignoring rallies with only serves
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
    status <- ifelse(call %in% c("in","est"),"Good","Bad")
    
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
  angleDF$pep1Y[i] <- angleDF$ep1X[i] + ydiff 
  angleDF$pep2Y[i] <- angleDF$ep2X[i] + ydiff 
  angleDF$pspX[i] <- 39 
  angleDF$pspY[i] <- 0 
  
}

#splitting the data for each player

sakke<-angleDF[angleDF$player =="sakke33",]
vesavee<-angleDF[angleDF$player =="vesavee",]

##plotting function

library(ggplot2)

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

tgplot <-function(d,a){
  ggplot(d,a) + 
    xlim(limits=c(-60, 60))+
    ylim(limits=c(-60, 60))+
    geom_point(col="red", size=2) +
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
    geom_point(aes(pspX,pspY),col="orange", size=5) + 
    geom_point(aes(pep2X,pep2Y),col="yellow", size=2) +
    geom_segment(data = d,aes(x=pep1X, y=pep1Y, xend=pspX, yend=pspY, color = factor(status))) + 
    geom_segment(data = d,aes(x=pspX, y=pspY, xend=pep2X, yend=pep2Y,color = factor(status)))
}

#developing Gplots
png("~/sakke_theta_analysis.png",width=900,height=600)
tgplot(sakke,aes(pep1X,pep1Y))+labs(title="sakke theta analysis")
dev.off()
png("~/vesavee_theta_analysis.png",width=900,height=600)
tgplot(vesavee,aes(pep1X,pep1Y))+labs(title="vesavee theta analysis")
dev.off()
