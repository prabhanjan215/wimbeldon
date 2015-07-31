#Metric 5

#setwd("~/R/ATP/Data")
shots <- read.csv("shots.csv")

library(plyr)

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

shots$speedZone <- ifelse(shots$speed <20,"Slow speed",ifelse(shots$speed >=20 & shots$sposX <= 40, "Medium speed", ifelse(shots$sposX >40 & shots$sposX  <=55, "Fast speed","NA")))
shots$speedZone <- as.factor(shots$speedZone)

# Adding height zone factors

shots$heightZone <- ifelse(shots$sposZ < 3,"low",ifelse(shots$sposZ >=3 & shots$sposZ <= 6, "Medium", ifelse(shots$sposZ > 6 , "High","NA")))
shots$heightZone <- as.factor(shots$heightZone)


rallieCounts <- count(shots,"rallieid")
valid <- rallieCounts[rallieCounts$freq == 1,] 
data <- subset(shots,!(shots$rallieid %in% valid$rallieid))

ralliesplit <- split(data,data$rallieid)

heightDF <- data.frame()

for (i in 1:length(ralliesplit)){
  
  n = nrow(ralliesplit[[i]])
  
  for (j in 2:n){
    
    rallieid <- ralliesplit[[i]][j,"rallieid"]
    shotid <- ralliesplit[[i]][j,"shotid"]
    player <- as.character(ralliesplit[[i]][j,"player"])
    epX <- ralliesplit[[i]][j-1,"eposX"]
    epY <- ralliesplit[[i]][j-1,"eposY"]
    spX <- ralliesplit[[i]][j,"sposX"]
    spY <- ralliesplit[[i]][j,"sposY"]
    
    speed <- as.character(ralliesplit[[i]][j-1,"speedZone"])
    height <- as.character(ralliesplit[[i]][j,"heightZone"])
    call <- as.character(ralliesplit[[i]][j,"call"])
    
    heightDF <- rbind(heightDF,(cbind(rallieid,shotid,player,epX,epY,spX,spY,speed,height,call)))
    
    
  }
}

heightDF$pepX <- 0
heightDF$pepY <- 0


for(i in c(4:7)){
  
  heightDF[,i]<-as.numeric(as.character(heightDF[,i]))
}



for (i in 1:nrow(heightDF)){
  
  xdiff <- 39 - heightDF$spX[i] 
  ydiff <- - heightDF$spY[i]
  
  
  heightDF$pepX[i] <- heightDF$epX[i] + xdiff 
  heightDF$pepY[i] <- heightDF$epY[i] + ydiff
  heightDF$pspX[i] <- 39 
  heightDF$pspY[i] <- 0 
  
}

#splitting the data for each player

sakke<-heightDF[heightDF$player =="sakke33",]
vesavee<-heightDF[heightDF$player =="vesavee",]


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
    ylim(limits=c(-30, 30))+
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
    geom_point(aes(pepX,pepY),col="red", size=2) +
    geom_segment(data = d,aes(x=pepX, y=pepY, xend=pspX, yend=pspY, colour = factor(speed)))
}

#developing Gplots
png("sakke_speedVSheight.png",width=900,height=600)
tgplot(sakke,aes(pepX,pepY)) + labs(title="Sakke's Height vs Speed Analysis") + facet_grid(height ~ .) + xlab("") + ylab("")
dev.off()
png("vesavee_speedVSheight.png",width=900,height=600)
tgplot(vesavee,aes(pepX,pepY))+labs(title="Vesavee Height vs Speed Analysis") + facet_grid(height ~ .) + xlab("") + ylab("")
dev.off()
