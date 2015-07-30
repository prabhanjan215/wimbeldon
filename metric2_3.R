#Metric 2

setwd("~/R/ATP/Data")
shots <- read.csv("shots.csv")

#Loading required libraries
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)
library(dplyr)


#plots code

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

cgplot <-function(d,a){
  ggplot(d,a)+
    xlim(-60,60) + ylim(-30,30) +
    geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
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
    theme(panel.background = element_rect(fill = 'light green', colour = 'Black'), axis.title.x = element_blank()) +
    labs(colour = "Shot type")
}

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

setwd("~/R/ATP/Data/Plots")

plot1 <- cgplot(ServeWins,aes(sposX,sposY)) + ggtitle("Serving Games")+ylab("Wins")
plot2 <- cgplot(NonServeWins,aes(sposX,sposY)) + ggtitle("Receiving Games") + ylab("Wins")
plot3 <- cgplot(ServeLoss,aes(sposX,sposY)) + ylab("Losses")
plot4 <- cgplot(NonServeLoss,aes(sposX,sposY)) + ylab("Losses")

png("C:/Users/ajith_patnaik/Documents/R/ATP/Data/Plots/SakkeServing.png",width = 900, height = 485)
grid.arrange(plot1,plot3,nrow=2,ncol=1, top = ("Sakke"))
dev.off()
png("C:/Users/ajith_patnaik/Documents/R/ATP/Data/Plots/SakkeReceiving.png",width = 900, height = 485)
grid.arrange(plot2,plot4,nrow=2,ncol=1, top = ("Sakke"))
dev.off()
# Analysing only the subset where Sakke is serving

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

setwd("~/R/ATP/Data/Plots")

plot1 <- cgplot(ServeWins,aes(sposX,sposY)) + ggtitle("Serving Games")+ylab("Wins")
plot2 <- cgplot(NonServeWins,aes(sposX,sposY)) + ggtitle("Receiving Games") + ylab("Wins") 
plot3 <- cgplot(ServeLoss,aes(sposX,sposY)) + ylab("Losses")
plot4 <- cgplot(NonServeLoss,aes(sposX,sposY)) + ylab("Losses")

png("C:/Users/ajith_patnaik/Documents/R/ATP/Data/Plots/VesaveeServing.png",width = 900, height = 485)
grid.arrange(plot1,plot3,nrow=2,ncol=1, top = ("Vesavee"))
dev.off()
png("C:/Users/ajith_patnaik/Documents/R/ATP/Data/Plots/VesaveeReceiving.png",width = 900, height = 485)
grid.arrange(plot2,plot4,nrow=2,ncol=1, top = ("Vesavee")) 
dev.off()

#sample Density plot

sampleplot <-function(d,a){
  ggplot(d,a)+
    xlim(-60,60) + ylim(-30,30) +
    geom_point(aes(color=factor(type)),size=5,alpha=3/4)+
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
    theme(panel.background = element_rect(fill = 'light green', colour = 'Black'), axis.title.x = element_blank(),axis.title.y = element_blank()) +
    theme(legend.position=c(0,1), legend.justification=c(0,1),legend.title = element_blank())
}

scatter <- sampleplot(ServeWins,aes(sposX,sposY))

xdensity <- ggplot(ServeLoss,aes(sposX)) +
  xlim(-60,60) +
  geom_density()

ydensity <- ggplot(ServeLoss,aes(sposY)) +
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


png("C:/Users/ajith_patnaik/Documents/R/ATP/Data/Plots/SamplePlot.png",width = 900, height = 485)

grid.arrange(xdensity, blankPlot, scatter, ydensity, 
             ncol=2, nrow=2, widths=c(4, 0.7), heights=c(1.4, 4), top = ("Density Profile of Sakke Winning Shots while Serving"))

dev.off()


#METRIC 3


setwd("~/R/ATP/Data")
shots <- read.csv("shots.csv")
detach("package:dplyr", unload=TRUE)
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


m3plot <- function(d,a){ggplot(d,a) +
                          xlim(-60,60) + ylim(-30,30) +
                          geom_point(aes(color=factor(speedZone)),size=4,alpha=3/4) +
                          geom_point(aes(39,0),color="red",shape = 13,size=5) +
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
                          theme(panel.background = element_rect(fill = 'light green', colour = 'Black'), axis.title.x = element_blank()) +
                          theme(legend.position=c(0,1), legend.justification=c(0,1)) +
                          labs(colour = "Speed Zone")
}


plot1 <- m3plot(vesaveeInFailure,aes(endPosX,endPosY)) + ylab("Failure")
plot2 <- m3plot(sakkeInFailure,aes(endPosX,endPosY)) + ylab("Failure")
plot3 <- m3plot(vesaveeInSuccess,aes(endPosX,endPosY)) + ylab("Success")
plot4 <- m3plot(sakkeInSuccess,aes(endPosX,endPosY)) + ylab("Success")


png("C:/Users/ajith_patnaik/Documents/R/ATP/Data/Plots/Sakke.png",width = 900, height = 485)
grid.arrange(plot4,plot2,nrow=2,ncol=1, top = ("Sakke"))
dev.off()
png("C:/Users/ajith_patnaik/Documents/R/ATP/Data/Plots/Vesavee.png",width = 900, height = 485)
grid.arrange(plot3,plot1,nrow=2,ncol=1, top = ("Vesavee"))
dev.off()

