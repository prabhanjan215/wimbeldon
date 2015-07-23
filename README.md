# wimbeldon
data analysis

##load mmshots data set in before
# Loading the libraries needed for exploratory analysis

library(ggplot2)
library(dplyr)

#function for plotting using ggplot

cgplot <-function(d,a){
  ggplot(d,a)+
    xlim(-60,60) + ylim(-30,30) +
    geom_point(aes(color=factor(serv_result)),size=4,alpha=3/4)+
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
    theme(panel.background = element_rect(fill = 'light green', colour = 'Black'))
}
##########################################################################################################

##Serve analysis for sakke33

#splitting dataset for player1
player1<-mmshots[mmshots$player=="sakke33",]
#collecting only servs data
servs<-player1[(player1$type == "first_serve" | player1$type == "second_serve"),]

#grouping player shots by rallies, to obtain final shot of the each rally
gpByRally <- group_by(player1, rallieid)
lastShots <- summarize(gpByRally, max.pt = max(shotid))
winShots <- subset(player1,player1$shotid %in% lastShots$max.pt)

#classifying winshots into servshots(finding Ace shots)
ServWins <- winShots[(winShots$type == "first_serve" | winShots$type == "second_serve") & (winShots$call %in% c("in","est")),]

##creating a column serv_result,which gives the actual result of that shot (specially includes Ace's in the respective "in" calls)
servs$serv_result<-servs$call
servs$serv_result<-as.character(servs$serv_result)
servs$serv_result[servs$shotid %in% ServWins$shotid]<- "ace"
servs$serv_result<-as.factor(servs$serv_result)
sapply(servs,class)

##splitting the servs dataset according to quadrents of court to transpose the start position co-ordinates of the shot
q1<-servs[(servs$sposX>0 & servs$sposY>0),]
q2<-servs[(servs$sposX>0 & servs$sposY<0),]
q3<-servs[(servs$sposX<0 & servs$sposY>0),]
q4<-servs[(servs$sposX<0 & servs$sposY<0),]

#transposing the coordinates of q4 ##t(q4[(x-,y-)]) =q1[(x+,y+)]
q4[,7]<-q4[,7]*-1
q4[,8]<-q4[,8]*-1
q4[,10]<-q4[,10]*-1
q4[,11]<-q4[,11]*-1

#rowbinding them to complete it as a right half dataset, ready for plotting
q14<-rbind(q1,q4)##righthalf

#transposing the coordinates of q3 ##t(q3[(x-,y+)]) =q2[(x+,y-)]
q3[,7]<-q3[,7]*-1
q3[,8]<-q3[,8]*-1
q3[,10]<-q3[,10]*-1
q3[,11]<-q3[,11]*-1

#rowbinding them to complete it as a left half dataset, ready for plotting
q23<-rbind(q2,q3)##lefthalf

##plotting the two datasets of sakke33 which are performance of servs from left and right halfs.
cgplot(q14,aes(eposX,eposY))+labs(title="Sakke33, End position of ball when his serving area was Right Half to him ")##righthalfplot
cgplot(q23,aes(eposX,eposY))+labs(title="Sakke33, End position of ball when his serving area was Left Half to him ")##lefthalfplot

#######################################################################################################################################################################

##Serve analysis for vesavee

#splitting dataset for player2
player2<-mmshots[mmshots$player=="vesavee",]
#collecting only servs data
servs<-player2[(player2$type == "first_serve" | player2$type == "second_serve"),]

#grouping player shots by rallies, to obtain final shot of the each rally
gpByRally <- group_by(player2, rallieid)
lastShots <- summarize(gpByRally, max.pt = max(shotid))
winShots <- subset(player2,player2$shotid %in% lastShots$max.pt)

#classifying winshots into servshots(finding Ace shots)
ServWins <- winShots[(winShots$type == "first_serve" | winShots$type == "second_serve") & (winShots$call %in% c("in","est")),]

##creating a column serv_result,which gives the actual result of that shot (specially includes Ace's in the respective "in" calls)
servs$serv_result<-servs$call
servs$serv_result<-as.character(servs$serv_result)
servs$serv_result[servs$shotid %in% ServWins$shotid]<- "ace"
servs$serv_result<-as.factor(servs$serv_result)
sapply(servs,class)

##splitting the servs dataset according to quadrents of court to transpose the start position co-ordinates of the shot
q1<-servs[(servs$sposX>0 & servs$sposY>0),]
q2<-servs[(servs$sposX>0 & servs$sposY<0),]
q3<-servs[(servs$sposX<0 & servs$sposY>0),]
q4<-servs[(servs$sposX<0 & servs$sposY<0),]

#transposing the coordinates of q4 ##t(q4[(x-,y-)]) =q1[(x+,y+)]
q4[,7]<-q4[,7]*-1
q4[,8]<-q4[,8]*-1
q4[,10]<-q4[,10]*-1
q4[,11]<-q4[,11]*-1

#rowbinding them to complete it as a right half dataset, ready for plotting
q14<-rbind(q1,q4)##righthalf

#transposing the coordinates of q3 ##t(q3[(x-,y+)]) =q2[(x+,y-)]
q3[,7]<-q3[,7]*-1
q3[,8]<-q3[,8]*-1
q3[,10]<-q3[,10]*-1
q3[,11]<-q3[,11]*-1

#rowbinding them to complete it as a left half dataset, ready for plotting
q23<-rbind(q2,q3)##lefthalf

##plotting the two datasets of vesavee which are performance of servs from left and right halfs.
cgplot(q14,aes(eposX,eposY))+labs(title="Vesavee, End position of ball when his serving area was Right Half to him ")##righthalfplot
cgplot(q23,aes(eposX,eposY))+labs(title="vesavee, End position of ball when his serving area was Left Half to him ")##lefthalfplot

