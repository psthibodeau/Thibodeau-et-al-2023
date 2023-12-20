#T Thibodeau 2023
#Code for PNAS manuscript

#####chl#####
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyr)
library(Hmisc)

#####cumulative sum chla by decade for Table S2 & Fig. 2#####
setwd('C:/Users/pthibodeau1/OneDrive - University of New England/URI/NBPTS/Data/BCO-DMO/Github')

bco <- read.csv("NBPTS data 1959-2019 Final.csv", header=T)

bco.chl <-na.omit(cbind(bco$year,bco$ChlA))

Year <- unique(bco.chl[,1]) #vector of individual unique names of all bottles
x<-0
sum<-data.frame()  #creates empty dataframe

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(bco.chl[,1]==i) #b row number from data when year equals 'i'
  b1<-bco.chl[b,2] #subsets all rows in column b
  cum<-sum(b1)
  sum[x,1]<-i #add value for 'i' into column one of dataframe sum
  sum[x,2]<-cum #add value for slope of 'i' into column two from dataframe sum
}

colnames(sum)<-c("Year","Cum") #column names for year and slope
sum 

#####Table S2#####
#first complete decade (1973-1982)
first.mean <- mean(sum$Cum[4:13])
first.sd<-sd(sum$Cum[4:13])

#last complete decade (2009-2019), exclude 2012
last.mean<-mean(c(sum$Cum[36:38],sum$Cum[40:46]))
last.sd<-sd(c(sum$Cum[36:38],sum$Cum[40:46]))

t.test(sum$Cum[4:13],c(sum$Cum[36:38],sum$Cum[40:46]))

#average % change
((last.mean-first.mean)/first.mean)*100

#####Fig. 2b####
par(mfrow=c(1,1))
par(mai=c(0.5,1,0.25,0.15))
last.mean.c<- c(sum$Cum[36:38],sum$Cum[40:46])

boxplot(sum$Cum[4:13],last.mean.c,type="p",xlab=" ",pch=19,las=1,ylim=c(200,900),
        ylab=" ",cex.axis=1.5,cex=1.5,horizontal = FALSE)
text(1, 115, cex=1.35, xpd = NA, srt =360, "First decade")
text(2, 115, cex=1.35, xpd = NA, srt =360, "Last decade")
text(2, 540, cex=1.75, xpd = NA, srt =360, "*")
text(-0.25, 550, cex=1.5, labels = expression("Cumulative Chlorophyll a"~ " (mg m"^"-3"*")"),
     xpd = NA, srt =90)
mtext(side=3, at=c(0.5), las=1, cex=1.25, "B")

#####annual max chla by decade for Table S2 & Fig. 2c#####

Year <- unique(bco.chl[,1]) #vector of individual unique names of all bottles
x<-0
max.chl<-data.frame()  #creates empty dataframe

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(bco.chl[,1]==i) #b row number from data when year equals 'i'
  b1<-bco.chl[b,2] #subsets all rows in column b
  max<-max(b1)
  max.chl[x,1]<-i #add value for 'i' into column one of dataframe sum
  max.chl[x,2]<-max #add value for slope of 'i' into column two from dataframe sum
}

colnames(max.chl)<-c("Year","max") #column names for year and slope
max.chl 

###Fig. 2C### (begins at 1972 to align with 2A)
plot(max.chl$Year[3:46],max.chl$max[3:46],pch=19,col="green3",
     ylab=" ",xlab=" ",ylim=c(0,105),las=2,xlim=c(1970,2020),cex=1.25,cex.axis=1.5)
max.lm <- lm(log(max.chl$max[3:46])~max.chl$Year[3:46])
summary(max.lm) 
par(new=T)
max.pred <- exp(predict(max.lm))
lines(max.pred ~ max.chl$Year[3:46], col="black",lwd=2)
text(1995, -28, cex=1.5,
     xpd = NA, srt =1, "Year")
minor.tick(nx=10, tick.ratio=0.5,ny='')
mtext(side=3, at=c(1970), las=1, cex=1.25, "C")
text(1950, 50, cex=1.5, labels = expression("Chlorophyll a"~ " (mg m"^"-3"*")"),
     xpd = NA, srt =90)

dev.off()

#####Table S2#####
#first complete decade (1973-1982)
first.mean <- mean(max.chl$max[4:13])
first.sd<-sd(max.chl$max[4:13])

#last complete decade (2009-2019), exclude 2012
last.mean<-mean(c(max.chl$max[36:38],max.chl$max[40:46]))
last.sd<-sd(c(max.chl$max[36:38],max.chl$max[40:46]))

t.test(max.chl$max[4:13],c(max.chl$max[36:38],max.chl$max[40:46]))

#average % change
((last.mean-first.mean)/first.mean)*100

####avg annual CV by decade###
Year <- unique(bco.chl[,1]) #vector of individual unique names of all bottles
x<-0
var.chl<-data.frame()  #creates empty dataframe

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(bco.chl[,1]==i) #b row number from data when year equals 'i'
  b1<-bco.chl[b,2]
  var<-100*(sd(b1)/mean(b1))
  var.chl[x,1]<-i #add value for 'i' into column one of dataframe sum
  var.chl[x,2]<-var #add value for slope of 'i' into column two from dataframe sum
}

colnames(var.chl)<-c("Year","CV") #column names for year and slope
var.chl
mean(var.chl$CV) #round to 88%

#10 year decadal average max+sd for Table S2
#first complete decade (1973-1982)
first.mean <- mean(var.chl$CV[4:13])
first.sd<-sd(var.chl$CV[4:13])

#last complete decade (2009-2019), exclude 2012
last.mean<-mean(c(var.chl$CV[36:38],var.chl$CV[40:46]))
last.sd<-sd(c(var.chl$CV[36:38],var.chl$CV[40:46]))

t.test(var.chl$CV[4:13],c(var.chl$CV[36:38],var.chl$CV[40:46]))


####Fig. S1####
#winter/spring (January to April, weeks 1 thru 16)
#summer/fall (June to September, weeks 22 thru 38)
#fall/winter (post week 38)

c.new <- as.data.frame(na.omit((cbind(bco$year,bco$week,bco$ChlA))))

chl1 <- c.new %>% #period 1
  filter(c.new$V2<17)
chl2 <- c.new %>% # period 2
  filter(c.new$V2>21 & c.new$V2<39)
chl3 <- c.new %>% #period 3
  filter(c.new$V2>38)

#calculate annual means
chl.yr.mean.1 <- aggregate(chl1$V3, 
                             by=list(year=chl1$V1),FUN=mean)
chl.yr.mean.2 <- aggregate(chl2$V3, 
                             by=list(year=chl2$V1),FUN=mean)
chl.yr.mean.3 <- aggregate(chl3$V3, 
                           by=list(year=chl3$V1),FUN=mean)

#calculate annual standard deviation
chl.yr.sd.1 <- aggregate(chl1$V3, 
                           by=list(year=chl1$V1),FUN=sd)
chl.yr.sd.2 <- aggregate(chl2$V3, 
                           by=list(year=chl2$V1),FUN=sd)
chl.yr.sd.3 <- aggregate(chl3$V3,by=list(year=chl3$V1),FUN=sd)

par(mfrow=c(1,3)) 
par(mai=c(0.75,0.65,0.25,0.1))

#period 1
plot(chl.yr.mean.1$year,chl.yr.mean.1$x,type="p",pch=19,col="black",cex.axis=1.5,
     ylab=" ",xlab=" ",ylim=c(0,40),las=2,xlim=c(1965,2020),cex=1.5,xaxt="n")
arrows(chl.yr.mean.1$year,chl.yr.mean.1$x+chl.yr.sd.1$x,chl.yr.mean.1$year,
       chl.yr.mean.1$x-chl.yr.sd.1$x,code=3,col="dark grey",
       length=0.03,angle=90)
text(1938, 20, cex=1.5, labels = expression("Chlorophyll a "~ " (mg m"^"-3"*")"),
    xpd = NA, srt =90)
axis(side=1, at = chl.yr.mean.1$year, labels=chl.yr.mean.1$year, par(las=2),font=1, cex.axis=1.5)
minor.tick(nx=10, tick.ratio=0.5,ny='')
chl.lm <- lm(log(chl.yr.mean.1$x)~chl.yr.mean.1$year)
summary(chl.lm) 
par(new=T)
var.pred <- exp(predict(chl.lm))
lines(var.pred ~ chl.yr.mean.1$year, col="black",lwd=2)
mtext(side=3, at=c(1968), las=1, cex=1, "A")

plot(chl.yr.mean.2$year,chl.yr.mean.2$x,type="p",pch=19,col="black",cex.axis=1.5,
     ylab=" ",xlab=" ",ylim=c(0,40),las=2,xlim=c(1965,2020),cex=1.5,xaxt="n")
arrows(chl.yr.mean.2$year,chl.yr.mean.2$x+chl.yr.sd.2$x,chl.yr.mean.2$year,
       chl.yr.mean.2$x-chl.yr.sd.2$x,code=3,col="dark grey",
       length=0.03,angle=90)
axis(side=1, at = chl.yr.mean.2$year, labels=chl.yr.mean.2$year, par(las=2),font=1, cex.axis=1.5)
minor.tick(nx=10, tick.ratio=0.5,ny='')
chl.lm <- lm(log(chl.yr.mean.2$x)~chl.yr.mean.2$year)
summary(chl.lm)
par(new=T)
var.pred <- exp(predict(chl.lm))
lines(var.pred ~ chl.yr.mean.2$year, col="black",lwd=2)
mtext(side=3, at=c(1968), las=1, cex=1, "B")
text(1995, -8, cex=1.5, xpd = NA, srt =360, "Year")

plot(chl.yr.mean.3$year,chl.yr.mean.3$x,type="p",pch=19,col="black",cex.axis=1.5,
     ylab=" ",xlab=" ",ylim=c(0,40),las=2,xlim=c(1965,2020),cex=1.5,xaxt="n")
arrows(chl.yr.mean.3$year,chl.yr.mean.3$x+chl.yr.sd.3$x,chl.yr.mean.3$year,
       chl.yr.mean.3$x-chl.yr.sd.3$x,code=3,col="dark grey",
       length=0.03,angle=90)
axis(side=1, at = chl.yr.mean.3$year, labels=chl.yr.mean.3$year, par(las=2),font=1, cex.axis=1.5)
minor.tick(nx=10, tick.ratio=0.5,ny='')
chl.lm <- lm(log(chl.yr.mean.3$x)~chl.yr.mean.3$year)
summary(chl.lm)
par(new=T)
var.pred <- exp(predict(chl.lm))
lines(var.pred ~ chl.yr.mean.3$year, col="black",lwd=2)
mtext(side=3, at=c(1968), las=1, cex=1, "C")


#####biomass threshold#####
#From Siegel et al. 2002: date when chl concentration 5% > annual median

colnames(c.new)<-c("c.Year","c.week","chl")

#compute annual median for each year
med.68 <- median(c.new[which(c.new$c.Year==1968),3]) #11.98
med.68
#1.05*11.41 = 11.98
cbind(c.new[which(c.new$c.Year==1968),2],c.new[which(c.new$c.Year==1968),3]) 
#week 2 (initiates and max) 


med.69 <- median(c.new[which(c.new$c.Year==1969),3]) #29.36
#1.05*12.51 = 30.83
cbind(c.new[which(c.new$c.Year==1969),2],c.new[which(c.new$c.Year==1969),3]) 
#week 2 (initiates and max) 

med.72 <- median(c.new[which(c.new$c.Year==1972),3]) #12.51
#1.05*12.51 = 13.15
cbind(c.new[which(c.new$c.Year==1972),2],c.new[which(c.new$c.Year==1972),3]) 
#week 7, 17

med.73 <- median(c.new[which(c.new$c.Year==1973),3]) #14.52
#1.05*14.52 = 15.26
cbind(c.new[which(c.new$c.Year==1973),2],c.new[which(c.new$c.Year==1973),3]) 
#week 9, 15, 21, 29, 37, 41, 48

med.74 <- median(c.new[which(c.new$c.Year==1974),3]) #11.32
#1.05*11.32 = 11.88
cbind(c.new[which(c.new$c.Year==1974),2],c.new[which(c.new$c.Year==1974),3]) 
#week 8, 13, 15, 20, 22, 25, 31, 44, 49

med.75 <- median(c.new[which(c.new$c.Year==1975),3]) #8.52
#1.05*8.52 = 8.946
cbind(c.new[which(c.new$c.Year==1975),2],c.new[which(c.new$c.Year==1975),3]) 
#week 9

med.76 <- median(c.new[which(c.new$c.Year==1976),3]) #8.03
#1.05*8.03 = 8.43
cbind(c.new[which(c.new$c.Year==1976),2],c.new[which(c.new$c.Year==1976),3]) 
#week 5
  #initiates week 5, reaches max week 8

med.77 <- median(c.new[which(c.new$c.Year==1977),3]) #10.24
#1.05*10.24 = 10.75
cbind(c.new[which(c.new$c.Year==1977),2],c.new[which(c.new$c.Year==1977),3]) 
#week 1

med.78 <- median(c.new[which(c.new$c.Year==1978),3]) #8.32
#1.05*8.32 = 8.73
cbind(c.new[which(c.new$c.Year==1978),2],c.new[which(c.new$c.Year==1978),3]) 
#week 10
  #initiates and max week 10

med.79 <- median(c.new[which(c.new$c.Year==1979),3]) #6.43
#1.05*6.43 = 6.75
cbind(c.new[which(c.new$c.Year==1979),2],c.new[which(c.new$c.Year==1979),3]) 
#week 11
  #initiates week 11, max 12

med.80 <- median(c.new[which(c.new$c.Year==1980),3]) #10.05
#1.05*10.05 = 10.55
cbind(c.new[which(c.new$c.Year==1980),2],c.new[which(c.new$c.Year==1980),3]) 
#week 1

med.81 <- median(c.new[which(c.new$c.Year==1981),3]) #6.03
#1.05*6.03 = 6.33
cbind(c.new[which(c.new$c.Year==1981),2],c.new[which(c.new$c.Year==1981),3]) 
#week 6

med.82 <- median(c.new[which(c.new$c.Year==1982),3]) #6.99
#1.05*6.99 = 7.33
cbind(c.new[which(c.new$c.Year==1982),2],c.new[which(c.new$c.Year==1982),3]) 
#week 9

med.83 <- median(c.new[which(c.new$c.Year==1983),3]) #10.99
#1.05*10.99 = 11.53
cbind(c.new[which(c.new$c.Year==1983),2],c.new[which(c.new$c.Year==1983),3]) 
#week 2

med.84 <- median(c.new[which(c.new$c.Year==1984),3]) #7.07
#1.05*7.07 = 7.42
cbind(c.new[which(c.new$c.Year==1984),2],c.new[which(c.new$c.Year==1984),3]) 
#week 6

med.85 <- median(c.new[which(c.new$c.Year==1985),3]) #7.42
#1.05*7.42 = 7.70
cbind(c.new[which(c.new$c.Year==1985),2],c.new[which(c.new$c.Year==1985),3]) 
#week 3

med.86 <- median(c.new[which(c.new$c.Year==1986),3]) #8.00
#1.05*8.00 = 8.4
cbind(c.new[which(c.new$c.Year==1986),2],c.new[which(c.new$c.Year==1986),3]) 
#week 1

med.87 <- median(c.new[which(c.new$c.Year==1987),3]) #16.12
#1.05*16.12 = 16.92
cbind(c.new[which(c.new$c.Year==1987),2],c.new[which(c.new$c.Year==1987),3]) 
#week 8

med.88 <- median(c.new[which(c.new$c.Year==1988),3]) #6.82
#1.05*6.82 = 7.16
cbind(c.new[which(c.new$c.Year==1988),2],c.new[which(c.new$c.Year==1988),3]) 
#week 2, 9

med.89 <- median(c.new[which(c.new$c.Year==1989),3]) #7.11
#1.05*7.11 = 7.465
cbind(c.new[which(c.new$c.Year==1989),2],c.new[which(c.new$c.Year==1989),3]) 
#week 1, 6

med.90 <- median(c.new[which(c.new$c.Year==1990),3]) #5.78
#1.05*5.78 = 6.069
cbind(c.new[which(c.new$c.Year==1990),2],c.new[which(c.new$c.Year==1990),3]) 
#week 1
  #initates week 1, max bloom week 3

med.91 <- median(c.new[which(c.new$c.Year==1991),3]) #5.749
#1.05*5.749 = 6.03
cbind(c.new[which(c.new$c.Year==1991),2],c.new[which(c.new$c.Year==1991),3]) 
#week 8
  #initaties and blooms in same week

med.92 <- median(c.new[which(c.new$c.Year==1992),3]) #2.39
#1.05*2.39 = 2.51
cbind(c.new[which(c.new$c.Year==1992),2],c.new[which(c.new$c.Year==1992),3]) 
#week 2
  #initiates week 2, max week 8

med.93 <- median(c.new[which(c.new$c.Year==1993),3]) #5.49
#1.05*5.49 = 5.76
cbind(c.new[which(c.new$c.Year==1993),2],c.new[which(c.new$c.Year==1993),3]) 
#week 12
#initiates week 12, max week 14

med.94 <- median(c.new[which(c.new$c.Year==1994),3]) #5.88
#1.05*5.88 = 6.17
cbind(c.new[which(c.new$c.Year==1994),2],c.new[which(c.new$c.Year==1994),3]) 
#week 4

#no data 1995-1998, filled with output from chla DLM

med.99 <- median(c.new[which(c.new$c.Year==1999),3]) #5.04
#1.05*5.04 = 5.29
cbind(c.new[which(c.new$c.Year==1999),2],c.new[which(c.new$c.Year==1999),3]) 
#week 9

med.00 <- median(c.new[which(c.new$c.Year==2000),3]) #9.24
#1.05*9.24 = 9.70
cbind(c.new[which(c.new$c.Year==2000),2],c.new[which(c.new$c.Year==2000),3]) 
#week 2, 6

med.01 <- median(c.new[which(c.new$c.Year==2001),3]) #6.55
#1.05*6.55 = 6.87
cbind(c.new[which(c.new$c.Year==2001),2],c.new[which(c.new$c.Year==2001),3]) 
#week 4

med.02 <- median(c.new[which(c.new$c.Year==2002),3]) #5.57
#1.05*5.57 = 5.848
cbind(c.new[which(c.new$c.Year==2002),2],c.new[which(c.new$c.Year==2002),3]) 
#week 9

med.03<- median(c.new[which(c.new$c.Year==2003),3]) #5.56
#1.05*5.56 = 5.838
cbind(c.new[which(c.new$c.Year==2003),2],c.new[which(c.new$c.Year==2003),3]) 
#week 1,3


med.04 <- median(c.new[which(c.new$c.Year==2004),3]) #6.14
#1.05*6.14 = 6.44
cbind(c.new[which(c.new$c.Year==2004),2],c.new[which(c.new$c.Year==2004),3]) 
#week 2

med.05 <- median(c.new[which(c.new$c.Year==2005),3]) #3.76
#1.05*3.76 = 3.94
cbind(c.new[which(c.new$c.Year==2005),2],c.new[which(c.new$c.Year==2005),3]) 
#week 8
  #initation week 8, max week 9

med.06 <- median(c.new[which(c.new$c.Year==2006),3]) #4.11
#1.05*4.11 = 4.31
cbind(c.new[which(c.new$c.Year==2006),2],c.new[which(c.new$c.Year==2006),3]) 
#week 1

med.07 <- median(c.new[which(c.new$c.Year==2007),3]) #6.81
#1.05*6.81 = 7.15
cbind(c.new[which(c.new$c.Year==2007),2],c.new[which(c.new$c.Year==2007),3]) 
#week 3

med.08 <- median(c.new[which(c.new$c.Year==2008),3]) #6.81
#1.05*6.81 = 7.15
cbind(c.new[which(c.new$c.Year==2008),2],c.new[which(c.new$c.Year==2008),3]) 
#week 1, 6

med.09 <- median(c.new[which(c.new$c.Year==2009),3]) #3.72
#1.05*3.72 = 3.90
cbind(c.new[which(c.new$c.Year==2009),2],c.new[which(c.new$c.Year==2009),3]) 
#week 9

med.10 <- median(c.new[which(c.new$c.Year==2010),3]) #3.97
#1.05*3.97 = 4.16
cbind(c.new[which(c.new$c.Year==2010),2],c.new[which(c.new$c.Year==2010),3]) 
#week 3, 11, 16, 18, 24, 27, 30, 33, 44, 51

med.11 <- median(c.new[which(c.new$c.Year==2011),3]) #4.05
#1.05*4.05 = 4.25
cbind(c.new[which(c.new$c.Year==2011),2],c.new[which(c.new$c.Year==2011),3]) 
#week 2,5, 9, 17,19,25,32,37,44

med.13 <- median(c.new[which(c.new$c.Year==2013),3]) #3.195
#1.05*3.195 = 3.35
cbind(c.new[which(c.new$c.Year==2013),2],c.new[which(c.new$c.Year==2013),3]) 
#week 4,11,23,27,30,32,40,46,

med.14 <- median(c.new[which(c.new$c.Year==2014),3]) #3.51
#1.05*3.51 = 3.68
cbind(c.new[which(c.new$c.Year==2014),2],c.new[which(c.new$c.Year==2014),3]) 
#week 3,10,14,26,29,31,33,37,40

med.15 <- median(c.new[which(c.new$c.Year==2015),3]) #5.09
#1.05*5.09 = 5.34
cbind(c.new[which(c.new$c.Year==2015),2],c.new[which(c.new$c.Year==2015),3]) 
#week 5,9,14,18,26,31,36,42,45

med.16 <- median(c.new[which(c.new$c.Year==2016),3]) #5.975
#1.05*5.975 = 6.27
cbind(c.new[which(c.new$c.Year==2016),2],c.new[which(c.new$c.Year==2016),3]) 
#week 7,11,26,32,35,42,46,49

med.17 <- median(c.new[which(c.new$c.Year==2017),3]) #6.35
#1.05*6.35 = 6.6675
cbind(c.new[which(c.new$c.Year==2017),2],c.new[which(c.new$c.Year==2017),3]) 
#week 1,3,5,12,14,25,27,31,34,38,40,42,49

med.18 <- median(c.new[which(c.new$c.Year==2018),3]) #5.43
#1.05*5.43 = 5.70
cbind(c.new[which(c.new$c.Year==2018),2],c.new[which(c.new$c.Year==2018),3]) 
#week 1,13,16,22,31,33,37

med.19 <- median(c.new[which(c.new$c.Year==2019),3]) #3.75
#1.05*3.75 = 3.9375
cbind(c.new[which(c.new$c.Year==2019),2],c.new[which(c.new$c.Year==2019),3]) 
#week 2,10,19,22,25,32,46

#output into 'raw chl phenology metrics.csv'

#####Fig. S5#####
###freq boxplot full time series
phen <- read.csv('raw chl phenology metrics.csv')
par(mfrow=c(1,3))
par(mai=c(0.6,0.5,0.25,0.15))
boxplot(phen$totalfreq,type="p",xlab=" ",pch=19,las=2,
        ylab=" ",ylim=c(0,12),cex.axis=1.5,cex=1.5)
#points(x=1,y=11.35,pch=8,cex=1.5,col="black",typ="p")
boxplot(phen$winfreq,type="p",pch=19,las=2,
        xlab=" ",ylim=c(0,12),cex.axis=1.5,cex=1.5,
        ylab=" ")
boxplot(phen$sumfreq,type="p",xlab=" ",pch=19,las=2,
        ylab=" ",cex.axis=1.5,cex=1.5,ylim=c(0,12))

kruskal.test(phen$totalfreq~phen$Decade) #p = 0.18
kruskal.test(phen$winfreq~phen$Decade) #p = 0.27
kruskal.test(phen$sumfreq~phen$Decade) #p = 0.37
summary(aov(phen$winfreq~phen$Decade)) #p = 0.27
summary(aov(phen$sumfreq~phen$Decade)) #p = 0.45


#####Fig 1A#####
#Load packages
library(maptools)
library(raster)
library(prettymapr)

#Load USA and individual state shapefile data
USA <- getData('GADM', country='USA', level=1)
ME <- (USA[USA$NAME_1=="Maine",])
NH <- (USA[USA$NAME_1=="New Hampshire",])
VT <- (USA[USA$NAME_1=="Vermont",])
RI <- (USA[USA$NAME_1=="Rhode Island",])
MA <- (USA[USA$NAME_1=="Massachusetts",])
CT <- (USA[USA$NAME_1=="Connecticut",])
NY <- (USA[USA$NAME_1=="New York",])


#Plot zoomed in
plot(RI, bg = "white", border = "black", axes = T, col = "grey90", 
     ylim = c(41.5, 41.7), xlim = c(-71.6,-71),las=2)
plot(MA, axes = T, col = "grey90", border = "black", add = T)
#Narragansett Bay Plankton Time Series location
points(-71.4, 41.567, pch =21, bg = "black", cex = 1.5, lwd = 1)

library(prettymapr)
addnortharrow(pos = "topright", padin = c(0.1,0.1), scale = 1, lwd = 1, 
              border = "black", cols = c("white","black"), text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25, 
            unitcategory = "metric", htin = 0.1, padin = c(2.25,0.1), style = "bar", 
            bar.cols = c("black","white"),
            lwd = 1, linecol = "black", tick.cex = 0.5, labelpadin = 0.15, 
            label.cex = 1.25, label.col = "black")


#plot zoomed out
plot(RI, bg = "white", border = "black", axes = T, col = "grey70", las=2,
     ylim = c(40.5,44), xlim = c(-70.955,-70.94))
plot(ME, axes = T, col = "grey70", border = "black", add = T)
plot(VT, axes = T, col = "grey70", border = "black", add = T)
plot(NH, axes = T, col = "grey70", border = "black", add = T)
plot(MA, axes = T, col = "grey70", border = "black", add = T)
plot(CT, axes = T, col = "grey70", border = "black", add = T)
plot(NY, axes = T, col = "grey70", border = "black", add = T)

rect(-72, 41, -71, 42.25, border = "black", lwd = 2)

RI_Map <- recordPlot()

####Fig 1b####
par(mfrow=c(4,2))
par(mai=c(0.1,0.65,0.35,0.12))

#bco <- read.csv('NBPTS data 1959-2019 Final.csv')

#temperature
t.month.mean <- aggregate(bco$Temperature_SURF, 
                          by=list(week=bco$week,year=bco$year),FUN=mean)
bco.temp <- na.omit(t.month.mean)
t.yr.mean <- aggregate(bco.temp[,3], 
                       by=list(week=bco.temp[,1]),FUN=mean)

plot(t.month.mean[,1],t.month.mean[,3],xlab=" ",col="grey55",
     ylab=" ",las=1,ylim=c(-5,30),cex.axis=1.5)
par(new=T)
plot(t.yr.mean$x[1:52],xlab=" ",xlim=c(0,52),ylim=c(-5,30),
     ylab=" ",las=1,type="l",col="black",las=1,
     lwd=2,lty=1,xaxt = 'n',yaxt = 'n')
text(-13, 12.5, cex=1.5,xpd = NA, srt =90, 
     labels =expression("Temp ("*~degree*C*")"))
minor.tick(nx=10, tick.ratio=0.5,ny='')

#light
light.month.mean <- aggregate(bco$light, 
                            by=list(week=bco$week,year=bco$year),FUN=mean)
bco.light <- na.omit(light.month.mean)
light.yr.mean <- aggregate(bco.light[,3], 
                       by=list(year=bco.light[,1]),FUN=mean)
plot(light.month.mean[,1],light.month.mean[,3],xlab=" ",ylim=c(0,1500),
     las=1,ylab=" ",cex.axis=1.5,col="grey55")
par(new=T)
plot(light.yr.mean$x,xlab=" ",xlim=c(0,52),ylim=c(0,1500),
     ylab=" ",las=1,type="l",col="black",las=1,
     lwd=2,lty=1,xaxt = 'n',yaxt = 'n')
text(-16, 750, cex=1.5, labels = expression('Light '* "(" * mu* "mol"~ "m"^"-2"*" d"^"-1"*")"),
     xpd = NA, srt =90)
minor.tick(nx=10, tick.ratio=0.5,ny='')

#salinity

sal.month.mean <- aggregate(bco$Salinity, 
                            by=list(week=bco$week,year=bco$year),FUN=mean)
bco.sal <- na.omit(sal.month.mean)
sal.yr.mean <- aggregate(bco.sal[,3], 
                           by=list(year=bco.sal[,1]),FUN=mean)
plot(sal.month.mean[,1],sal.month.mean[,3],xlab=" ",
     ylim=c(20,35),las=1,ylab=" ",col="grey55",cex.axis=1.5)
par(new=T)
plot(sal.yr.mean[,1],sal.yr.mean[,2],xlab=" ",lwd=2,lty=1,xaxt = 'n',yaxt='n',
     ylim=c(20,35),las=1,ylab=" ",col="black",cex.axis=1.5,typ="l")
text(-13, 27.5, cex=1.5,xpd = NA, srt =90, "Salinity")
minor.tick(nx=10, tick.ratio=0.5,ny='')

#nuts
si.month.mean <- aggregate(bco$SiO4, 
                            by=list(week=bco$week,year=bco$year),FUN=mean)
bco.si <- na.omit(si.month.mean)
si.yr.mean <- aggregate(bco.si[,3], 
                         by=list(year=bco.si[,1]),FUN=mean)
plot(si.month.mean[,1],si.month.mean[,3],xlab=" ",col="grey55",
     ylab=" ",las=1,xlim=c(0,52),cex.axis=1.5)
par(new=T)
plot(si.yr.mean$x[1:52],xlab=" ",xlim=c(0,52),
     ylab=" ",las=1,type="l",col="black",las=1,
     ylim=c(0,60),lwd=2,lty=1,xaxt = 'n',yaxt='n')
text(-14, 30, cex=1.5, labels = expression('SiO'[4]*' '* "(" * mu* "M)"),
     xpd = NA, srt =90)
minor.tick(nx=10, tick.ratio=0.5,ny='')

po.month.mean <- aggregate(bco$PO4, 
                           by=list(week=bco$week,year=bco$year),FUN=mean)
bco.po <- na.omit(po.month.mean)
po.yr.mean <- aggregate(bco.po[,3], 
                        by=list(year=bco.po[,1]),FUN=mean)
plot(po.month.mean[,1],po.month.mean[,3],col="grey55",cex.axis=1.5,
     ylab=" ",las=1,xlab=" ",ylim=c(0,6)) #2 point removed w/ylim=8
par(new=T)
plot(po.yr.mean$x[1:52],xlab=" ",xlim=c(0,52),cex.axis=1.5,
     ylab=" ",las=1,type="l",col="black",las=1,
     ylim=c(0,6),lwd=2,lty=1,xaxt = 'n',yaxt='n')
text(-12, 3, cex=1.5, labels = expression('PO'[4]*' '* "(" * mu* "M)"),
     xpd = NA, srt =90)
minor.tick(nx=10, tick.ratio=0.5,ny='')

nh.month.mean <- aggregate(bco$NH4, 
                           by=list(week=bco$week,year=bco$year),FUN=mean)
bco.nh <- na.omit(nh.month.mean)
nh.yr.mean <- aggregate(bco.nh[,3], 
                        by=list(year=bco.nh[,1]),FUN=mean)
plot(nh.month.mean[,1],nh.month.mean[,3],xlab=" ",cex.axis=1.5,
     ylab=" ",las=1,col="grey55",ylim=c(0,15)) #2 data points removed
par(new=T)
plot(nh.yr.mean$x[1:52],xlab=" ",xlim=c(0,52),ylim=c(0,15),
     ylab=" ",las=1,type="l",col="black",las=1,
     lwd=2,lty=1,xaxt = 'n',yaxt='n')
text(-13, 8, cex=1.5, labels = expression('NH'[4]*' '* "(" * mu* "M)"),
     xpd = NA, srt =90)
minor.tick(nx=10, tick.ratio=0.5,ny='')

no.month.mean <- aggregate(bco$NO3_2, 
                           by=list(week=bco$week,year=bco$year),FUN=mean)
bco.no <- na.omit(no.month.mean)
no.yr.mean <- aggregate(bco.no[,3], 
                        by=list(year=bco.no[,1]),FUN=mean)
plot(no.month.mean[,1],no.month.mean[,3],xlab=" ",col="grey55",ylim=c(0,30),
     ylab=" ",las=1,cex.axis=1.5) #2 data points removed
text(-12, 15, cex=1.5, labels = expression('NO'[3/2]*' '* "(" * mu* "M)"),
     xpd = NA, srt =90)
par(new=T)
plot(no.yr.mean$x[1:52],xlab=" ",xlim=c(0,52),
     ylab=" ",las=1,type="l",col="black",las=1,
     ylim=c(0,30),lwd=2,lty=1,xaxt = 'n',yaxt='n')
minor.tick(nx=10, tick.ratio=0.5,ny='')

#chl
chl.month.mean <- aggregate(bco$ChlA, 
                            by=list(week=bco$week,year=bco$year),FUN=mean)
chl.yr <-na.omit(chl.month.mean)
chl.yr.mean <- aggregate(chl.yr[,3], 
                            by=list(year=chl.yr[,1]),FUN=mean)
plot(chl.month.mean[,1],chl.month.mean[,3],xlab=" ",
     ylab=" ",las=1,col="grey55",cex.axis=1.5,ylim=c(0,50))
par(new=T)
plot(chl.yr.mean$year,chl.yr.mean$x,xlab=" ",xlim=c(0,52),ylim=c(0,50),
     ylab=" ",las=1,type="l",col="black",las=1,
     lwd=2,lty=1,xaxt = 'n',yaxt='n')
text(-14, 25, cex=1.5, labels = expression("Chl a"~ "(mg m"^"-3"*")"),
     xpd = NA, srt =90)
minor.tick(nx=10, tick.ratio=0.5,ny='')

####Fig. 3####
#####Chl sc by decade#####
library(ggplot2)
library(reshape)
library(ggpubr)
library(scales)
library(plyr)
library(tidyverse)
library(Hmisc)

chl2 <- read.csv("jchl.68.79.csv")
jchl2<-apply(chl2,1,mean)

jchl80 <- read.csv("jdata_80_89.csv")
md <- melt(jchl80,id=c("Year","Month","Week"))
md.new <- cast(md,Week~Year,mean)
jchl80.m<-apply(md.new,1,mean)

jchl90 <- read.csv("jdata_90_99.csv")
md <- melt(jchl90,id=c("Year","Month","Week"))
md.new <- cast(md,Week~Year,mean)
jchl90<-apply(md.new,1,mean)

jchl00 <- read.csv("jdata_00_09.csv")
md <- melt(jchl00,id=c("Year","Month","Week"))
md.new <- cast(md,Week~Year,mean)
jchl00<-apply(md.new,1,mean)

chl1 <- read.csv("jdata_10_19.csv")
jchl19<-apply(chl1,1,mean)

chl.ks <-read.csv("ks.chl.PNAS.csv")
par(mai=c(0.85,1,0.5,0.15))
par(mfrow=c(1,1)) 

plot(chl.ks$X1970s,type="l",col="dark green",ylim=c(0,20),
     xlab = ' ', ylab = ' ',yaxt = 'n',lwd=3,lty=1,xaxt = 'n')
par(new=T)
plot(chl.ks$X1980s,type="l",col="palegreen4",las=1,
     ylab=" ",xlab=" ",ylim=c(0,20),lwd=3,lty=5,yaxt = 'n',xaxt = 'n')
par(new=T)
plot(chl.ks$X1990s,type="l",col="palegreen3",las=1,
     ylab=" ",xlab=" ",ylim=c(0,20),lwd=3,lty=5,yaxt = 'n',xaxt = 'n')
par(new=T)
plot(chl.ks$X2000s,type="l",col="palegreen2",las=1,
     ylab=" ",xlab=" ",ylim=c(0,20),lwd=3,lty=3,yaxt = 'n',xaxt = 'n')
par(new=T)
plot(chl.ks$X2010s,type="l",col="palegreen3",ylim=c(0,20),cex.axis=1.15,
     xlab = 'Week', ylab = ' ',las=1,lwd=3,lty=1,xaxt = 'n',cex.lab=1.25)

text(-8, 10, cex=1.25, labels = expression("Chlorophyll a "~ " (mg m"^"-3"*")"),
     xpd = NA, srt =90)
week <- c(0,5,10,15,20,25,30,35,40,45,50)
axis(side=1, at = week, labels=week, par(las=1),font=1, cex.axis=1.15)
minor.tick(nx=10, tick.ratio=0.5,ny='')
legend(x="topright",
       legend=c("1968-1979","1980-1989","1990-1999","2000-2009","2010-2019"),
       lwd=3,border="black",col=c("dark green","palegreen4","palegreen3",
                                  "palegreen2", "palegreen3"),
       lty=c(1,5,4,3,1,1),
       bty="n",cex=1)

#K-sample Anderson-Darling tests
#testing whether several collections 
#of observations can be modelled as coming from a single population

md <- melt(chl.ks,id=c("Week")) #align data by decade

library('PMCMRplus') #to look at multiple comparisons with anderson-darling test
#adAllPairsTest(x, g, p.adjust.method = p.adjust.methods, ...)

md$decade<-as.factor(md$variable) #make decade a factor for analysis
adAllPairsTest(value~decade,md,p.adjust.method = "bonferroni")
#all decades diff. from each other except 2010s not diff. from 1990s

#####Fig.5#####

bats <- read.csv('BATS pigments.csv')

cb <- read.csv('CBWaterQualityWaterQualityStation.csv')

l4 <- read.csv('L4_fluorometric_chla_1992.csv')

sf <- read.csv('SanFranciscoBayWaterQualityData1969-2015v3.csv')

library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyr)

###bats##
bats <- bats %>% 
  filter(bats$Chl> 0)

Year <- unique(bats$year)
x<-0
sum<-data.frame()  #creates empty dataframe
#CV=100*sd(obs)/mean(obs)

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(bats$year==i) #b row number from data when year equals 'i'
  b1<-bats[b,] #subsets all rows in column b
  var<-100*sd(b1$Chl)/mean(b1$Chl)
  sum[x,1]<-i #add value for 'i' into column one of dataframe sum
  sum[x,2]<-var #add value for slope of 'i' into column two from dataframe sum
}

colnames(sum)<-c("Year","CV") #column names for year and slope
sum 
bats.sum <- sum

###cb##
new.cb = strptime(cb$SampleDate,format="%m/%d/%Y") 

years = substr(new.cb, 1,4)    # characters 1 to 4 of dates
years
months = substr(new.cb, 6,7)   # characters 6 to 7 of dates
months
days = substr(new.cb,9,10)
days 

cb <- na.omit(data.frame(years,months,days,cb$MeasureValue))

Year <- unique(cb$years)
x<-0
sum<-data.frame()  #creates empty dataframe
#CV=100*sd(obs)/mean(obs)

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(cb$years==i) #b row number from data when year equals 'i'
  b1<-cb[b,] #subsets all rows in column b
  var<-100*sd(b1$cb.MeasureValue)/mean(b1$cb.MeasureValue)
  sum[x,1]<-i #add value for 'i' into column one of dataframe sum
  sum[x,2]<-var #add value for slope of 'i' into column two from dataframe sum
}

colnames(sum)<-c("Year","CV") #column names for year and slope
sum 
sum.cb <- sum

###sf##

sf.new <- sf %>% 
  filter(sf$Station_Number==18)

new.sf = strptime(sf.new$Date,format="%m/%d/%Y") 

years = substr(new.sf, 1,4)    # characters 1 to 4 of dates
years
months = substr(new.sf, 6,7)   # characters 6 to 7 of dates
months
days = substr(new.sf,9,10)
days 

sf.new <- na.omit(data.frame(years,months,days,sf.new$Calculated_Chlorophyll))

Year <- unique(sf.new$years) 
x<-0
sum<-data.frame()  #creates empty dataframe
#CV=100*sd(obs)/mean(obs)

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(sf.new$years==i) #b row number from data when year equals 'i'
  b1<-sf.new[b,] #subsets all rows in column b
  var<-100*sd(b1$sf.new.Calculated_Chlorophyll)/mean(b1$sf.new.Calculated_Chlorophyll)
  sum[x,1]<-i #add value for 'i' into column one of dataframe sum
  sum[x,2]<-var #add value for slope of 'i' into column two from dataframe sum
}

colnames(sum)<-c("Year","CV") #column names for year and slope
sum 
sum.sf <- sum

###L4###
new.l4 = strptime(l4$Date,format="%m/%d/%Y") 

years = substr(new.l4, 1,4)    # characters 1 to 4 of dates
years
months = substr(new.l4, 6,7)   # characters 6 to 7 of dates
months
days = substr(new.l4,9,10)
days 

l4<- na.omit(data.frame(years,months,days,l4$chl))

Year <- unique(l4$years) 
x<-0
sum<-data.frame()  #creates empty dataframe
#CV=100*sd(obs)/mean(obs)

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(l4$years==i) #b row number from data when year equals 'i'
  b1<-l4[b,] #subsets all rows in column b
  chl <- na.omit(as.numeric(as.character(b1$l4.chl)))
  var<-100*sd(chl)/mean(chl)
  sum[x,1]<-i #add value for 'i' into column one of dataframe sum
  sum[x,2]<-var #add value for slope of 'i' into column two from dataframe sum
}

colnames(sum)<-c("Year","CV") #column names for year and slope
sum 
sum.l4 <- sum


##nb##

c=read.csv('NBPTS data 1959-2019 Final.csv')

#subset data from 1968-2019
c.new=na.omit(data.frame(c$year[469:3172],c$week[469:3172],c$ChlA[469:3172]))

colnames(c.new)<-c("year","week","chl") 

Year <- unique(c.new$year) 
x<-0
sum<-data.frame()  #creates empty dataframe
#CV=100*sd(obs)/mean(obs)

for(i in Year){
  x<-x+1 #when x = 1, 2, ...
  b<-which(c.new$year==i) #b row number from data when year equals 'i'
  b1<-c.new[b,] #subsets all rows in column b
  var<-100*sd(b1$chl)/mean(b1$chl)
  sum[x,1]<-i #add value for 'i' into column one of dataframe sum
  sum[x,2]<-var #add value for slope of 'i' into column two from dataframe sum
}

colnames(sum)<-c("Year","CV") #column names for year and slope
sum 
sum.nb <- sum

##Fig. 5###
par(mfrow=c(5,1))
par(mai=c(0.6,0.5,0.05,0.15))
boxplot(bats.sum$CV,type="p",xlab=" ",pch=19,las=1,ylim=c(30,180),
        ylab=" ",cex.axis=1.5,cex=1.5,horizontal = TRUE)
stripchart(bats.sum$CV, method = "jitter", pch = 19, add = TRUE, col = "deeppink",cex=1.5)
text(20, 1, cex=1.5, xpd = NA, srt =90, "BATS")

boxplot(sum.cb$CV,type="p",xlab=" ",pch=19,las=1,cex.axis=1.5,cex=1.5,
        ylab=" ",horizontal = TRUE,ylim=c(30,180))  
stripchart(sum.cb$CV, method = "jitter", pch = 19, add = TRUE, col = "aquamarine4",cex=1.5)
text(20, 1, cex=1.5, xpd = NA, srt =90, "CBay")

boxplot(sum.l4$CV,type="p",las=1,cex.axis=1.5,cex=1.5,ylim=c(30,180),
        ylab=" ",pch=19,xlab=" ",horizontal = TRUE)
stripchart(sum.l4$CV, method = "jitter", pch = 19, add = TRUE, col = "purple3",cex=1.5)
text(20, 1, cex=1.5, xpd = NA, srt =90, "L4")

boxplot(sum.sf$CV,type="p",las=1,cex.axis=1.5,cex=1.5,ylim=c(30,180),
        ylab=" ",pch=19,xlab=" ",horizontal = TRUE) 
stripchart(sum.sf$CV, method = "jitter", pch = 19, add = TRUE, col = "orange2",cex=1.5)
text(20, 1, cex=1.5, xpd = NA, srt =90, "SFBay")

boxplot(sum.nb$CV,type="p",las=1,cex.axis=1.5,cex=1.5,ylim=c(30,180),
        ylab=" ",pch=19,xlab=" ",horizontal = TRUE)  
stripchart(sum.nb$CV, method = "jitter", pch = 19, add = TRUE, col = "blue3",cex=1.5)
text(20, 1, cex=1.5, xpd = NA, srt =90, "NBay")
text(100, -0.45, cex=1.5, xpd = NA, srt =360, "CV (%)")

library(TSA)
library(tseries)
library(artfima)
library(arfima)
library(INLA)
library(lmtest)
library(Hmisc)

#setwd("C:/Users/pthibodeau1/OneDrive - University of New England/URI/NBPTS/Data/BCO-DMO/Github")

#raw data are from bco-dmo dataset, just renamed to match the code used to run the DLM below#
data_mod  = read.csv("raw.data.dlm.csv", header=T) 
#NBPTS file
data_mod_ts=ts(data_mod, start=1970,frequency=52)
N = dim(data_mod_ts)[1]

#####env param DLMs#####
### Si

Yt = data_mod$Si.raw
xt = data_mod$merl.Si
ind1 = which(Yt==0)
ind2 = which(xt==0)
Yt[ind1]=0.01
xt[ind2]=0.01

id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id0,id1,id2,Yt,xt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")
lines(data_mod$Si,col='blue')

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

Si_fits = fits
Si_new  = data_mod$Si.raw
Si_new[ind3] = fits[ind3]
plot(Si_fits,col="blue")
lines(Si_new,col="red")
plot(Si_new-Si_fits)


plot(seq(1970,2019,length.out=2600),Yt,col="lightblue", ylab="Si04",xlab="Time")
lines(seq(1970,2019,length.out=2600),fits,col="red")
lines(seq(1970, 2019, length.out=2600), exp(2.165+r1$summary.random$id0[,2]),col="blue", lwd=2)

legend("topleft", c("data","fitted values","trend"), lty=1, col=c("lightblue","red","blue"))


### NH4


Yt = data_mod$NH4.raw
xt = data_mod$merl.NH4
ind1 = which(Yt==0)
ind2 = which(xt==0)
Yt[ind1]=0.01
xt[ind2]=0.01

id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id0,id1,id2,Yt,xt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")
lines(data_mod$NH4,col='blue')

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

NH4_fits = fits
NH4_new  = data_mod$NH4.raw
NH4_new[ind3] = fits[ind3]
plot(NH4_fits,col="blue")
lines(NH4_new,col="red")
plot(NH4_new-NH4_fits)



plot(seq(1970,2019,length.out=2600),Yt,col="lightblue", ylab="NH4",xlab="Time",ylim=c(0,23))
lines(seq(1970,2019,length.out=2600),fits,col="red")
lines(seq(1970, 2019, length.out=2600), exp(0.297+r1$summary.random$id0[,2]),col="blue", lwd=2)
legend("topleft", c("data","fitted values","trend"), lty=1, col=c("lightblue","red","blue"))



### NO32


Yt = data_mod$NO32.raw
xt = data_mod$merl.NO32
ind1 = which(Yt<=0)
ind2 = which(xt<=0)
Yt[ind1]=0.01
xt[ind2]=0.01

id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id0,id1,id2,Yt,xt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt,col="lightblue")
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")
lines(data_mod$NO32,col='blue')

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

NO32_fits = fits
NO32_new  = data_mod$NO32.raw
NO32_new[ind3] = fits[ind3]
plot(NO32_fits,col="blue")
lines(NO32_new,col="red")
plot(NO32_new-NO32_fits)


plot(seq(1970,2019,length.out=2600),Yt,col="lightblue", ylab="NO32",xlab="Time")
lines(seq(1970,2019,length.out=2600),fits,col="red")
lines(seq(1970, 2019, length.out=2600), exp(0.064+r1$summary.random$id0[,2]),col="blue", lwd=2)
legend("topleft", c("data","fitted values","trend"), lty=1, col=c("lightblue","red","blue"))

### PO4


Yt = data_mod$PO4.raw
xt = data_mod$merl.PO4
ind1 = which(Yt<=0)
ind2 = which(xt<=0)
Yt[ind1]=0.01
xt[ind2]=0.01

id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id0,id1,id2,Yt,xt),
           control.predictor=list(compute=TRUE))
summary(r1)



ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")
lines(data_mod$PO4,col='blue')

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

PO4_fits = fits
PO4_new  = data_mod$PO4.raw
PO4_new[ind3] = fits[ind3]
plot(PO4_fits,col="blue")
lines(PO4_new,col="red")
plot(PO4_new-PO4_fits)

plot(seq(1970,2019,length.out=2600),Yt,col="lightblue", ylab="PO4",xlab="Time")
lines(seq(1970,2019,length.out=2600),fits,col="red")
lines(seq(1970, 2019, length.out=2600), exp(r1$summary.random$id0[,2]),col="blue", lwd=2)
legend("topleft", c("data","fitted values","trend"), lty=1, col=c("lightblue","red","blue"))

#### Surface Light
SL = (data_mod$light)
plot(SL)

Yt = SL
ind1 = which(Yt>2000) #remove outliers
Yt[ind1]=NA
plot(Yt)

#id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  #f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id1,id2,Yt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

SL_fits = fits
SL_new  = SL
SL_new[ind3] = fits[ind3]
plot(SL_fits,col="blue")
lines(SL_new,col="red")
plot(SL_new-SL_fits)

###Secchi depth
SD = (data_mod$Secchi.depth)
plot(SD)

Yt = SD
ind1 = which(Yt>10 | Yt==0) #remove outliers
Yt[ind1]=NA
plot(Yt)

#id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  #f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id1,id2,Yt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

SD_fits = fits
SD_new  = SD
SD_new[ind3] = fits[ind3]
plot(SD_fits,col="blue")
lines(SD_new,col="red")
plot(SD_new-SD_fits)




### Salinity_bottom
SB = data_mod$SAL_BOT
plot(SB)

Yt = SB
ind1 = which(Yt<=20) #remove outliers
Yt[ind1]=NA
plot(Yt)

#id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  #f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id1,id2,Yt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

SB_fits = fits
SB_new  = SB
SB_new[ind3] = fits[ind3]
plot(SB_fits,col="blue")
lines(SB_new,col="red")
plot(SB_new-SB_fits)




### Salinity_surface
SF = data_mod$SAL_SURF
plot(SF)

Yt = SF
ind1 = which(Yt<=20) #remove outliers
Yt[ind1]=NA
plot(Yt)

#id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  #f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id1,id2,Yt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = exp(ub[ind3])
lb[ind3] = exp(lb[ind3])

SF_fits = fits
SF_new  = SF
SF_new[ind3] = fits[ind3]
plot(SF_fits,col="blue")
lines(SF_new,col="red")
plot(SF_new-SF_fits)


### temp_bottom
TB = data_mod$Temperature_BOT
plot(TB)

Yt = TB
ind1 = which(Yt>=30)
Yt[ind1]=NA
plot(Yt)

#id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  #f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gaussian"), data = data.frame(id1,id2,Yt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = (r1$summary.fitted.values[ind3,1])
lines(fits,col="red")

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = (ub[ind3])
lb[ind3] = (lb[ind3])

TB_fits = fits
TB_new  = TB
TB_new[ind3] = fits[ind3]
plot(TB_fits,col="blue")
lines(TB_new,col="red")
plot(TB_new-TB_fits)


### temp_surface
TF = data_mod$Temperature_SURF
plot(TF)

Yt = TF
#ind1 = which(Yt>=30)
#Yt[ind1]=NA
plot(Yt)

#id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)


x1 = as.numeric(harmonic(data_mod_ts[,4],1)[,1])
x2 = as.numeric(harmonic(data_mod_ts[,4],1)[,2])

formula1 <- Yt ~ 
  #f(id0, xt, model="rw1",param=c(.5,.01),constr=F) +
  f(id1, x1, model="rw1",param=c(1,1),constr=F) +
  f(id2, x2, model="rw1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gaussian"), data = data.frame(id1,id2,Yt),
           control.predictor=list(compute=TRUE))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = (r1$summary.fitted.values[ind3,1])
lines(fits,col="red")

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = (ub[ind3])
lb[ind3] = (lb[ind3])

TF_fits = fits
TF_new  = TF
TF_new[ind3] = fits[ind3]
plot(TF_fits,col="blue")
lines(TF_new,col="red")
plot(TF_new-TF_fits)



## Chlorophyl DLM

CH = data_mod$chla[105:2600]
ind = which(is.na(CH)==F)[1]
Yt = CH[ind: length(CH)]

ind1 = which(Yt==0)
Yt[ind1]=0.001

plot(Yt)

id0 = 1:length(Yt)   # indices for beta_1
id1 = 1:length(Yt)
id2 = 1:length(Yt)

x0 = rep(1,length(Yt))
x1 = as.numeric(harmonic(ts(Yt,start = 1972, frequency =52),1)[,1])
x2 = as.numeric(harmonic(ts(Yt,start = 1972, frequency =52),1)[,2])

formula1 <- Yt ~ 
  #f(id0, xt, model="rw1",param=c(.5,.01),constr=F) + 
  f(id1, x1, model="ar1",param=c(1,1),constr=F) +
  f(id2, x2, model="ar1",param=c(1,1),constr=F) 
r1 <- inla(formula1, family=c("gamma"), data = data.frame(id0,id1,id2,Yt),
           control.compute = list(return.marginals=TRUE, return.marginals.predictor=TRUE), control.family=list(link='log'))
summary(r1)

plot(Yt)
ind3 = which(is.na(Yt)==T)
fits = r1$summary.fitted.values[,1]
fits[ind3] = exp(r1$summary.fitted.values[ind3,1])
lines(fits,col="red")

lb = r1$summary.fitted.values[1:N,3]
ub = r1$summary.fitted.values[1:N,5]
ub[ind3] = (ub[ind3])
lb[ind3] = (lb[ind3])

CH_fits = fits
CH_new  = CH
CH_new[ind3] = fits[ind3]
plot(CH_fits,col="lightblue")
lines(CH_new,col="red")

#####fig. 2A####
tiff(filename = "Figure2A.tiff", width = 3.543, height = 2.363, units = "in", pointsize = 5,
     compression = "lzw", bg = "white", res = 600)
#1972-2019
#4 points removed when limit y-axis to 70
par(mai=c(0.5,0.35,0.25,0.05))
par(mfrow=c(1,3))
plot(seq(1972,2019,length.out=2491),Yt,xlim=c(1970,2020),
     col="grey55", ylab=" ",xlab=" ",las=2,cex.axis=1.5,ylim=c(0,70))
text(1950, 35, cex=1.5, labels = expression("Chlorophyll a"~ " (mg m"^"-3"*")"),
     xpd = NA, srt =90)
text(1995, -18, cex=1.5,
     xpd = NA, srt =1, "Year")
minor.tick(nx=10, tick.ratio=0.5,ny='')
mtext(side=3, at=c(1970), las=1, cex=1.25, "A")
par(new=T)
lines(seq(1972,2019,length.out=2491),fits,col="black",ylim=c(0,70))
chl.mean <- aggregate(fits,by=list(year=(data_mod$Year[105:2595])),FUN=mean)
par(new=T)
plot(chl.mean$year,chl.mean$x,type="l",las=1,xaxt="n",yaxt="n",
     ylim=c(0,70),col="green",lwd=1.5,ylab=" ",xlab=" ")



## Stratification 
library(gsw)
SA <- c(30.19, 30.73) #parameter constants set for plankton time series location
CT <- c(-0.4, -0.4)
p <- c(      0,      8)
latitude <- 41
r <- gsw_Nsquared(SA, CT, p, latitude=41) #stratification equation

#equation as a loop:

stratification = rep(0,length(SB))
p <- c(      0,      8)
latitude <- 41

for(i in 1:length(SB)){
  SA <- c(SF_new[i],SB_new[i])
  CT <- c(TF_new[i],TB_new[i])
  r <- gsw_Nsquared(SA, CT, p, latitude=41)   #stratification equation
  stratification[i]<-r$N2*1e3 #output value for stratification
}

plot(stratification,type="l")




### SARIMAX Chl model#####

data_mod_ts2=ts(data_mod, start=1,frequency=26)
data_mod_ts3=ts(data_mod, start=1,frequency=52)

###

cor(cbind(PO4_new,NH4_new,Si_new,NO32_new,SD_new,SL_new,
          stratification,TF_new,SF_new,data_mod[,12]))

data_mod2 = data.frame(data_mod[,1:2],data_mod[,11],PO4_new,NH4_new,Si_new,NO32_new,SD_new,SL_new,
                       stratification,TF_new,SF_new,data_mod[,12])

new_data = cbind(data_mod2[-c(1:6),3],
                 data_mod2[-c(1:6),4:13],
                 data_mod2[-c(1:5,N),4:13],
                 data_mod2[-c(1:4,N,N-1),4:13],
                 data_mod2[-c(1:3,N,N-1,N-2),4:13],
                 data_mod2[-c(1:2,N,N-1,N-2,N-3),4:13],
                 data_mod2[-c(1,N,N-1,N-2,N-3,N-4),4:13],
                 data_mod2[-c(N,N-1,N-2,N-3,N-4,N-5),4:13])

names(new_data) = c("Chl",
                    "PO4","NH4","Si","NO32","secchi","light","strat","Temp","Salinity","Precip",
                    "PO4_lag1","NH4_lag1","Si_lag1","NO32_lag1","secchi_lag1","light_lag1","strat_lag1","Temp_lag1","Sal_lag1","Precip_lag1",
                    "PO4_lag2","NH4_lag2","Si_lag2","NO32_lag2","secchi_lag2","light_lag2","strat_lag2","Temp_lag2","Sal_lag2","Precip_lag2",
                    "PO4_lag3","NH4_lag3","Si_lag3","NO32_lag3","secchi_lag3","light_lag3","strat_lag3","Temp_lag3","Sal_lag3","Precip_lag3",
                    "PO4_lag4","NH4_lag4","Si_lag4","NO32_lag4","secchi_lag4","light_lag4","strat_lag4","Temp_lag4","Sal_lag4","Precip_lag4",
                    "PO4_lag5","NH4_lag5","Si_lag5","NO32_lag5","secchi_lag5","light_lag5","strat_lag5","Temp_lag5","Sal_lag5","Precip_lag5",
                    "PO4_lag6","NH4_lag6","Si_lag6","NO32_lag6","secchi_lag6","light_lag6","strat_lag6","Temp_lag6","Sal_lag6","Precip_lag6"
)                    

data_mod_ts = ts(new_data,start=time(data_mod_ts[6]))

#####granger causality#####
#temp granger causes nuts with 5 lags
grangertest(data_mod_ts[,9],data_mod_ts[,2],5) 
grangertest(data_mod_ts[,9],data_mod_ts[,3],5) 
grangertest(data_mod_ts[,9],data_mod_ts[,4],5) 
grangertest(data_mod_ts[,9],data_mod_ts[,5],5) 

#temp granger causes chl
grangertest(data_mod_ts[,9],data_mod_ts[,1],5) 

#nuts granger cause chl
grangertest(data_mod_ts[,2],data_mod_ts[,1],5) 
grangertest(data_mod_ts[,3],data_mod_ts[,1],5) 
grangertest(data_mod_ts[,4],data_mod_ts[,1],5)
grangertest(data_mod_ts[,5],data_mod_ts[,1],5) 

#salinity granger cause chl
grangertest(data_mod_ts[,10],data_mod_ts[,1],5) 

#secchi depth granger cause chl
grangertest(data_mod_ts[,6],data_mod_ts[,1],5) 

#light granger cause chl
grangertest(data_mod_ts[,7],data_mod_ts[,1],5) 

#precip granger cause chl
grangertest(data_mod_ts[,11],data_mod_ts[,1],5)

#strat granger cause chl
grangertest(data_mod_ts[,8],data_mod_ts[,1],5) 

#strat granger cause nuts
grangertest(data_mod_ts[,8],data_mod_ts[,2],5) 
grangertest(data_mod_ts[,8],data_mod_ts[,3],5) 
grangertest(data_mod_ts[,8],data_mod_ts[,4],5)
grangertest(data_mod_ts[,8],data_mod_ts[,5],5)

#####monte carlo simulations#####
mod0 = lm(data_mod_ts[,1]~data_mod_ts[,2:31] )
summary(mod0)
acf(residuals(mod0),204)
pacf(residuals(mod0),204)


ind3 = which(is.na(Yt)==T)
fits = rnorm(2685,r1$summary.fitted.values[,1],r1$summary.fitted.values[,2])
fits[ind3] = exp(fits[ind3])
CH_fits = fits
CH_new  = Yt
CH_new[ind3] = fits[ind3]
new_data$Chl = CH_new[92:2685]
data_mod_ts = ts(new_data,start=time(data_mod_ts[6]))  


A6 = list()
nsim = 3


for(i in 1:nsim){
  ind3 = which(is.na(Yt)==T)
  fits = rnorm(2685,r1$summary.fitted.values[,1],r1$summary.fitted.values[,2])
  fits[ind3] = exp(fits[ind3])
  CH_fits = fits
  CH_new  = Yt
  CH_new[ind3] = fits[ind3]
  new_data$Chl = CH_new[92:2685]
  data_mod_ts = ts(new_data,start=time(data_mod_ts[6]))  
  mod49=arima(x = data_mod_ts[, 1], order = c(2, 0, 1), 
              seasonal = list(order = c(0,1, 1), period = 52), 
              xreg = data_mod_ts[,c(3:6,9:10,13:15,20,23,25,30,35)])
  A6[[i]] = coeftest(mod49)
  print(i)
} 


dim(A6[[1]])

acf(residuals(mod49),na.action=na.pass,20)
pacf(residuals(mod49),na.action=na.pass,20)
A6 

pvec = rep(0, dim(A6[[1]])[1])
for(j in 1:length(A6)){
  pp = as.numeric(A6[[j]][,4]<0.05)
  pvec = pvec+pp  
}

pp1 = pvec / length(A6)
ind1 = which(pp1>0)

coeftest(mod49)[ind1,]
coeftest(mod49)

#####Table 2#####
apply(cmat,1,mean)
apply(cmat,1,sd)


#####phenology models#####
#Chlorophyll a phenology defined with a threshold approach of > 5% of the annual 
#chl a median for each year for the winter-spring (wks 1-16) and summer-fall 
#(wks 22-38) time periods. Environmental data were aggregated over the same 
# weeks and seasons as the chl a data.

###how to calculate mean environmental factors for each season#
### Winter

dd2 = read.csv("phen.win.bco.dmo.csv", header=T)
names(dd2)
plot(dd2$winMax)
plot(dd2$winStart)
dd2 = data.frame(dd2)

#filling in raw env factors with modeled data
for(i in 1:length(unique(dd2$Year))){
  ind = which(data_mod$Year==unique(dd2$Year)[i])
  dd2$SiO4[i] = mean(Si_new[ind][1:16])
  dd2$NH4[i] = mean(NH4_new[ind][1:16])
  dd2$NO32[i] = mean(NO32_new[ind][1:16])
  dd2$PO4[i] = mean(PO4_new[ind][1:16])
  dd2$Stratification[i] = mean(stratification[ind][1:16]) 
  dd2$Light[i] = mean(SL_new[ind][1:16]) 
  dd2$Secchi[i] = mean(SD_new[ind][1:16])
}


#all parameters
mod1 = glm(dd2$winStart~dd2$Year+dd2$NAO+dd2$Temperature+dd2$RSWT+
             dd2$Light+dd2$Secchi+dd2$Stratification+dd2$GSI
           +dd2$PO4+dd2$SiO4+dd2$NH4+dd2$NO32+dd2$salinity,family="poisson")

summary(mod1)

#redoing w/significant factors
mod2 = glm(dd2$winStart~dd2$Year+dd2$Secchi+dd2$GSI+dd2$SiO4
           +dd2$NO32+dd2$salinity,family="poisson")
summary(mod2)

mod2b = lm(dd2$winStart~dd2$Year)
summary(mod2b)

#####Fig.4#####
tiff(filename = "Figure4.tiff", width = 11, height = 11, units = "cm", pointsize = 10,
     compression = "lzw", bg = "white", res = 600)

par(mfrow=c(2,3))
par(mai=c(0.45,0.40,0.15,0.05))

plot(dd2$Year,dd2$winStart,las=2,ylab=" ",pch=19,xlab=" ",cex=1.25,
     xlim=c(1970,2020),cex.axis=1.25,ylim=c(0,16))
lines(dd2$Year[-c(27:28,41,48)],fitted(mod2),col="aquamarine3",lwd=2)
minor.tick(nx=10, tick.ratio=0.5,ny='')
text(1950, 7.5, cex=1.25, labels = "Start week",
     xpd = NA, srt =90)
mtext(side=3, at=c(1971), las=1, cex=1, "A")

mod5 = lm(winMax~Year,data=dd2)
summary(mod5)

mod4 = glm(dd2$winMax~dd2$salinity+dd2$Secchi+dd2$GSI+dd2$Stratification
           +dd2$NO32,family="poisson")  
summary(mod4) 

plot(dd2$Year,dd2$winMax,ylab=" ",pch=19,xlab=" ",las=2,cex=1.25
     ,ylim=c(0,16),cex.axis=1.25, xlim=c(1970,2020))
lines(dd2$Year[-summary(mod4)$na.action],fitted(mod4),lwd=2,col="aquamarine3")
minor.tick(nx=10, tick.ratio=0.5,ny='')
text(1950, 7.5, cex=1.25, labels = "Maximum week",
     xpd = NA, srt =90)
mtext(side=3, at=c(1971), las=1, cex=1, "B")

mod7 = glm(dd2$winDur~dd2$GSI
           +dd2$SiO4+dd2$NO32,family="poisson")
summary(mod7)

mod8 = lm(dd2$winDur~dd2$Year)
summary(mod8)
#not significant

plot(dd2$Year,dd2$winDur,ylab=" ",pch=19,xlab=" ",las=2,
     ylim=c(0,16),cex.axis=1.25,cex=1.25,xlim=c(1970,2020))
lines(dd2$Year[-summary(mod7)$na.action],fitted(mod7),lwd=2,col="aquamarine3")
minor.tick(nx=10, tick.ratio=0.5,ny='')
text(1950, 7.5, cex=1.25, labels = "Duration (weeks)",
     xpd = NA, srt =90)
mtext(side=3, at=c(1971), las=1, cex=1, "C")


##
### Summer

dd2 = read.csv("phen.sum.bco.dmo.csv", header=T)
names(dd2)
dd2 = data.frame(dd2)

#filling in raw env factors with modeled data
for(i in 1:length(unique(dd2$Year))){
  ind = which(data_mod$Year==unique(dd2$Year)[i])
  dd2$SiO4[i] = mean(Si_new[ind][22:38])
  dd2$NH4[i] = mean(NH4_new[ind][22:38])
  dd2$NO32[i] = mean(NO32_new[ind][22:38])
  dd2$PO4[i] = mean(PO4_new[ind][22:38])
  dd2$Stratification[i] = mean(stratification[ind][22:38]) 
  dd2$Light[i] = mean(SL_new[ind][22:38]) 
  dd2$Secchi[i] = mean(SD_new[ind][22:38])
}


#start
#all parameters
mod1 = glm(dd2$sumStart~dd2$Year+dd2$NAO+dd2$Temperature+dd2$RSWT+
             dd2$Secchi+dd2$GSI+dd2$salinity
           +dd2$PO4+dd2$SiO4+dd2$NH4+dd2$NO32,family="poisson")

summary(mod1)

mod2 = glm(dd2$sumStart~dd2$NAO,family="poisson") 
#can't find sig. model but lowest AIC

summary(mod2)

mod2b = lm(dd2$sumStart~dd2$Year)

summary(mod2b)

plot(dd2$Year,dd2$sumStart,ylab=" ",pch=19,xlab=" ",las=2,
     ylim=c(22,37),cex.axis=1.25,cex=1.25,xlim=c(1970,2020))
lines(dd2$Year,fitted(mod2),lwd=2,col="purple3")
minor.tick(nx=10, tick.ratio=0.5,ny='')
text(1950, 30, cex=1.25, labels = "Start week",
     xpd = NA, srt =90)
mtext(side=3, at=c(1971), las=1, cex=1, "D")

#max

#all parameters
mod3 = glm(dd2$sumMax~dd2$Year+dd2$NAO+dd2$Temperature+dd2$RSWT+
             dd2$Secchi+dd2$GSI+dd2$salinity
           +dd2$PO4+dd2$SiO4+dd2$NH4+dd2$NO32,family="poisson")

summary(mod3)

#not significant but lowest AIC
mod4 = glm(dd2$sumMax~dd2$Temperature
           ,family="poisson")

summary(mod4)

mod4b = lm(dd2$sumMax~dd2$Year)
summary(mod4b)

plot(dd2$Year,dd2$sumMax,ylab=" ",pch=19,xlab=" ",las=2,
     ylim=c(22,37),cex.axis=1.25,cex=1.25,xlim=c(1970,2020))
lines(dd2$Year,fitted(mod4),lwd=2,col="purple3")
minor.tick(nx=10, tick.ratio=0.5,ny='')
text(1950, 30, cex=1.25, labels = "Maximum week",
     xpd = NA, srt =90)
mtext(side=3, at=c(1971), las=1, cex=1, "E")

#duration
#all parameters

mod6 = glm(dd2$sumDur~dd2$Year+dd2$NAO+dd2$Temperature+dd2$RSWT+
             dd2$Secchi+dd2$GSI+dd2$salinity+
             +dd2$PO4+dd2$SiO4+dd2$NH4+dd2$NO32,family="poisson")

summary(mod6)

mod7 = glm(dd2$sumDur~dd2$Year+dd2$RSWT+
             dd2$salinity+
             +dd2$SiO4,family="poisson")
summary(mod7)

mod8 = lm(dd2$sumDur~dd2$Year)
summary(mod8)

plot(dd2$Year,dd2$sumDur,ylab=" ",pch=19,xlab=" ",las=2,
     cex.axis=1.25,cex=1.25,xlim=c(1970,2020),ylim=c(0,16))
lines(dd2$Year[-summary(mod7)$na.action],fitted(mod7),lwd=2,col="purple3")
#lines(dd2$Year,fitted(mod8),lwd=3,col="black",lty=2)
minor.tick(nx=10, tick.ratio=0.5,ny='')
text(1950, 8, cex=1.25, labels = "Duration (weeks)",
     xpd = NA, srt =90)
mtext(side=3, at=c(1971), las=1, cex=1, "F")

dev.off()
