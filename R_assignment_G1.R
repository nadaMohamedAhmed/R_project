getwd()
myData <- read.csv('G1_Allometry.csv')
View(myData)
summary(myData)
str(myData)

#1)remove the text("") &(convert , to . ) in height feature to convert it to numeric to use in the analysis
myData$height<-gsub("\\“", '',myData$height)
myData$height<-gsub("\\”", '',myData$height)
myData$height<-gsub(",", '.',myData$height)
myData
myData$height<-as.numeric(myData$height)

str(myData)
summary(myData)

#2)To get all locations of NA
complete.cases(myData)

#3)get all row with missing data for specific variable
any(is.na(myData$height))

#4) Get all rows contain missing data.
myData[ ! complete.cases(myData), ]

#5)Replace each NA in height according to the mean of [
#height to all (PSME) & height to all (PIPO) & height to all (PIMO) ]
PSME_mean <-mean(myData[ myData$species == 'PSME', 'height'], na.rm = T)
PSME_mean
myData[is.na(myData$height) & myData$species == 'PSME', 'height'] <-PSME_mean
myData

PIPO_mean <-mean(myData[ myData$species == 'PIPO', 'height'], na.rm = T)
PIPO_mean
myData[is.na(myData$height) & myData$species == 'PIPO', 'height'] <-PIPO_mean
myData

PIMO_mean <-mean(myData[ myData$species == 'PSME', 'height'], na.rm = T)
PIMO_mean
myData[is.na(myData$height) & myData$species == 'PIMO', 'height'] <-PIMO_mean
myData

#6)Subset only species (PIMO)
sub1<-myData[myData$species == 'PIMO', ]
sub1

#7)Subset only species (PSME) who have leafarea greater than  100
sub2<-myData[myData$species == 'PSME' & myData$leafarea >100 , ]
sub2

#8)Subset species who have branchmass greater than the median and have height 
#greater than or equal to 35  for specific col (species , height , branchmass)
sub3<-myData[myData$branchmass > median(myData$branchmass) &
               myData$height >= 35 ,c(1,3,5) ]
sub3

#9)sort the data set ascending according to 2 variables
sorted<-myData[order(myData$height ,myData$leafarea) , ]
sorted

#10)Get only the first 15 rows
h<-head(myData ,15)

#11)Get only the last 20 rows
t<-tail(myData ,20)

#-------------------------------------------------------
#now we will make visuilization for our data set

library(tidyverse)
library(ggplot2)

#12)display the effect of the diameter and branchmass
fig1<-ggplot(myData)
fig1<-ggplot(myData, aes( x=diameter, y=branchmass))
fig1 + geom_point()

#13)Show the distribution of height using histogram
fig2<-ggplot(myData , aes(height))
fig2
fig2 + geom_histogram(binwidth =3)
fig2 + geom_histogram(color = "darkslategray", fill="red", alpha=.5)+
  ggtitle("species's height distribution")+labs(x="height" , y="Frequency")

#14 using Bar Chart
fig3<-ggplot(myData , aes(x= diameter ,fill= species))
fig3 +geom_bar()+ theme_light()

