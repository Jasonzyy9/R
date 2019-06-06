OurData <- read.csv("D:/STUDY/Data Mining Applications/final/mayorsfoodcourt.csv")
head(OurData)

#BusinessName = OurData$businessName
#BusinessName
#BusinessName1 <- unique(BusinessName)
#BusinessName1

head(BusinessName)
head(OurData)

BusinessName2 <- data.frame(OurData$businessName)
head(BusinessName2)
BusinessName3 <- count(BusinessName2,"OurData.businessName")
BusinessName3

positions <- order(BusinessName3$freq, decreasing = TRUE)
positions
BusinessName5 <- BusinessName3[positions, ]
BusinessName5

##Figure out which 6 restaurants violate most
BusinessName6 <- head(BusinessName5)
ggplot(BusinessName6, aes(x=OurData.businessName, y=freq)) + geom_bar(stat="identity")

##Extract ViolLevel and businessName
BusinessVio <- data.frame(OurData$businessName,OurData$ViolLevel)
BusinessVio


##violation <- c('*')
##BusinessVio[is.element(BusinessVio$OurData.ViolLevel,violation),]
#Count the freq of different ViolLevels
Vio1 <- count(BusinessVio,"OurData.ViolLevel")
Vio1 <- Vio1[-c(1,2,6),]
Vio1

#Pie Chart
slices <- c(358354, 29862, 82947)
lbls <- c("*", "**", "***")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%", sep="")
pie(slices, labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of VioLevel")

#Relationship between Violation Status and Violation Level
StaVio <- data.frame(OurData$ViolStatus, OurData$ViolLevel)
head(StaVio)

StaVioCount <- count(StaVio, c("OurData.ViolStatus", "OurData.ViolLevel"))
StaVioCount <- StaVioCount[-c(1,2,3,4,5,9),]
StaVioCount
PassRate1 <- StaVioCount[4,3]/(StaVioCount[1,3]+ StaVioCount[4,3])
PassRate2 <- StaVioCount[5,3]/(StaVioCount[2,3]+ StaVioCount[5,3])
PassRate3 <- StaVioCount[6,3]/(StaVioCount[3,3]+ StaVioCount[6,3])
PassRate1
PassRate2
PassRate3

#Comparison between Reatail and Eating
BusinessDes <- data.frame(OurData$DESCRIPT)
Des <- count(BusinessDes,"OurData.DESCRIPT")
Des

library(plotrix)
slices2 <- c(237601, 193616, 3374, 66314)
lbls2 <- c("Eating & Drinking", "Eating & Drinking w/ Take out", "Mobile Food Walk on", "Retail Food")
pie3D(slices2, labels= lbls2 , explode=0.1,
      main = "Different types of business which is supposed to violate")

#Different years' violations
VioYear <- data.frame(OurData$VIOLDTTM)
VioYear
head(VioYear)

VioYear$OurData.VIOLDTTM = as.Date(VioYear$OurData.VIOLDTTM, format="%Y/%m/%d %I:%M")
VioYear$OurData.VIOLDTTM = as.Date(format(VioYear$OurData.VIOLDTTM, "%Y/%m/%d"))
head(VioYear$OurData.VIOLDTTM)

VioYear1 <- format(as.Date(VioYear$OurData.VIOLDTTM, format="%Y-%m-%d"),"%Y-%m")
head(VioYear1)
VioYearCount <- count(VioYear1)
VioYearCount <- VioYearCount[-c(133),]
VioYearCount

VioMonth <- format(as.Date(VioYear$OurData.VIOLDTTM, format="%Y-%m-%d"),"%m")
head(VioMonth)
VioMonthCount <- count(VioMonth)
VioMonthCount <- VioMonthCount[-c(13),]
VioMonthCount
scatter.smooth(x=VioMonthCount$x, y=VioMonthCount$freq, main="Summary of Violations", xlab = "Month samples", ylab = "Violations")


#lm=glm(freq~x,data=VioYearCount)
#summary(lm)

pacf(VioYearCount)
x <- ts(VioYearCount,start=c(2007,1,15),frequency=7)
x1 <- forecast(x, h=12)
plot(x)
plot(x1, main ="forcast")

#plot.ts(VioYearCount)
scatter.smooth(x=VioYearCount$x, y=VioYearCount$freq, main="Summary of Violations", xlab = "Month samples", ylab = "Violations")

#OurModel <- naiveBayes(freq~., data = VioYearCount)
#OurModel


