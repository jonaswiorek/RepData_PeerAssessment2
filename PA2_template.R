library(dplyr)
library(ggplot2)
library(gridExtra)

if(!file.exists("data")){
        dir.create("data")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile="./data/repdata-data-StormData.csv.bz2", method="curl")
stormData <- read.csv(bzfile("./data/repdata-data-StormData.csv.bz2"))

#1
eventFatalitiesInjuries <- summarize(group_by(stormData, EVTYPE), 
                                     fatalities = sum(FATALITIES, na.rm=TRUE),
                                     injuries = sum(INJURIES, na.rm =TRUE))


fatalitiesOrdered <- arrange(eventFatalitiesInjuries, desc(fatalities))
fatalitiesOrderedFiltered <- fatalitiesOrdered[1:10,]

injuriesOrdered <- arrange(eventFatalitiesInjuries, desc(injuries))
injuriesOrderedFiltered <- injuriesOrdered[1:10,]


ggplot(data=fatalitiesOrderedFiltered,aes(x=EVTYPE,y=fatalities)) +
  geom_bar( stat="identity") +
  scale_x_discrete(limits = fatalitiesOrderedFiltered$EVTYPE) +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=90))

ggplot(data=injuriesOrderedFiltered,aes(x=EVTYPE,y=injuries)) +
  geom_bar( stat="identity") +
  scale_x_discrete(limits = injuriesOrderedFiltered$EVTYPE) +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=90))

par(mar=c(7,4,4,2))
barplot(fatalitiesOrderedFiltered$fatalities, names.arg = fatalitiesOrderedFiltered$EVTYPE, las =3, cex.names = 0.6)
barplot(injuriesOrderedFiltered$injuries, names.arg = injuriesOrderedFiltered$EVTYPE, las =3, cex.names = 0.6)
par(mar=c(5,4,4,2))


#2
eventPropertyCrop <- summarize(group_by(stormData, EVTYPE), 
                                     property = sum(PROPDMG, na.rm=TRUE),
                                     crop = sum(CROPDMG, na.rm =TRUE))

propertyOrdered <- arrange(eventPropertyCrop, desc(property))
propertyOrderedFiltered <- propertyOrdered[1:10,]

cropOrdered <- arrange(eventPropertyCrop, desc(crop))
cropOrderedFiltered <- cropOrdered[1:10,]
par(mfrow = c(2,1))
g3 <- ggplot(data=propertyOrderedFiltered,aes(x=EVTYPE,y=property)) +
        geom_bar( stat="identity") +
        scale_x_discrete(limits = propertyOrderedFiltered$EVTYPE) +
        theme(text = element_text(size=12), axis.text.x = element_text(angle=90))

g4 <- ggplot(data=cropOrderedFiltered,aes(x=EVTYPE,y=crop)) +
        geom_bar( stat="identity") +
        scale_x_discrete(limits = cropOrderedFiltered$EVTYPE) +
        theme(text = element_text(size=12), axis.text.x = element_text(angle=90))
grid.arrange(g3,g4)




expConverter <- function(exp) {
        times <- 1
        if (as.character(exp) == "B") {
                times <- 10^9
        }
         else if (as.character(exp) == "M") {
                 times <- 10^6
         }
         else if (as.character(exp) == "m") {
                 times <- 10^6
         }
         else if (as.character(exp) == "K") {
                 times <- 10^3
         }
         else if (as.character(exp) == "k") {
                 times <- 10^3
         }
         else if (as.character(exp) == "H") {
                 times <- 10^2
         }
         else if (as.character(exp) == "h") {
                 times <- 10^2
         }
          else if (exp == 1) {
                   times <- 10^as.integer(exp)
           } 
        else {
                times <- 1
        } 
        
        times
}




# qplot(x=EVTYPE, y=fatalities, data= fatalitiesOrderedFiltered, geom="bar")
# 
# 
# barplot(fatalitiesOrdered$fatalities[1:20])
# 
# barplot(xtabs(~fatalitiesOrdered$fatalities[1:5]))
# 
# eventFatalities <- summarize(group_by(stormData, EVTYPE), 
#                                      fatalities = sum(FATALITIES, na.rm=TRUE))
# 
# fatalitiesOrderedTest <- arrange(eventFatalities, desc(fatalities))
# 
# mp <-barplot(fatalitiesOrdered$fatalities[1:20],xaxt='n')
# axis(1, at = mp, labels = fatalitiesOrdered$EVTYPE[1:20], las = 3, cex.axis = 0.4)
# 
# mp <-barplot(injuriesOrdered$injuries[1:20],xaxt='n')
# axis(1, at = mp, labels = injuriesOrdered$EVTYPE[1:20], las = 3, cex.axis = 0.4)
# 
# ggplot(fatalitiesOrdered, aes(EVTYPE)) +
#         geom_bar()
