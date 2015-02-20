library(dplyr)
library(ggplot2)
stormData <- read.csv("./data/repdata-data-StormData.csv")

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










qplot(x=EVTYPE, y=fatalities, data= fatalitiesOrderedFiltered, geom="bar")


barplot(fatalitiesOrdered$fatalities[1:20])

barplot(xtabs(~fatalitiesOrdered$fatalities[1:5]))

eventFatalities <- summarize(group_by(stormData, EVTYPE), 
                                     fatalities = sum(FATALITIES, na.rm=TRUE))

fatalitiesOrderedTest <- arrange(eventFatalities, desc(fatalities))

mp <-barplot(fatalitiesOrdered$fatalities[1:20],xaxt='n')
axis(1, at = mp, labels = fatalitiesOrdered$EVTYPE[1:20], las = 3, cex.axis = 0.4)

mp <-barplot(injuriesOrdered$injuries[1:20],xaxt='n')
axis(1, at = mp, labels = injuriesOrdered$EVTYPE[1:20], las = 3, cex.axis = 0.4)

ggplot(fatalitiesOrdered, aes(EVTYPE)) +
        geom_bar()
