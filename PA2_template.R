library(dplyr)
library(ggplot2)
stormData <- read.csv("./data/repdata-data-StormData.csv")

eventFatalitiesInjuries <- summarize(group_by(stormData, EVTYPE), 
                                     fatalities = sum(FATALITIES, na.rm=TRUE),
                                     injuries = sum(INJURIES, na.rm =TRUE))

fatalitiesOrdered <- arrange(eventFatalitiesInjuries, desc(fatalities))

qplot(EVTYPE, fatalities, data= fatalitiesOrdered)

injuriesOrdered <- arrange(eventFatalitiesInjuries, desc(injuries))

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
