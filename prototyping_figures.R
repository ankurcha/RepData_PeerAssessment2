require(ggplot2)
require(reshape2)
require(scales)
#cleanStormData <- stormData
#Question 1: Across the United States, which types of events are most harmful with respect to population health
# totalsByEventType<- aggregate( cbind(FATALITIES,INJURIES) ~ EVTYPE, data=stormData, FUN=sum)
# discreteEventInjuries<-melt(totalsByEventType, id.vars = c("EVTYPE"),measure.vars = c("FATALITIES","INJURIES") )
# p = ggplot(data=discreteEventInjuries, aes(x=reorder(EVTYPE,value), y=value,fill=variable)) +
#    scale_fill_discrete(name="Type",
#                        breaks=c("FATALITIES","INJURIES"),
#                        labels=c("Fatalities","Injuries")) +
#    geom_bar(stat="identity") +
#    theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = .5)) +
#    scale_y_sqrt(labels=comma ) +
#    xlab("Event Type") +
#    ylab("Fatalities + Injuries") +
#    ggtitle("Fatalities and Injuries by Storm Event Type")

#Question 2: Across the United States, which types of events have the greatest economic consequences
totalsByEventType<- aggregate( cbind(PROPDMGDOLLAR,CROPDMGDOLLAR) ~ EVTYPE, data=stormData, FUN=sum)
discreteEventDamage<-melt(totalsByEventType, id.vars = c("EVTYPE"),measure.vars = c("PROPDMGDOLLAR","CROPDMGDOLLAR") )
p = ggplot(data=discreteEventDamage, aes(x=reorder(EVTYPE,value), y=value,fill=variable)) +
   scale_fill_discrete(name="Type",
                       breaks=c("PROPDMGDOLLAR","CROPDMGDOLLAR"),
                       labels=c("Property Damage","Crop Damage")) +
   geom_bar(stat="identity") +
   theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = .5)) +
   scale_y_sqrt(breaks=c(1000000000,5000000000,10000000000,50000000000,100000000000,200000000000),labels=dollar ) +
   xlab("Event Type") +
   ylab("Damage in Dollars") +
   ggtitle("Damage by Event Type")


print(p)
