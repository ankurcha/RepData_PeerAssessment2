require(ggplot2)
require(reshape2)
require(scales)
#cleanStormData <- stormData
evtypeTotals <- aggregate( cbind(PROPDMGDOLLAR,CROPDMGDOLLAR) ~ EVTYPE, data=cleanStormData, FUN=sum)
propdmg<-evtypeTotals[order(evtypeTotals$PROPDMGDOLLAR, decreasing=TRUE),]
damage<-melt(propdmg, id.vars = c("EVTYPE"),measure.vars = c("PROPDMGDOLLAR","CROPDMGDOLLAR") )
p = ggplot(data=damage, aes(x=EVTYPE, y=value, fill=variable)) +
   #facet_grid(. ~ variable) +
    geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = .5)) +
<<<<<<< HEAD
    scale_y_continuous(labels=dollar)

=======
    scale_y_continuous()
>>>>>>> bdab65a27936cac1f6e318cd4ce4afd9b1dd309a

print(p)
