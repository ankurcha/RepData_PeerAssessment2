require(data.table)
dataFileExt <-  "/home/pengler/ReproducableResearch/RepData_PeerAssessment2/data/repdata.data.StormData.csv"
# The data set has 37 columns,
importColumns <- rep("NULL",37)
# Popoluate the colClasses vectors will NULLs (will import nothing)
# we are interested in the following columns
importColumns[8] =  "character" #EVTYPE
importColumns[23] = "numeric" #FATALITIES
importColumns[24] = "numeric" #INJURIES
importColumns[25] = "numeric" #PROPDMG
importColumns[26] = "factor" #PROPDMGEXP
importColumns[27] = "numeric" #CROPDMG
importColumns[28] = "factor" #CROPDMGEXP

DF<-read.csv (dataFileExt, nrows=-1,colClasses = importColumns)
