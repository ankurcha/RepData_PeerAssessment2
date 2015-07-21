#require(data.table)
mydir <- getwd()
dataFileExt <- paste0(mydir,"/data/repdata.data.StormData.csv")
#dataFileExt <-  "/home/pengler/ReproducableResearch/RepData_PeerAssessment2/data/repdata.data.StormData.csv"
# The data set has 37 columns,
importColumns <- rep("NULL",37)
# Populate the colClasses vectors will NULLs (will import nothing)
# we are interested in the following columns
importColumns[2] =  "character"    #BGN_DATE
importColumns[6] =  "factor"       #COUNTYNAME
importColumns[7] =  "factor"       #STATE
importColumns[8] =  "factor"       #EVTYPE
importColumns[23] = "numeric"      #FATALITIES
importColumns[24] = "numeric"      #INJURIES
importColumns[25] = "numeric"      #PROPDMG
importColumns[26] = "factor"       #PROPDMGEXP
importColumns[27] = "numeric"      #CROPDMG
importColumns[28] = "factor"       #CROPDMGEXP

stormDataOrig<-read.csv (dataFileExt, nrows=-1,colClasses = importColumns )
