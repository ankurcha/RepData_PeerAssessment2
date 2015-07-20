# Clean up our data

# Make a temp value for testing
stormData <- stormDataOrig
origRows <-nrow(stormData)
#Convert our date's to a POSIX date 
stormData$BGN_DATE <-as.POSIXct( stormData$BGN_DATE , format = "%m/%d/%Y %H:%M:%S")

# Convert the property damage to actual dollars 
# Estimates should be rounded to three significant digits, followed by an 
# alphabetical character signifying the magnitude of the number, i.e., 1.55B 
# for $1,550,000,000. Alphabetical characters used to signify magnitude
# include “K” for thousands, “M” for millions, and “B” for billions

# Covert the PROPDMGEXP to upper case for consistency
stormData$PROPDMGEXP<- as.factor(toupper(stormData$PROPDMGEXP))
stormData$CROPDMGEXP<- as.factor(toupper(stormData$CROPDMGEXP))
# Filter out rows with bad values
stormData <- stormData[stormData$PROPDMGEXP %in% c("","K","M","B"),]
stormData <- stormData[stormData$CROPDMGEXP %in% c("","K","M","B"),]
#re-level the factor now that rows have been removed
stormData$PROPDMGEXP <-factor(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <-factor(stormData$CROPDMGEXP)
step1Rows <- nrow(stormData)

# There are certain cases where the EXP value was not included but a
# DMG value was associated. Impute these to "K" values, this may lead to 
# under reporting damage estimates
stormData[(stormData$PROPDMG > 0 & stormData$PROPDMGEXP ==""),]$PROPDMGEXP <-"K"
stormData[(stormData$CROPDMG > 0 & stormData$CROPDMGEXP ==""),]$CROPDMGEXP <-"K"

# Add columns for the real $ value based on the *DMG and *DMGEXP columns
expandDollars <- function (dmg,dmgexp) { 
   if (dmg == 0 ) { return(0) }
   else if (dmgexp =="K") {return(dmg*1000)}
   else if (dmgexp =="M") {return(dmg*1000000)}
   else if (dmgexp =="B") {return(dmg*1000000000)}
   }
stormData$PROPDMGDOLLAR<-mapply(expandDollars,stormData$PROPDMG,stormData$PROPDMGEXP)
stormData$CROPDMGDOLLAR<-mapply(expandDollars,stormData$CROPDMG,stormData$CROPDMGEXP)

# Remove data that does not seem to align to a known EVTYPE
# This is bases of off the Events Type list in section 2.1.1 (page 6)
# of the "NATIONAL WEATHER SERVICE INSTRUCTION 10-1605" document

# Remove "Summary","Record","Monthly" rows, these seem to be summarizing data
# and could be leading to double counting 
stormData <- stormData[!grepl("^Summary",stormData$EVTYPE,ignore.case = TRUE),]
stormData <- stormData[!grepl("^Record",stormData$EVTYPE,ignore.case = TRUE),]
stormData <- stormData[!grepl("^Month",stormData$EVTYPE,ignore.case = TRUE),]

stormData$EVTYPE <-factor (stormData$EVTYPE)
step2Rows<-nrow(stormData)

