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

#Rename factors that seem to be miscatagorized - "OTHER" will be the catch all catagory
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="?"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Marine Accident"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="?"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="?"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE"] <- "OTHER"

# Group EVTYPES to align with doc
# Due to overlapping in descriptions the ordering the grouping is done in is fairly sensitive 
levels(stormData$EVTYPE)[grepl("TIDE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ASTRONOMICAL HIGH TIDE"
levels(stormData$EVTYPE)[grepl("AVALA",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "AVALANCHE"
levels(stormData$EVTYPE)[grepl("BLIZZ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "BLIZZARD"
levels(stormData$EVTYPE)[grepl("RIP",levels(stormData$EVTYPE),ignore.case = TRUE)]  <- "RIP CURRENT" 
levels(stormData$EVTYPE)[grepl("SURF",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HIGH SURF"
levels(stormData$EVTYPE)[grepl("COAST",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "COASTAL FLOOD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXTREME HEAT"] <- "HEAT"
levels(stormData$EVTYPE)[grepl("EXTREME ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "EXTREME COLD/WIND CHILL"
levels(stormData$EVTYPE)[grepl("FROST",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FROST/FREEZE"
levels(stormData$EVTYPE)[grepl("FREEZE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FROST/FREEZE"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MARINE TSTM WIND"] <-  "MARINE THUNDERSTORM WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FOG AND COLD TEMPERATURES"] <-  "DENSE FOG"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FOG"] <-  "DENSE FOG"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="PATCHY DENSE FOG"] <-  "DENSE FOG"
levels(stormData$EVTYPE)[grepl("FREEZING FOG",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FREEZING FOG"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Ice Fog"] <-  "FREEZING FOG"
levels(stormData$EVTYPE)[grepl("SMOKE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DENSE SMOKE"
levels(stormData$EVTYPE)[grepl("DROUGHT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DROUGHT"
levels(stormData$EVTYPE)[grepl("DUST D",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DUST DEVIL"
levels(stormData$EVTYPE)[grepl("DUST S",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DUST STORM"
levels(stormData$EVTYPE)[grepl("SAHA",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"DUST STORM"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="DUSTSTORM"] <-  "DUST STORM"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="BLOWING DUST"] <-  "DUST STORM"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Excessive Cold"] <- "COLD/WIND CHILL"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVELY DRY"] <- "DROUGHT"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE PRECIPITATION"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE RAIN"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE RAINFALL"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE SNOW"] <- "HEAVY SNOW"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE WETNESS"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[grepl("FLASH",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FLASHFLUD" #temp
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LAKE FLOOD"] <- "LAKEFLUD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LAKESHORE FLOOD"] <- "LAKEFLUD"
levels(stormData$EVTYPE)[grepl("FLOOD",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FLOOD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FLASHFLUD"] <- "FLASH FLOOD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LAKEFLUD"] <- "LAKESHORE FLOODING"



#levels(stormData$EVTYPE)[grepl("^TSTM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "THUNDERSTORM WIND"
#levels(stormData$EVTYPE)[grepl("^TROPICAL STORM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "TROPICAL STORM"
#levels(stormData$EVTYPE)[grepl("^THUNDER",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "THUNDERSTORM WIND"


stormData$EVTYPE <-factor (stormData$EVTYPE)
step2Rows<-nrow(stormData)
