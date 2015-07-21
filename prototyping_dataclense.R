# Clean up our data

# Make a temp value for testing
stormData <- stormDataOrig
origRows <-nrow(stormData)

# We are only interested in data where there was: Property/Crop Damage, 
# Injuries or Fatalities. If all of these data points are 0 for a row we
# can discard it. 

stormData <- stormData[(stormData$PROPDMG > 0 | 
                        stormData$CROPDMG > 0 |
                        stormData$FATALITIES >0 |
                        stormData$INJURIES >0) , ]

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

# #Rename factors that seem to be miscatagorized - "OTHER" will be the catch all catagory
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="?"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Other"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Marine Accident"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MARINE MISHAP"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="NORTHERN LIGHTS"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="APACHE COUNTY"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Temperature record"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="DROWNING"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="NONE"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Metro Storm, May 26"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="RED FLAG CRITERIA"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="No Severe Weather"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="SOUTHEAST"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="RAPIDLY RISING WATER"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="WALL CLOUD"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="ROTATING WALL CLOUD"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LARGE WALL CLOUD"] <- "OTHER"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="REMNANTS OF FLOYD"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY MIX"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="SEVERE TURBULENCE"] <- "HEAVY RAIN"

# 
# 
# 
# 
# # Group EVTYPES to align with doc
# # Due to overlapping in descriptions the ordering the grouping is done in is fairly sensitive 
# 
# 
# #TStorm Surge
levels(stormData$EVTYPE)[grepl("SURGE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "STORM SURGE"
# 
# #Tide
levels(stormData$EVTYPE)[grepl("TIDE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ASTRONOMICAL HIGH TIDE"
# 
# #Avalanche
levels(stormData$EVTYPE)[grepl("AVALA",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "AVALANCHE"
# 
# #Blizzard
levels(stormData$EVTYPE)[grepl("BLIZZ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "BLIZZARD"
# 
# #Rip Current
levels(stormData$EVTYPE)[grepl("RIP",levels(stormData$EVTYPE),ignore.case = TRUE)]  <- "RIP CURRENT" 
# 
# #High Surf
levels(stormData$EVTYPE)[grepl("SURF",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HIGH SURF"
# 
# #Costal Flooding
levels(stormData$EVTYPE)[grepl("COAST",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "COASTALFLUD" #temp
# 
# #Debris Flow
levels(stormData$EVTYPE)[grepl("SLIDE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DEBRIS FLOW" 
levels(stormData$EVTYPE)[grepl("SLUMP",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DEBRIS FLOW" 
# 
# #Extreme
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXTREME HEAT"] <- "EXH1"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE HEAT"] <- "EXH1"
levels(stormData$EVTYPE)[grepl("EXTREME ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ECWC1" #temp
# 
# #Frost/Freezing
levels(stormData$EVTYPE)[grepl("FROST",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FROST/FREEZE"
levels(stormData$EVTYPE)[grepl("FREEZ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FROST/FREEZE"
# 
# 
# 
# #Fog
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FOG AND COLD TEMPERATURES"] <-  "DENSE FOG"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FOG"] <-  "DENSE FOG"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="VOG"] <-  "DENSE FOG" #typo perhaps? 
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="PATCHY DENSE FOG"] <-  "DENSE FOG"
levels(stormData$EVTYPE)[grepl("FREEZING FOG",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FREEZING FOG"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Ice Fog"] <-  "FREEZING FOG"
levels(stormData$EVTYPE)[grepl("GLAZE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FREEZING FOG"
# 
# 
# #Dust
levels(stormData$EVTYPE)[grepl("SMOKE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DENSE SMOKE"
levels(stormData$EVTYPE)[grepl("DROUGHT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DROUGHT"
levels(stormData$EVTYPE)[grepl("MICROBURST",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TWNID" #temp
levels(stormData$EVTYPE)[grepl("DRY",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DROUGHT"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="BELOW NORMAL PRECIPITATION"] <-  "DROUGHT"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="DRIEST MONTH"] <-  "DROUGHT"
levels(stormData$EVTYPE)[grepl("DUST D",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DUST DEVIL"
levels(stormData$EVTYPE)[grepl("DUST S",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DUST STORM"
# levels(stormData$EVTYPE)[grepl("SAHA",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"DUST STORM"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="DUSTSTORM"] <-  "DUST STORM"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="BLOWING DUST"] <-  "DUST STORM"
# 
# #Excessive/Heavy
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="Excessive Cold"] <- "CWC1"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVELY DRY"] <- "DROUGHT"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE PRECIPITATION"] <- "HEAVY RAIN"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE RAIN"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE RAINFALL"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE SNOW"] <- "HEAVY SNOW"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE WETNESS"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY SHOWER"] <- "HEAVY RAIN"


# levels(stormData$EVTYPE)[grepl("FLASH",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FLASHFLUD" #temp
# 
# #Lakes and floods
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LAKE FLOOD"] <- "LAKEFLUD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LAKESHORE FLOOD"] <- "LAKEFLUD"
levels(stormData$EVTYPE)[grepl("FLOOD",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FLOOD"
levels(stormData$EVTYPE)[grepl("URBAN",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FLOOD"
levels(stormData$EVTYPE)[grepl("STREAM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FLOOD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FLASHFLUD"] <- "FLASH FLOOD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LAKEFLUD"] <- "LAKESHORE FLOODING"
levels(stormData$EVTYPE)[grepl("COASTALFLUD",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "COASTAL FLOOD"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="GUSTY LAKE WIND"] <- "HIGH WIND"
levels(stormData$EVTYPE)[grepl("LAKE ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "LESW" #temp
levels(stormData$EVTYPE)[grepl("DAM ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FLASH FLOOD"
# 
# #WaterSpout
levels(stormData$EVTYPE)[grepl("WATERSPOUT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "WATERSPOUT"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="WATER SPOUT"] <- "WATERSPOUT"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="WAYTERSPOUT"] <- "WATERSPOUT"
# 
# #Funnel Clouds 
levels(stormData$EVTYPE)[grepl("FUNNEL",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FUNNEL CLOUD"
# 
# #Hail (to the king baby)
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MARINE HAIL"] <- "MHIAL"
levels(stormData$EVTYPE)[grepl("HAIL",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HAIL"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MHIAL"] <- "MARINE HAIL"
# 
# #Heat
levels(stormData$EVTYPE)[grepl("HEAT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HEAT"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH"] <- "HEAT"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH TEMPERATURE RECORD"] <- "HEAT"
levels(stormData$EVTYPE)[grepl("WARM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HEAT"
# levels(stormData$EVTYPE)[grepl("HOT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HEAT"
levels(stormData$EVTYPE)[grepl("HYPERTHERMIA/EXPOSURE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HEAT"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXH1"] <- "EXCESSIVE HEAT"
# 
# #Ice Storm
levels(stormData$EVTYPE)[grepl("ICE STORM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ICE STORM"
levels(stormData$EVTYPE)[grepl("ICY",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ICE STORM"
# 
# #Sleet
levels(stormData$EVTYPE)[grepl("SLEET",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "SLEET"
# 
# #Cold/Wind Chill
levels(stormData$EVTYPE)[grepl("COLD",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("CHILL",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("COOL",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("LOW TEMP",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("HYPOTHERMIA",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
# 
# #Heavy Snow
levels(stormData$EVTYPE)[grepl("SNOW",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY SNOW"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LESW"] <-  "LAKE-EFFECT SNOW"
# 
# #Rain
levels(stormData$EVTYPE)[grepl("RAIN",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY RAIN"
levels(stormData$EVTYPE)[grepl("WET",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY RAIN"
levels(stormData$EVTYPE)[grepl("PRECIP",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY RAIN"
# 
# #Lightening

levels(stormData$EVTYPE)[grepl("LIGHT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "LIGHTNING"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LIGNTNING"] <-  "LIGHTNING"
# 
# #Wind
levels(stormData$EVTYPE)[grepl("MARINE STRONG",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "MSW1" #temp
levels(stormData$EVTYPE)[grepl("MARINE HIGH",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "MSW2" #temp
levels(stormData$EVTYPE)[grepl("strong",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "SWC1"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="NON-TSTM WIND"] <-  "HIGH WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="NON TSTM WIND"] <-  "HIGH WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MARINE TSTM WIND"] <-  "MTWNID" #temp
levels(stormData$EVTYPE)[grepl("THUND",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TWNID" #temp
levels(stormData$EVTYPE)[grepl("TSTM",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TWNID"
levels(stormData$EVTYPE)[grepl("WIND",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HIGH WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MTWNID"] <-  "MARINE THUNDERSTORM WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="TWNID"] <-  "THUNDERSTORM WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="CWC1"] <-  "COLD/WIND CHILL"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="ECWC1"] <-  "EXTREME COLD/WIND CHILL"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="SWC1"] <-  "STRONG WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MSW1"] <-  "MARINE STRONG WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MSW2"] <-  "MARINE HIGH WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="WND"] <-  "WIND"
levels(stormData$EVTYPE)[grepl("GUSTNADO",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"THUNDERSTORM WIND"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="DOWNBURST"] <-  "THUNDERSTORM WIND"
# 
# # High Surf
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH SWELLS"] <-  "HIGH SURF"
# levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH  SWELLS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH WATER"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH SEAS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH WAVES"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[grepl("beach",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HIGH SURF" 
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY SEAS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY SWELLS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="ROGUE WAVE"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="ROUGH SEAS"] <-  "HIGH SURF"
# 
# #Hurricane 
levels(stormData$EVTYPE)[grepl("HUR",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HURRICANE/TYPHOON"
levels(stormData$EVTYPE)[grepl("TYP",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HURRICANE/TYPHOON"
# 
# #Ice Storm
levels(stormData$EVTYPE)[grepl("ICE",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"ICE STORM"
# 
# #Tornado
levels(stormData$EVTYPE)[grepl("TORNADO",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TORNADO"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="TORNDAO"] <-  "TORNADO"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LANDSPOUT"] <-  "TORNADO"
# 
# #Topical Storm
levels(stormData$EVTYPE)[grepl("TROPICAL ST",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TROPICAL STORM"
# 
# #Volcanic Ash
# levels(stormData$EVTYPE)[grepl("volc",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "VOLCANIC ASH"
# 
# #Wildfire
levels(stormData$EVTYPE)[grepl("FIRE",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"WILDFIRE"
# 
# #Winter Weather and Winter Storm
levels(stormData$EVTYPE)[grepl("WINTER ST",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"XWNT" #temp
levels(stormData$EVTYPE)[grepl("WINT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "WINTER WEATHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="XWNT"] <-  "WINTER STORM"
# 
# 
# #levels(stormData$EVTYPE)[grepl("^TSTM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "THUNDERSTORM WIND"
# #levels(stormData$EVTYPE)[grepl("^TROPICAL STORM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "TROPICAL STORM"
# #levels(stormData$EVTYPE)[grepl("^THUNDER",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "THUNDERSTORM WIND"


stormData$EVTYPE <-factor (stormData$EVTYPE)
step2Rows<-nrow(stormData)
