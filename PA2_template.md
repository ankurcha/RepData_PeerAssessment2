# Impact of Storm Events on Health and the Economy
Paul  
July 23, 2015  

## Summary/Synopsis

This report attempts to summarize the information within the NOAA Storm Database
to answer two basic questions. 

First, across the United States, which types of 
events are most harmful with respect to population health. To answer this question 
the data set has been analyzed to look for any events that lead to injuries or 
fatalities. Based on this information it appears that **Tornados** have the 
greatest impact to public health. 


Second, Across the United States, which types of events have the greatest 
economic consequences? To answer this question the data set has been analyzed 
to look for any events that lead to property of crop damage. Based on this 
information it appears that **Floods** have the greatest economic impact. 

## Data Processing

### Data Retreival 

All the data for this analysis is taken from the NOAA Storm Database located 
at: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2


```r
dataURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dataFile <-"repdata.data.StormData.csv.bz2"
baseDir<-getwd()
dataDir<-paste(baseDir,"data",sep="/")
dataFile<-paste(dataDir,dataFile,sep="/")
dataFileExt <- "repdata.data.StormData.csv"
dataFileExt  <- paste(dataDir,dataFileExt,sep="/")

# Only download the and unpack the file if it does not exist
# This should be a one time performance penalty on the first execution
# and will leave the uncompress data on disk so that we can be more
# efficient in reading the data later on. 
if (!file.exists(dataFile)) { 
   dir.create(dataDir,showWarnings = FALSE)
   download.file(url=dataURL,destfile=dataFile,method="curl")
}   

# Check if we have the uncompressed file
if (!file.exists(dataFileExt)) {
   # Assumes we are on a Linux system with bunzip2 in the path
   # uncompress the file and leave the compressed file in place
   system(paste("bunzip2 -k", dataFile))
}
```

### Data Loading
Once the data file is pulled down, read in the data. The original file contains
37 rows of data, most of which are not germane to the analysis. To limit the memory
footprint the import is limited to the following fields: 

- EVTYPE - Event Type
- FATALITIES - Fatalities occurring from storm events
- INJURIES - Injuries occurring from storm events
- PROPDMG - Property damage from storm events
- PRODDMGEXP - A scale factor for property damage (K=Thousands, M=Millions, B=Billions)
- CROPDMG - Crop damage from storm events
- CROPDMGEXP - A scale factor for crop damage (K=Thousands, M=Millions, B=Billions)
   

```r
# The data should all be in an uncompress format on disk
# we can now read in the data using read.csv and
# defining our colClasses to minimize our in-memory footprint 

# The data set has 37 columns,
importColumns <- rep("NULL",37)

# Populate the colClasses vectors will NULLs (will import nothing)
# we are interested in the following columns
#importColumns[2] =  "character"    #BGN_DATE
importColumns[8] =  "factor"       #EVTYPE
importColumns[23] = "numeric"      #FATALITIES
importColumns[24] = "numeric"      #INJURIES
importColumns[25] = "numeric"      #PROPDMG
importColumns[26] = "factor"       #PROPDMGEXP
importColumns[27] = "numeric"      #CROPDMG
importColumns[28] = "factor"       #CROPDMGEXP

stormData<-read.csv (dataFileExt, nrows=-1,colClasses = importColumns )
```

### Tidying Data

In order to perform the analysis the following steps were taken to manipulate the source data:

1. All Rows that do not contain a value for PROPDMG,CROPDMG,FATALITIES, or INJURIES were removed
2. Row's with bad EXP values (valid are "", K,M,B) for crop and property damage were removed
3. If there are rows where property damage or crop damage are above 0 but there is no scale value(EXP), the EXP value is imputed to be "K". This could lead to under reported damage estimates
4. Two new rows are imputed for the crop and property damage based on the value and the EXP. So for example a value of 5 with and EXP of M would be imputed in a new row to equal 5,000,000. 
5. Rows that seem to contain summary data that may lead to double counting were removed. These include rows where the EVTYPE contained "Summary", "Record", or "Month"
6. A new EVTYPE call "OTHER" was added to allow for collation of events that do not seem to align to any of the EVTYPEs as defined on Page 6 of the [NATIONAL WEATHER SERVICE INSTRUCTION 10-1605](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
7. The remainder of the EVTYPE were aligned to the values contained within the aforementioned document, based on the descriptions of each EVTYPE. 


```r
# We are only interested in data where there was: Property/Crop Damage, 
# Injuries or Fatalities. If all of these data points are 0 for a row we
# can discard it. 

stormData <- stormData[(stormData$PROPDMG > 0 | 
                        stormData$CROPDMG > 0 |
                        stormData$FATALITIES >0 |
                        stormData$INJURIES >0) , ]

#Convert our date's to a POSIX date 
#stormData$BGN_DATE <-as.POSIXct( stormData$BGN_DATE , format = "%m/%d/%Y %H:%M:%S")

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
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="APACHE COUNTY"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="DROWNING"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="RAPIDLY RISING WATER"] <- "OTHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY MIX"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="SEVERE TURBULENCE"] <- "HEAVY RAIN"

#Group EVTYPES to align with doc
# Due to overlapping in descriptions the ordering the grouping is done in is fairly sensitive 
 
#TStorm Surge
levels(stormData$EVTYPE)[grepl("SURGE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "STORM SURGE"
 
# #Tide
levels(stormData$EVTYPE)[grepl("TIDE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ASTRONOMICAL HIGH TIDE"
 
#Avalanche
levels(stormData$EVTYPE)[grepl("AVALA",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "AVALANCHE"

#Blizzard
levels(stormData$EVTYPE)[grepl("BLIZZ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "BLIZZARD"

#Rip Current
levels(stormData$EVTYPE)[grepl("RIP",levels(stormData$EVTYPE),ignore.case = TRUE)]  <- "RIP CURRENT" 

#High Surf
levels(stormData$EVTYPE)[grepl("SURF",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HIGH SURF"

#Costal Flooding
levels(stormData$EVTYPE)[grepl("COAST",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "COASTALFLUD" #temp

#Debris Flow
levels(stormData$EVTYPE)[grepl("SLIDE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DEBRIS FLOW" 
levels(stormData$EVTYPE)[grepl("SLUMP",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DEBRIS FLOW" 
 
#Extreme
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXTREME HEAT"] <- "EXH1"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE HEAT"] <- "EXH1"
levels(stormData$EVTYPE)[grepl("EXTREME ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ECWC1" #temp

#Frost/Freezing
levels(stormData$EVTYPE)[grepl("FROST",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FROST/FREEZE"
levels(stormData$EVTYPE)[grepl("FREEZ",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FROST/FREEZE"

#Fog
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FOG AND COLD TEMPERATURES"] <-  "DENSE FOG"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="FOG"] <-  "DENSE FOG"
levels(stormData$EVTYPE)[grepl("FREEZING FOG",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FREEZING FOG"
levels(stormData$EVTYPE)[grepl("GLAZE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FREEZING FOG"

#Dust
levels(stormData$EVTYPE)[grepl("SMOKE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DENSE SMOKE"
levels(stormData$EVTYPE)[grepl("DROUGHT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DROUGHT"
levels(stormData$EVTYPE)[grepl("MICROBURST",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TWNID" #temp
levels(stormData$EVTYPE)[grepl("DRY",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DROUGHT"
levels(stormData$EVTYPE)[grepl("DUST D",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DUST DEVIL"
levels(stormData$EVTYPE)[grepl("DUST S",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "DUST STORM"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="BLOWING DUST"] <-  "DUST STORM"

#Excessive/Heavy
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE RAINFALL"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE SNOW"] <- "HEAVY SNOW"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXCESSIVE WETNESS"] <- "HEAVY RAIN"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY SHOWER"] <- "HEAVY RAIN"

#Lakes and floods
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

#WaterSpout
levels(stormData$EVTYPE)[grepl("WATERSPOUT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "WATERSPOUT"

#Funnel Clouds 
levels(stormData$EVTYPE)[grepl("FUNNEL",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "FUNNEL CLOUD"

#Hail (to the king baby)
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MARINE HAIL"] <- "MHIAL"
levels(stormData$EVTYPE)[grepl("HAIL",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HAIL"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="MHIAL"] <- "MARINE HAIL"

#Heat
levels(stormData$EVTYPE)[grepl("HEAT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HEAT"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH"] <- "HEAT"
levels(stormData$EVTYPE)[grepl("WARM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HEAT"
levels(stormData$EVTYPE)[grepl("HYPERTHERMIA/EXPOSURE",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HEAT"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="EXH1"] <- "EXCESSIVE HEAT"

#Ice Storm
levels(stormData$EVTYPE)[grepl("ICE STORM",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ICE STORM"
levels(stormData$EVTYPE)[grepl("ICY",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "ICE STORM"

#Sleet
levels(stormData$EVTYPE)[grepl("SLEET",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "SLEET"

#Cold/Wind Chill
levels(stormData$EVTYPE)[grepl("COLD",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("CHILL",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("COOL",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("LOW TEMP",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp
levels(stormData$EVTYPE)[grepl("HYPOTHERMIA",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"CWC1" #temp

#Heavy Snow
levels(stormData$EVTYPE)[grepl("SNOW",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY SNOW"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LESW"] <-  "LAKE-EFFECT SNOW"

#Rain
levels(stormData$EVTYPE)[grepl("RAIN",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY RAIN"
levels(stormData$EVTYPE)[grepl("WET",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY RAIN"
levels(stormData$EVTYPE)[grepl("PRECIP",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HEAVY RAIN"

#Lightening
levels(stormData$EVTYPE)[grepl("LIGHT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "LIGHTNING"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LIGNTNING"] <-  "LIGHTNING"

#Wind
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

# High Surf
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH SWELLS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH WATER"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH SEAS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HIGH WAVES"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[grepl("beach",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "HIGH SURF" 
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY SEAS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="HEAVY SWELLS"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="ROGUE WAVE"] <-  "HIGH SURF"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="ROUGH SEAS"] <-  "HIGH SURF"

#Hurricane 
levels(stormData$EVTYPE)[grepl("HUR",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HURRICANE/TYPHOON"
levels(stormData$EVTYPE)[grepl("TYP",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"HURRICANE/TYPHOON"

#Ice Storm
levels(stormData$EVTYPE)[grepl("ICE",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"ICE STORM"

#Tornado
levels(stormData$EVTYPE)[grepl("TORNADO",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TORNADO"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="TORNDAO"] <-  "TORNADO"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="LANDSPOUT"] <-  "TORNADO"

#Topical Storm
levels(stormData$EVTYPE)[grepl("TROPICAL ST",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"TROPICAL STORM"

#Wildfire
levels(stormData$EVTYPE)[grepl("FIRE",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"WILDFIRE"

#Winter Weather and Winter Storm
levels(stormData$EVTYPE)[grepl("WINTER ST",levels(stormData$EVTYPE),ignore.case = TRUE)] <-"XWNT" #temp
levels(stormData$EVTYPE)[grepl("WINT",levels(stormData$EVTYPE),ignore.case = TRUE)] <- "WINTER WEATHER"
levels(stormData$EVTYPE)[levels(stormData$EVTYPE)=="XWNT"] <-  "WINTER STORM"

#stormData$EVTYPE <-factor (stormData$EVTYPE)
```

## Results

### Question 1: Across the United States, which types of events are most harmful with respect to population health

```r
require(ggplot2)
require(reshape2)
require(scales)

#Question 1: Across the United States, which types of events are most harmful with respect to population health
totalsByEventType<- aggregate( cbind(FATALITIES,INJURIES) ~ EVTYPE, data=stormData, FUN=sum)
discreteEventInjuries<-melt(totalsByEventType, id.vars = c("EVTYPE"),measure.vars = c("FATALITIES","INJURIES") )
p = ggplot(data=discreteEventInjuries, aes(x=reorder(EVTYPE,value), y=value,fill=variable)) +
   scale_fill_discrete(name="Type",
                       breaks=c("FATALITIES","INJURIES"),
                       labels=c("Fatalities","Injuries")) +
   geom_bar(stat="identity") +
   theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = .5)) +
   scale_y_sqrt(labels=comma ) +
   xlab("Event Type") +
   ylab("Fatalities + Injuries") +
   ggtitle("Fatalities and Injuries by Storm Event Type")

print (p)
```

![](PA2_template_files/figure-html/Question_One-1.png) 
**FIGURE 1:** A bar plot showing the event types that are most harmful with respect to
population health. The plot is organized in ascending order (left -> right) of least
to most harmful event types. The cumulative attributed injuries and fatalities are 
stacked to provide a combined impact to population health. 


### Question 2: Across the United States, which types of events have the greatest economic consequences

```r
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
```

![](PA2_template_files/figure-html/Question_two-1.png) 
**FIGURE 2:** A bar plot showing the event types that have the greatest economic 
impact. The plot is organized in ascending order (left -> right) of least
to most economically impactful event types. The cumulative property and crop
damage costs are stacked to provide a combined economic impact. 
