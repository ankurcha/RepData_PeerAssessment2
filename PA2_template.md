# Reproducible Research: Peer Assessment 2
Paul Engler  
July 17, 2015  

## Summary/Synopsis
- Synopsis: Immediately after the title, there should be a synopsis which describes and summarizes your analysis in at most 10 complete sentences.

## Data Processing

- There should be a section titled Data Processing which describes (in words and code) how the data were loaded into R and processed for analysis. In particular, your analysis must start from the raw CSV file containing the data. You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider using the cache = TRUE option for certain code chunks

- Set up the environment for the data analysis
- Retreive and uncompress the data file


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

- Read in the data file

```r
# The data should all be in an uncompress format on disk
# we can now read in the data using read.csv and
# defining our colClasses to minimize our in-memory footprint 

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

stormData<-read.csv (dataFileExt, nrows=-1,colClasses = importColumns )
```

- Clean the data

```r
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

#re-level our event types factor
stormData$EVTYPE <-factor (stormData$EVTYPE)
step2Rows<-nrow(stormData)

sprintf("Data clensing has remove %d of %d rows, a %f percent reduction in the data set", 
        origRows-step2Rows,origRows,((origRows-step2Rows)/origRows)*100)
```

```
## [1] "Data clensing has remove 872 of 902297 rows, a 0.096642 percent reduction in the data set"
```

## Results
- The analysis document must have at least one figure containing a plot.
- Your analyis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but there cannot be more than three figures total
