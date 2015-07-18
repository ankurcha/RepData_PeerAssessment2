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
importColumns[8] =  "character" #EVTYPE
importColumns[23] = "numeric"   #FATALITIES
importColumns[24] = "numeric"   #INJURIES
importColumns[25] = "numeric"   #PROPDMG
importColumns[26] = "factor"    #PROPDMGEXP
importColumns[27] = "numeric"   #CROPDMG
importColumns[28] = "factor"    #CROPDMGEXP

stormData<-read.csv (dataFileExt, nrows=-1,colClasses = importColumns)
```
## Results
- The analysis document must have at least one figure containing a plot.
- Your analyis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but there cannot be more than three figures total
