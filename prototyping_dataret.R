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

