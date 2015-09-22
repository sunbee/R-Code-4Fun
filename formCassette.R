setwd("C:/Users/ssbhat3/Desktop/R-Code-4Fun")
library(data.table)

dataStore <- "CassetteInput.csv"
theData <- readLines(dataStore)

# Process df 
completeRecords <- grep("^pMON", theData)
dfset <- do.call(rbind.data.frame, strsplit(theData[completeRecords], ","))
dfset[] <- lapply(dfset, as.character)
names(dfset) <- c("Construct", "ElementName", "ElementType", 
                  "ElementDescription", "ElementFunction", 
                  "StartCoordinate", "EndCoordinate", "Strand")

# Long to wide
df <- reshape(dfset, 
              timevar = "ElementFunction", 
              idvar = c("Construct"),
              direction="wide")
dfx <- df[c("Construct", 
     "ElementDescription.Enhancer", 
     "ElementDescription.Promoter",
     "ElementDescription.Leader",
     "ElementDescription.Intron",
     "ElementDescription.Gene of Interest",
     "ElementDescription.Targeting Sequence",
     "ElementDescription.Transcription termination sequence")]


# Set cassette

# Use Data Frame ?
df <- as.data.frame(theData, stringsAsFactors = FALSE)
DT <- as.data.table(df)

# From DT extract cassettes
completeRecords <- DT[, grep("^pMON", theData)]
DT[completeRecords, .(unlist(strsplit(theData, ",")))]
DT[completeRecords, .(sapply(strsplit(theData, ","), '[[', 1))]
DT[completeRecords, .(sapply(strsplit(theData, ","), '[[', 2))]
DT[completeRecords, .(sapply(strsplit(theData, ","), '[[', 3))]
dt <- DT[completeRecords, c("V1", "V2") := list(
  sapply(strsplit(theData, ","), '[[', 1),
  sapply(strsplit(theData, ","), '[[', 2))]
DT[completeRecords, list(
  sapply(strsplit(theData, ","), '[[', 1),
  sapply(strsplit(theData, ","), '[[', 2),
  sapply(strsplit(theData, ","), '[[', 3),
  sapply(strsplit(theData, ","), '[[', 4))]
DT[completeRecords, LETTERS[1:8] :=  do.call(rbind.data.frame, strsplit(theData, ","))][]

# From DT remove noise

# Sandbox
grep("^pMON", theData[4]) 
Wurdz <- strsplit(theData[4:9], ",")
Wurdz <- strsplit(theData[4:9], ",")[[1]]
Wurdz <- strsplit(theData[4:9], ",")
