setwd("C:/Users/ssbhat3/Desktop/R-Code-4Fun")
library(data.table)

dataStore <- "CassetteInput.csv"
theData <- readLines(dataStore)

# Process df 
# 1. Rows that start with pMON are the one's to grab.
# 2. Split on commas, this gives a list of vectors size 8
# 3. Convert list to table, with correct type 
# 4. Set header
completeRecords <- grep("^pMON", theData)
dfset <- do.call(rbind.data.frame, strsplit(theData[completeRecords], ","))
dfset[] <- lapply(dfset, as.character)
names(dfset) <- c("Construct", "ElementName", "ElementType", 
                  "ElementDescription", "ElementFunction", 
                  "StartCoordinate", "EndCoordinate", "Strand")

# Long to wide
# The data in native form are long. That is, several rows per pMON (i.e construct).
# Convert to wide. Note the diversity of elements within a pMON (i.e construct).
# Select only the elements of interest. 
#     Do: grep("ElementDescripton", names(df))
#     Do: grep("ElementType, names(df))
# LEFT ON TABLE:
# 1. There may be multiple elements of the same type in one pMON (i.e. construct)
# 2. Replace NA with blanks in final output
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
names(dfx) <- c("Construct", "Enhancer", "Promoter", "Leader", "Intron", 
                "Gene of Interest", "Targeting Sequence", "Termination Sequence")
  

# Write to file
write.csv(dfx, "CassetteOutput.csv", na="")


# SANDBOX 
# RUN AT YOUR OWN RISK BELOW THIS POINT

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
