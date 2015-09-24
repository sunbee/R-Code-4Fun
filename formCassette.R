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

# A single pMON (i.e. construct) may have one or more cassettes. Extract subsets.
DTset <- as.data.table(dfset)
DTcas <- DTset[ElementType %in% c("CASSETTE")]

# Take 3 pMONs - pMON269350, 269371, (307172), 272075
# For each pMON ..
#   Subset on pMON (Call this a Construct)
#   Locate cassette(s)
#   For each cassette
#     Subset on Construct
#     Cast Construct into single row using dcast.data.table
#     Append row to ouput
out <- data.frame()
myMONs <- c("pMON269350", "pMON269371", "pMON272075")
for (pMON in myMONs) { # Subset on pMON 
  myConstruct <- DTset[Construct %in% pMON] # Construct
  myCassettes <- DTset[Construct %in% pMON & ElementType %in% c("CASSETTE"), 
                     .(StartCoordinate, EndCoordinate)] # End-Points
  for (n in 1:nrow(myCassettes)) { # Process cassette
    myRow <- myConstruct[, "SELECTOR" := cut(as.numeric(StartCoordinate), 
                                 breaks=c(0, as.numeric(myCassettes[n,])-1, Inf),
                                 labels=FALSE,
                                 include.lowest=TRUE)][SELECTOR==2, ][, getFlat2(.SD)]
    print(myRow)
    out <- rbind(out, myRow, fill=TRUE)
  }
} 

getFlat2 <- function(DT) { # Cast Construct into single row 
  DX <- dcast.data.table(DT, Construct ~ ElementFunction,
                    value.var="ElementDescription",
                    fun=function(x) {paste(x, collapse="|")})
  return(DX)
}

# Take a pMON: pMON269350
my_pMON <- "pMON269350"
# Get construct and end-points for cassettes
myConstruct <- DTset[Construct %in% my_pMON] # Block for one pMON (i.e. construct)
myBracket <- DTset[Construct %in% my_pMON & ElementType %in% c("CASSETTE"), 
                   .(StartCoordinate, EndCoordinate)] # End-Points

myConstruct[, .(cut(as.numeric(StartCoordinate), breaks=as.numeric(myBracket[1,])))]
myConstruct[, "Test" := cut(as.numeric(StartCoordinate), 
                            breaks=c(0, as.numeric(myBracket[1,])-1, Inf),
                            labels=FALSE,
                            include.lowest=TRUE)]

out <- getFlat(myConstruct) # Pass a cassette extracted from construct
names(out)[grep("^ElementDes", names(out))]


# Long to wide
# The data in native form are long. That is, several rows per pMON (i.e construct).
# Convert to wide. Note the diversity of elements within a pMON (i.e construct).
# Select only the elements of interest. 
#     Do: grep("ElementDescripton", names(df))
#     Do: grep("ElementType, names(df))

# Use reshape - the old method
df <- reshape(dfset, 
              timevar = "ElementFunction", 
              idvar = c("Construct"),
              direction="wide")
# Use dcast from reshape2
# LHS ~ RHS
# LHS: Specifies the key
# RHS: The values of this variable become columns (a.k.a Pivot)
# value.var: This is the variable that is rendered in the table
# When LHS does not grab unique rows, then
# fun: Sets up the aggregation pattern
df2 <- dcast(dfset, Construct ~ ElementFunction, 
             value.var="ElementDescription",
             fun = length)
df2 <- dcast(dfset, Construct ~ ElementFunction, 
             value.var="ElementDescription",
             fun = function(x) {x[1]})             
# Fast version for data.table
df2 <- dcast.data.table(DTset, Construct ~ ElementFunction, 
             value.var="ElementDescription",
             fun = function(x) {paste(x, collapse="|")})             

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

# String manipulation
grep("^pMON", theData[4]) 
Wurdz <- strsplit(theData[4:9], ",")
Wurdz <- strsplit(theData[4:9], ",")[[1]]
Wurdz <- strsplit(theData[4:9], ",")
