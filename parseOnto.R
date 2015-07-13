setwd('C:\\Users\\ssbhat3\\Desktop\\R Code4Fun')

on <- read.csv("OntoMadness.csv", stringsAsFactors=FALSE)
dim(on)
v <- list();
for (i in 1:dim(on)[1]) {
  a <- on[i,]
  filter_NA <- !is.na(a) 
  filter_ws <- a != ""
  filter_le <- sapply(a, function(x) !grepl("lev", x))
  filter_all <- filter_NA & filter_ws & filter_le
  newTerm <- a[, which(filter_all)]
  print(newTerm)
  if (newTerm %in% names(v)) {
    ct <- v[[newTerm]]
    ct <- c(ct, length(ct))
    v[[newTerm]] <- ct
  } else {
    v[[newTerm]] <- c(0)
  }
}

u <- sapply(v, function(x) length(x))
