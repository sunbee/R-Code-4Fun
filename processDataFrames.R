# Create data
createData <- function() {
  dd <- data.frame(name=sample(LETTERS[1:3], size=9, replace=TRUE), val=runif(9)*9)  
}

# Process in a loop
dd <- createData()
dd$out <- NA
for (i in seq_along(dd$out) ) {
  y <- bracketMe(dd$val[i])
  dd$out[i] <- y
}

# Use split-apply-combine manual
dd <- createData()
dd$out <- bracketMeVec(dd$val)

# Use plyr
library(plyr)
dd <- createData()
mutate(dd, out=bracketMeVec(val))
dd <- createData()
ddply(dd, "name", transform, out=mean(val))
ddply(dd, "name", summarise, out=mean(val))
ddply(dd, .(), transform, out=bracketMeVec(val))

# Functions
bracketMe <- function(x) {
  if (x > 0 & x <=3) {
    1
  } else if (x > 3 & x <=7) {
    2
  } else {
    3
  }
}
bracketMeVec <- Vectorize(bracketMe, SIMPLIFY=TRUE)
bracketMeVec(c(1.1, 3.3, 2.1, 7.6))

