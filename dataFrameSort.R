# Create data
createData <- function(n=9) {
  df <- data.frame(num=sample(1:n), val=runif(n)*9)  
}

df <- createData(15)

# Sort on a column
df <- df[order(df$num), ]

# Sort in a given order (by row no.)
newOrder <- 15:1
df <- df[newOrder,]

# Sort in random order
ranOrder <- sample(15)
df <- df[ranOrder,]