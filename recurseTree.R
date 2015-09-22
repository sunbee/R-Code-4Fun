# Recursively process a tree
# Tree is list nested within lists

l13 <- list(d=1)
l12 <- list(b=1, c=l13)
l11 <- list(a=l12)

# Template

recurseTree <- function(ll, appendMe='root') {
  leaf_node <- sapply(names(ll), function(x) !is.list(ll[[x]]))
  paths <- sapply(names(ll), function(x) { print(paste(appendMe, x, sep= '|'))} )
  names(paths) <- NULL
  
  if (length(which(!leaf_node))==0) {  # Only leaf nodes found  
    return(paths) 
  } else {
    new_paths <- sapply(names(ll)[which(!leaf_node)], function(x) { 
      recurseTree(ll[[x]], paste(appendMe, x, sep="|"))
      })
    paths <- c(paths, new_paths)
    names(paths) <- NULL
    return(paths)
  }
}

recurseTree(l11)
recurseTree(l12)
