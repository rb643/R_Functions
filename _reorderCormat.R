reorder_cormat <- function(cormat){
  #function to return the reordering indices of a matrix based on hclust
# Use correlation between variables as distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]

return(hc$order)
}
