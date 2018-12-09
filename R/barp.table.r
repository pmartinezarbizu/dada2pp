#'@title Barplot Table  
#'
#'@description Creates a table that be be used for a barplot graph 
#'
#'@param x The community table with Taxon reads counts
#'
#'@param by Aggregating vector of classes 
#'
#'@param fun Function used to aggregate. Default is sum. 
#' 
#'
#'@details
#' This is a wrapper function to 'aggregate' and 'apply'.
#' It takes a Taxon table (OTU or ASV table) and counts the number of reads 
#' aggregating by the vector specified by 'by' using function specified with 'fun'  
#'  
#'@return An object of class barp_table
#' A matrix with Taxa and aggregate values.
#' The object has plot method, which will produce barplots
#' For absolute and relative number of reads
#' and absolute and relative number of OTUs/ASV per Taxon
#'
#'@author Pedro Martinez Arbizu
#'
#'@import graphics 
#'@examples
#' 
#'
#'
#'@export barp.table
#'@seealso \code{\link{plot.barp_table}} 

##new fuction barp.sample	
###########################################################
barp.table <- function(x, by , fun,...) {
res <- matrix(ncol=1, nrow=length(unique(by)))

for(i in colnames(x)){
res <- cbind(res,aggregate(x[,i] ~ by, x, fun)[2])
}

rnames <- aggregate(x[,i] ~ by, x, fun)[1]
res <- as.matrix(res[,2:ncol(res)]) 
colnames(res) <- names(x)
rownames(res) <- rnames[,1]
res <- res[order(apply(res,1,sum),decreasing=TRUE),]

ret <- list(x=x, by=by, table=res)
class(ret) <- c("barp_table", "matrix")

return(ret)
}   
###########################################################
