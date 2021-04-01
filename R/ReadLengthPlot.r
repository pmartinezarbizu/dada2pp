#'@title Plot ASV read length frequency  
#'
#'@description  
#'
#'@param x The column with read length information
#'
#'@details
#'  
#'@return A plot  
#'
#'@author Pedro Martinez Arbizu
#'
#' 
#'@examples countTaxa(deepMeio[,9:27],deepMeio$Group,fac= c(rep('area1',7),rep('area2',12)))
#' 
#'
#'
#'
#'@export ReadLengthPlot
#'@seealso  


ReadLengthPlot <- function(x, xlab='read length', ylab='number of  ASVs', mode.lab='bp', pos = 2, ... ){

mode <- as.integer(names(which(table(x)==max(table(x)))))
counts <- max(table(x))
plot(table(x),ylab=ylab,xlab=xlab, ...)
text(mode,counts,labels=paste (mode, mode.lab),pos = pos, ...)

}
 
