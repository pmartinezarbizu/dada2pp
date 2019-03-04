#'@title Plot Method for divers.tt 
#'
#'@description  
#'
#'@param x
#'
#'@details
#' 
#'@return A graph with 4 boxplots
#'
#'
#'@author Pedro Martinez Arbizu
#'
#'@import graphics vegan 
#'@examples
#' 
#'
#'
#'@export plot.divers_tt
#'@seealso \code{\link{plot.divers_tt}} 

plot.divers_tt <- function(x,ty='ASVs'){
if(!(ty %in% c('ASVs','OTUs'))) {error("ty must be either \'ASVs\' or \'OTUs\'\n ")}
		
#typemain <- ifelse(ty=='ASVs','ASVs','OTUs')

par(mfrow=c(2,2),xpd=NA)
boxplot(x$N~x$area,col=as.vector(unique(x$col)),main='Number of reads per sample')
boxplot(x$S~x$area,col=as.vector(unique(x$col)),main=paste('Number of',ty, 'per sample'))
boxplot(x$J~x$area,col=as.vector(unique(x$col)),main='Evenness per sample')
boxplot(x$ES~x$area,col=as.vector(unique(x$col)),main=paste('Expected',ty, 'per sample'))
}
