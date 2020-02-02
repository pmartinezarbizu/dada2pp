#'@title Diversity Values Based on Taxon Table 
#'
#'@description Calculate miscelaneous diversity values 
#'
#'@param x community table
#'
#'@param by aggregating vector
#'
#'@param sample number of n for rarefaction (default is min(N)*0.8)
#' 
#'@param pal Palette object from pal2table
#'
#'@details Returns parameters N= number of specimens, S= number of species,
#' H= Shannon index, J= Pielou's evenness and ES= estimated number od species for N =sample
#' 
#'@return An object of class divers_tt
#' This object has a plot method
#'
#'
#'@author Pedro Martinez Arbizu
#'
#'@import graphics vegan 
#'@examples
#' 
#'
#'
#'@export diverse.tt
#'@seealso \code{\link{plot.diverse_tt}} 

diverse.tt <- function(x,by,sample=floor(min(N)*0.8),pal){

#Number of individuals
	N <- apply(x,FUN=sum,MARGIN=2)
#Shannon
	H <- diversity(t(x))
# Species richness (S)
	S <- specnumber(t(x)) 
# Pielou's evenness
	J <- H/log(S)
# Rarefaction
	sample <- sample 	
	ES <- rarefy(t(x),sample)
# colors
	col <- match2table(by,pal,'col')
	pch <- match2table(by,pal,'pch')
	pch2 <- match2table(by,pal,'pch2')
	
res <- data.frame(area=by,N,S,H,J,ES,col=col,pch=pch,pch2=pch2)
class(res) <- c("diverse_tt", "data.frame")
return(res)
}


