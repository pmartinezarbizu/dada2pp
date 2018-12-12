#'@title Diversity Values Based on Taxon Table 
#'
#'@description  
#'
#'@param x
#'
#'@param by 
#'
#'@param sample
#' 
#'@param pal
#'
#'@details
#' 
#'@return An object of class divers_tt
#'
#'
#'@author Pedro Martinez Arbizu
#'
#'@import graphics vegan 
#'@examples
#' 
#'
#'
#'@export divers.tt
#'@seealso \code{\link{plot.divers_tt}} 

divers.tt <- function(x,by,sample=min(N)*0.8,pal){

#Number of individuals
	N <- apply(x,FUN=sum,MARGIN=1)
#Shannon
	H <- diversity(x)
# Species richness (S)
	S <- specnumber(x) 
# Pielou's evenness
	J <- H/log(S)
# Rarefaction
	sample <- sample 	
	ES <- rarefy(x,sample)
# colors
	col <- match2table(by,pal,'col')
	pch <- match2table(by,pal,'pch')
	pch2 <- match2table(by,pal,'pch2')
	
res <- data.frame(area=by,N,S,H,J,ES,col=col,pch=pch,pch2=pch2)
class(res) <- c("divers_tt", "data.frame")
return(res)
}


