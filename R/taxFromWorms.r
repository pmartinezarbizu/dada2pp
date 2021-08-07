#'@title Retrieves Taxonomy from World Register of Marine Species  
#'
#'@description Query a vector to taxa names against WORMS and retrives valid names and classification 
#'
#'@param x A vector of species names 
#'
#'@param sep separator between genus and species. Default is space ' ' or '_' 
#'
#'@param ... additional arguments passed to worms::wormsmatchsp and worms::wormsmatchgen.
#'
#'@details
#' The taxonomic assignment of the ASVs resulting from blasting against GenBank is not allways up to date. This fuction will query the names 
#' against Worl Register of Marine Species at https://www.marinespecies.org/ by using the package worms.
#' The function returns a data.frame with the columns "kingdom", "phylum", "class", "order",
#' "family", "genus", "AphiaID", "status", "scientificname", "valid_name" and"query_name".
#'
#' The function assumes that the separator between genus name and species name is either a space or an underscore. 
#' 
#'@author Pedro Martinez Arbizu
#'@import worms 
#'
#'@examples
#' data(deepMeio)
#' tax_deepMeio <- taxFromWorms(deepMeio$Species[1:51])
#' plot(irc)
#'
#'#example retrieve non only marine species 
#' tax_deepMeio <- taxFromWorms(deepMeio$Species[1:51], marine_only=FALSE)
#' 
#' 
#' 
#'@export taxFromWorms
#'@seealso \code{\link{worms}} 


		taxFromWorms <- function(x, ...){
		
		cnksize <- ifelse(length(x) %% 50 == 1,55,50)
		
		spnames <- gsub(x=x,'[ _]',' ')
		genus <- sub( '[ _].*','',x)
		wormsmatchsp <- wormsbynames(spnames, chunksize = cnksize, ...)
		wormsmatchsp <- wormsmatchsp[,c(1,5,3,10)]	
		wormsmatchgen <- wormsbynames(genus, chunksize = cnksize, ...)
		wormsmatchgen <- wormsmatchgen[,c(13:18)]	
		dat <- data.frame(wormsmatchgen,wormsmatchsp, query_name = x)
		dat[is.na(dat)] <- 'no_match'
		return(dat)
		}
