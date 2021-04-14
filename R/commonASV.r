#'@title Most common ASVs
#'
#'@description Retrieve the most common ASVs by total number of reads and total number of samples present  
#'
#'@param x The community table with Taxon reads counts
#'
#'@param n Interger, Number of top ASVs to retrieve
#'
#'@details This function will order the ASVs by total number of reads and by
#' number of samples present and return two lists containing the of the n top ASVs
#'  
#'@return Two data.frames
#' \item{by_samp}{data.frame with the top n ASVs by presence in samples }
#' \item{by_reads}{data.frame with the top n ASVs by number of reads} 
#'
#'@author Pedro Martinez Arbizu
#'
#'@import vegan  
#'@examples 
#' 
#'@export commonASV
#'@seealso \code{\link{ASV_outliers}} 


commonASV <- function(x,taxa,n=50){
#die häufigsten by number of samples present 
by.samp <- apply(decostand(x,method='pa'),MARGIN=1,FUN=sum)
hau_by_sample <- data.frame(taxa[order(by.samp,decreasing=TRUE),][1:n,],N_samples=sort(by.samp,decreasing=TRUE)[1:n])


#total number of reads per ASVs
by.reads <- apply(x,FUN=sum,MARGIN=1)
hau_by_reads <- data.frame(taxa[order(by.reads,decreasing=TRUE),][1:n,],N_reads=sort(by.reads,decreasing=TRUE)[1:n])

return <- list(by_samp=hau_by_sample,by_reads=hau_by_reads)

}