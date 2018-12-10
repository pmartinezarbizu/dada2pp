#'@title Create statistics of Taxon table and reduces to target taxa  
#'
#'@description As input takes a taxon table and some vectors with names of contaminants and non-target taxa
#'
#'@param x A taxon table with all its colums
#'
#'@param reads The subset of colums containing the reads
#'
#'@param by The column containing the grouping names
#'
#'@param cont A vector of contaminants taxon names
#' 
#'@param non_target A vector of non-target taxon names
#'
#'@details
#'  TO be ...
#'
#'@return Table with summary statistics, the vectors of contanimants, non-target and target taxa
#' A reduced taxon table containing only target taxa  
#' 
#'@author Pedro Martinez Arbizu
#'
#'@import graphics 
#'@examples
#' st.mf <- stats.tt(mf,mf[,9:27],mf$Group,cont,notbemet)
#'
#'@export stats.tt
#'@seealso \code{\link{match2table}} 

stats.tt <- function(x,reads,by,cont,non_target){
rnames <- c('total','contamination','non_target','target')
n_taxa <- c(length(unique(by)),
			length(cont),
			length(non_target),
			length(unique(by[!by %in% c(cont,non_target)]))) 

n_reads <- c(sum(apply(reads,MARGIN=1,FUN=sum)),
			sum(apply(reads[by %in% cont,],MARGIN=1,FUN=sum)),
			sum(apply(reads[by %in% non_target,],MARGIN=1,FUN=sum)),
			sum(apply(reads[!by %in% c(cont,non_target),],MARGIN=1,FUN=sum)))

n_otus <-c(nrow(reads),
			nrow(reads[by %in% cont,]),
			nrow(reads[by %in% non_target,]),
			nrow(reads[!by %in% c(cont,non_target),]))

target.taxa <- unique(by[!by %in% c(cont,non_target)])
target.table <- x[by %in% target.taxa,]

stats <- data.frame(n_taxa=n_taxa,n_reads=n_reads,n_otus=n_otus,row.names=rnames)

return( list(stats=t(stats),cont=cont,non_target=non_target,target=target.taxa,target.table=target.table))
}

