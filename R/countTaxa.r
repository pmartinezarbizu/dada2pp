#'@title Count ASVs/OTUs per Taxon and Area  
#'
#'@description  
#'
#'@param x The community table with Taxon reads counts
#'
#'@param taxa vector with taxon information
#'
#'@param fac vector of factors to discriminate  
#'
#'@details
#'  
#'@return A data frame 
#'
#'@author Pedro Martinez Arbizu
#'
#'@import vegan 
#'@examples countTaxa(meio[,9:27],meio$Group,area)
#' 
#'
#'
#'
#'@export countTaxa
#'@seealso \code{\link{barp.table}} 


countTaxa <- function(x,taxa,fac, ... ){
total <-  table(taxa)

by_fac <- aggregate(t(x),list(fac),sum)
by_fac_pa <- decostand(by_fac[-1],method='pa')
shared <- apply(by_fac_pa,MARGIN=2,FUN=sum)

#create a table of counts per area
res <- c()
for(elem in 1:nrow(by_fac)){
res <- cbind(res,table(as.factor(taxa)[t(by_fac_pa[elem,])>0]))
}
colnames(res) <- by_fac[,1] 
res <- cbind(total,res,shared =table(as.factor(taxa)[shared==2]) )
return(res)
}
 
