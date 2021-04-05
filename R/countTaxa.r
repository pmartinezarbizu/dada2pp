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
#'@examples countTaxa(deepMeio[,9:27],deepMeio$Group,fac= c(rep('area1',7),rep('area2',12)))
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
sha_n <- c()
for(i in 1:length(unique(fac))){
res <- cbind(res, table(as.factor(taxa)[shared==i]))
sha_n <- c(sha_n,paste('shared',i,sep='_')) 
}
colnames(res) <- c(by_fac[,1],sha_n) 
res <- cbind(total,res )
return(res)
}
 
