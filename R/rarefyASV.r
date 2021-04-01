#'@title rarefy and Plot ASV reads  
#'
#'@description  
#'
#'@param x ASV table
#'
#'@param step Step used to rarefy 
#'@details
#'  
#'@return A plot  
#'
#'@author Pedro Martinez Arbizu
#'
#' 
#'@examples 
#' 
#'
#'
#'
#'@export rarefyASV
#'@export plot.rarefyASV
#'@exportS3Method plot rarefyASV
#'@seealso  



###rarefaction function
rarefyASV <- function(x,step){

N <- apply(t(x),FUN=sum,MARGIN=1)
stepx <- seq(1,max(N),round(max(N)/step))

maxn <- c()
for (elem in 1:length(N)){
maxn <- c(maxn,which(stepx > N[elem])[1])
}
maxn[is.na(maxn)] <- length(stepx)

rfy <- rarefy(t(x),sample= stepx)
ret <- (list(N=N,stepx=stepx,maxn=maxn,rare = rfy))

class(ret) <- c("rarefyASV", "list")
return(ret)

}

plot.rarefyASV <- function(x, ...){

plot(1,1,xlim=c(0,max(x$stepx)),ylim=c(0,max(x$rare)),
type='n',xlab='number of reads', ylab= 'number of ASVs',
main='sample based rarefaction')
 
for (elem in 1:nrow(x$rare)){
lines(x$stepx[1:x$maxn[elem]],x$rare[elem,1:x$maxn[elem]], ...)
}
}


