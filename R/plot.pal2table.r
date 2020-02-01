#'@title Plots pal2table object  
#'
#'@description Plots symbols, colors and the corresponding factor labels
#' of a pal2table object.
#'
#'@param x An object created with pal2table
#' 
#' 
#'@author Pedro Martinez Arbizu
#'
#'@import graphics 
#'@examples
#' data(iris)
#' irc <- pal2table(iris$Species)
#' plot(irc)
#'
#'@export plot.pal2table
#'@seealso \code{\link{pal2table}} 


plot.pal2table <- function(x){
plot(rep(1,nrow(x)),nrow(x):1,cex=2,pch=x$pch,bg=as.vector(x$col),axes=FALSE,ann=FALSE,xlim=c(1,10))
points(rep(1,nrow(x))+0.5,nrow(x):1,cex=2,pch=x$pch2,col=as.vector(x$col))
text(rep(1,nrow(x))+1,nrow(x):1,labels=x$class,pos=4)
}

