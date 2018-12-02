#'@title Match Colors to Factors in a Table  
#'
#'@description Creates a vector fo colors or symbols matching the factors in a table created by pal2table 
#'
#'@param x A vector of observations to be plotted containing (at least some of) the factors in table
#'
#'@param table An object created by pal2table. The data frame containing factors, colors and symbols
#'
#'@param obj The column to be used for. One of 'col' for colors, 'pch' for symbols, 'pch2' for second set of symbols.
#'
#'@details
#' Aim of the the functions pal2table and match2table is to provide an easy framework for assigning same colors and symbols for same factors
#' in different plots. Matching colors and symbols to factors is complicated when different graphs do not have the same factors in same order.
#' pal2table will create a data frame assigning colors and symbols to factors. An object of type pal2table is created that will be interpreted
#' by match2table. The later function can be used to create a vector of colors and symbols to be used by plot graphical parameters col, bg, fg, or pch.
#' The function will assign the colors and symbols matching the name of factors in the data frame to the provided vector of names. 
#' match2table is a wrapper of function match.
#' 
#'@author Pedro Martinez Arbizu
#'@import graphics 
#'@examples
#' data(iris)
#' irc <- pal2table(iris$Species)
#' plot(irc)
#'
#' col <- match2table(iris$Species,irc,'col')
#' pch <- match2table(iris$Species,irc,'pch')
#' plot(iris[,1],iris[,3],pch=pch,bg=col,cex=2)
#' barplot(as.matrix(iris[,-c(5)]),col=col,border=NA)
#'
#'@export match2table
#'@seealso \code{\link{pal2table}} 


match2table <- function(x,table,obj=c('col','pch','pch2')) {

if(!(inherits(table,'pal2table'))) {
        stop("table should be a pal2table Object\n ")}
		
res <- as.vector(table[,obj][match(x,table$class)])
return(res)
}
