#'@title Color Palette to Table  
#'
#'@description Creates a data frame assigning colors and symbols to factors 
#'
#'@param x A vector of factors
#'
#'@param pch A list of symbols to be used
#'
#'@param pch2 a second list of symbols
#'
#'@param alpha The transparency value for the color
#' 
#'@param pal A vector of colors. Default is built-in color palette
#'
#'@details
#' Aim of the the functions pal2table and match2table is to provide an easy framework for assigning same colors and symbols for same factors
#' in different plots. Matching colors and symbols to factors is complicated when different graphs do not have the same factors in same order.
#' pal2table will create a data frame assigning colors and symbols to factors. An object of type pal2table is created that will be interpreted
#' by match2table. The later function can be used to create a vector of colors and symbols to be used by plot graphical parameters col, bg, fg, or pch.
#' The function will assign the colors and symbols matching the name of factors in the data frame to the provided vector of names. 
#' pal2table has a plot method which will plot the assigned colors and symbols together with the names of the factors.
#' The built-in color palette is modified from 'brocolors' from package 'broman' by Karl W Broman. It accepts changing the alpha value using the argument alpha.
#' Using a different color palette with argument pal will probably inabilitate the alpha modulation. Please adjust alpha in your palette forehand.
#' Note that you need to coerce col column to a vector if you want to use it directly from the pal2table object.
#' See the legend example with iris below.  
#'  
#'@return An object of class pal2table.
#'  Dataframe with following columns.
#' \item{class}{names of the factors} 
#' \item{col}{assigned color} 
#' \item{pch}{assigned symbol} 
#' \item{pch2}{assigned second symbol} 
#' 
#'@author Pedro Martinez Arbizu
#'
#'@import graphics 
#'@examples
#' data(iris)
#' irc <- pal2table(iris$Species)
#' plot(irc)
#'
#'#example with iris data
#' col <- match2table(iris$Species,irc,'col')
#' pch <- match2table(iris$Species,irc,'pch')
#' pch2 <- match2table(iris$Species,irc,'pch2')
#'
#'#add space below graph
#' par(oma = c(2, 0, 0, 0),xpd=NA)
##plot graph
#' plot(iris[,1],iris[,3],pch=pch,bg=col,cex=2,xlab='Sepal length',ylab='Petal length')
##add the legnd
#' legend(min(iris[,1]), min(iris[,3])-2,bty='n',ncol=3, pt.cex=1.5, legend=irc$class,pch=irc$pch2, col=as.vector(irc$col))
#' legend(min(iris[,1]), min(iris[,3])-2,bty='n',ncol=3, pt.cex=1.5, legend=irc$class,pch=irc$pch)
#'
#'#example with barplot
#' barplot(as.matrix(iris[,-c(5)]),col=col,border=NA)
#'
#'@export pal2table
#'@seealso \code{\link{match2table}} 

pal2table <- function(x, pch = 21:24, pch2 = c(19,15,18,17), alpha=220, pal=colors) {

if((inherits(x,'numeric'))) {
        warning("x numeric values coerced to factors\n ")}
		
 	# define some colors	
	colors <- c(
	'lightblue' =rgb(102,203,254,maxColorValue=255,alpha=alpha),
	'pink' =rgb(254,102,254,maxColorValue=255,alpha=alpha),
	'green' =rgb(102,254,102,maxColorValue=255,alpha=alpha),
	'yellow' =rgb(254,203,102,maxColorValue=255,alpha=alpha),
	'darkblue' =rgb(  0,128,128,maxColorValue=255,alpha=alpha),
	'bgpng' =rgb(32, 32, 32, maxColorValue=255,alpha=alpha),
	'AJ' =rgb(240,240,  0,maxColorValue=255,alpha=alpha),
	'B6'  =rgb(128,128,128,maxColorValue=255,alpha=alpha),
	'redbrown' =rgb(200,100,50,max=255,alpha=alpha),
	'steelgreen' =rgb(20,210,200,max=255,alpha=alpha),
	'129' =rgb(240,128,128,maxColorValue=255,alpha=alpha),
	'NOD2' =rgb( 16, 14,250,maxColorValue=255,alpha=alpha),
	'CAST'=rgb(  0,160,  0,maxColorValue=255,alpha=alpha),
	'marfil2' =rgb(150,150,200,max=255,alpha=alpha),
	'NZO2' =rgb(  0,180,255,maxColorValue=255,alpha=alpha),
	'PWK' =rgb(240,  0,  0,maxColorValue=255,alpha=alpha),
	'WSB' =rgb(144,  0,224,maxColorValue=255,alpha=alpha),
	'hotpink'    =rgb(254,  0,128,maxColorValue=255,alpha=alpha),	              
	'myorange'     =rgb(255,  170,0,maxColorValue=255,alpha=alpha),
	'mypink' =rgb(250,150,200,max=255,alpha=alpha),
	'lightpurple'=rgb(190,192,50,maxColorValue=255,alpha=alpha)
	)
	
	# coerce x to factor
	x <- as.factor(x)

	#define matching table
	coltable <- data.frame(class=unique(x),
					col=rep(pal,ceiling(length(unique(x))/length(pal)))[1:length(unique(x))],
					pch=rep(pch,ceiling(length(unique(x))/length(pch)))[1:length(unique(x))],
					pch2=rep(pch2,ceiling(length(unique(x))/length(pch2)))[1:length(unique(x))])
					
class(coltable) <- c("pal2table", "data.frame")
return(coltable)					
}

