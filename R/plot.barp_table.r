#'@title Method plot for object barp_table  
#'
#'@description Plots 4 barplots in one page using a barp_table object as input 
#'
#'@param x Object of class barp_table
#'
#'@param main One of 'vsearchsum' or 'dada2sum' or a vector with 4 elements which are the titles the plots 
#'
#'@param pal2table Object of class pal2table for matching colors to Taxa 
#' 
#'@param oma1 Space on lower margin to allocate legend (passed to graphical parameter oma)
#'
#'@param ncol.leg Number of columns of legend
#'
#'@param x.leg X value for upper left corner of legend
#'
#'@param y.leg Y value for upper left corner of legend
#'
#'@param pt.cex Size of symbol in legend
#'
#'@param x.int Valeue passd to x.intersp (see ?legend )
#'
#'@details
#' This takes a table created by barp.table and creates 4 barplots
#' whith absolute and relative number of reads
#' and absolute and relative number of OTUs/ASV per Taxon
#'
#'@return A graph with 4 barplots
#'
#'@author Pedro Martinez Arbizu
#'
#'@import graphics vegan
#'@examples
#' 
#'
#'
#'@export barp.table
#'@seealso \code{\link{plot.barp_table}} 


plot.barp_table <- function(x,
							main='dada2sum',
							pal2table,
							oma1=12,
							ncol.leg=ceiling(nrow(x$table)/5),
							x.leg= -ncol(bp1$table)*1.5,
							y.leg=-0.5,
							pt.cex=2.5,
							x.int=1) {
vsearchsum <- c('absolute number of reads per Taxon',
				'relative number of reads per Taxon',
				'absolute number of OTUs per Taxon',
				'relative number of OTUs per Taxon')
dada2sum <-	  c('absolute number of reads per Taxon',
				'relative number of reads per Taxon',
				'absolute number of ASVs per Taxon',
				'relative number of ASVs per Taxon')
#initial checks				
if(!(inherits(x,'barp_table'))) {
        stop("x should be a barp_table Object\n ")}
if(!(inherits(pal2table,'pal2table'))) {
        stop("pal2table should be a pal2table Object\n ")}

ifelse(main %in% c('vsearchsum','dada2sum'),
	if (main=='vsearchsum'){
	main=vsearchsum
	}else if (main=='dada2sum'){
	main=dada2sum
	},main)

		
		
#adjust graphical parameters
par(mfrow=c(2,2), oma = c(oma1, 0, 0, 0),xpd=NA)

# match colors with taxa
col <- match2table(rownames(x$table),pal2table,'col')
#plot 1 and 2
barplot(x$table,las=2,  col=col, main=main[1])
barplot(decostand(x$table,method='total',MARGIN=2),las=2,col=col,main=main[2])

x_pa <- decostand(x$x,method='pa',MARGIN=2) 
x_pa_tab <- barp.table(x_pa,x$by,sum)

# match colors with taxa
col <- match2table(rownames(x_pa_tab$table),pal2table,'col')
#plot 3 and 4
barplot(x_pa_tab$table,las=2,col=col,main=main[3])
barplot(decostand(x_pa_tab$table,method='total',MARGIN=2),las=2,col=col,main=main[4])

legend(x.leg,y.leg,ncol=ncol.leg,legend=rownames(x_pa_tab$table),bty='n',pch=15,col=as.vector(col),pt.cex=pt.cex, x.intersp=x.int )
legend(x.leg,y.leg,,ncol=ncol.leg,legend=rownames(x_pa_tab$table),bty='n',pch=0,pt.cex=pt.cex, x.intersp=x.int )		

}

#examples
bp1 <- barp.table(meio[,9:27],meio$Group,sum)
colmeio <- pal2table( c(rownames(bp1$table),'Branchiopoda','Calanoida','Cyclopoida','Harpacticoida','Isopoda','Misophrioida','Ostracoda','Siphonostomatoida','Tanaidacea','Tantulocarida'),pal='c25bro2')
plot(bp1,pal2table=colmeio)
