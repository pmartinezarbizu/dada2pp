#'@title Aggregate ASVs to putative species   
#'
#'@description Several ASVs will be assigned to same species name. This function aggregates ASVs according to a variable (species name, accession number etc) 
#' Caution be sure than only aggregate relieable species assignments. 
#'
#'@param x the columns containing number of reads 
#'@param by The grouping variable
#'@param other Columns containing classification or other valuable information
#'
#'@details
#'
#' 
#'@author Pedro Martinez Arbizu
#'@import stats 
#'
#'@examples
#' data(deepMeio)
#' # grade >= 97 will give an indication of good species assignment
#' grade <- ((deepMeio$pident) + 2*(deepMeio$qcovs))/3
#' shortMeio <- deepMeio[grade>= 97,]
#' # grade > 97 will give an indication of good species assignment
#' # retrieve classification from WORMS
#' tax_deepMeio <- taxFromWorms(shortMeio$Species)
#'
#'# aggregate ASVs  
#' agg_tax_deepMeio <- aggregateASV(x=shortMeio[,9:ncol(shortMeio)], by=shortMeio$Species,other_str=tax_deepMeio[,2:6],other_num=shortMeio[,2:5])
#' 
#' agg_tax_deepMeio_2 <- aggregateASV(x=shortMeio[,9:ncol(shortMeio)], by=shortMeio$Species,other_num=shortMeio[,2:5])
#' 
#'@export  aggregateASV
#'@seealso \code{\link{taxFromWorms}} 
#'

aggregateASV <- function(x,by,other_str,other_num){ 

xres <- try(aggregate(x,by=list(by),FUN=sum),silent = TRUE)
other_str_res <- try(aggregate(other_str,by=list(by),FUN=unique)[,-1],silent = TRUE)
other_num_res <- try(aggregate(other_num,by=list(by),FUN=median)[,-1],silent = TRUE)

if(hasArg(other_str) & !hasArg(other_num)){ res <- data.frame(other_str_res,xres)
}else if(!hasArg(other_str) & hasArg(other_num)){ res <- data.frame(other_num_res,xres)
}else if(hasArg(other_str) & hasArg(other_num)){res <- data.frame(other_str_res,other_num_res,xres)}
return(res)
}
