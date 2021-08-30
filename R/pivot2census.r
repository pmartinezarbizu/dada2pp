#'@title From Community Table to Census Format  
#'
#'@description  
#'
#'@param x 
#'@param y 
#'@param dat 
#'@details
#'  
#'@return   
#'
#'@author Pedro Martinez Arbizu
#'
#' 
#'@examples 
#' 
#'
#'@export pivot2census
#'@seealso  


##
pivot2census <- function(x,y,dat){
cols <- rep(x,length(y))
rows <- rep(y,each=length(x))
dfd <- c()
for(i in 1:length(y)){
dfd <- c(dfd,dat[i,])
}
return(res = data.frame(cols,rows,dfd))
}


