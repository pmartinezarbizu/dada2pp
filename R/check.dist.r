#'@title Check for Distribution   
#'
#'@description  Use MASS fitdistr function to find best fitting distribution
#'
#'@param x vector of values
#'
#'@details If x contains integer values, fitdistr is used to check for "negative binomial" or "Poisson"
#' distribution. If x is float following distributions are tested "lognormal",
#' "normal", "exponential","geometric",'gamma'. Solution are ranked according to 
#' AIC information criterium. Lowest is best.
#'  
#'@return Ranked solutions 
#'
#'@author Pedro Martinez Arbizu
#'
#'@import MASS 
#'@examples
#' check.dist(rnorm(30,20,1)) 
#'
#' # negative binomial distribution from a collection of poisson samples 
#' d <- c()
#' for(i in 1:20){d=c(d,rpois(50,sample(10:1000,1)))}
#' check.dist(d)
#'
#'
#'
#'
#'@export check.dist
#'@seealso \code{\link{fitdistr}} 
#'



#############FUNCTION to chek distribution CHECK if INTEGER for count data
check.dist <- function(x){

res <- c()
if(class(x) == 'integer'){
dist <- c("negative binomial","Poisson")
for(d in dist){
res <- c(res,AIC(fitdistr(x,d)))
}
}else{
dist <- c("lognormal", "normal", "exponential","geometric",'gamma')
for(d in dist){
res <- c(res,AIC(fitdistr(x,d)))
}}
top <- data.frame(dist=dist[order(res)],AIC=res[order(res)])
param <- fitdistr(x,dist[order(res)][1])
return(list(AIC = top, Pa = param))
}

