#'@title remove ASV outliers using observed distribution  
#'
#'@description Recognize and remove ASV outliers according to selected variables
#'
#'@param x The community table with Taxon reads counts
#'
#'@param by A list with variables used to produce the threshold
#'
#'@param n_var A number, minimum number of variables that should be below threshold to declare outliers default = 1.  
#'
#'@details This function will detect outlier ASVs and divide the data.frame into outliers and not outliers accordingly. The threshold will be assigned by the lower wisker of the boxplot (1.5x interquartile variation).
#' Variables used to produced the treshhold a free configurable, but query coverage, percent identity and query length are meaningful.
#' User can define how many of the selected variables should be below the hreshold for the ASV to be declared as outlier. Default is 1, which correspond to combine the variables using an 'or' logical operator. 
#'    
#'  
#'@return Two data.frames containing outliers and non outliers.
#' \item{keep}{data.frame with non outlier ASVs (above the treshold)} 
#' \item{outliers}{data.frame with outlier ASVs  (below the treshold)} 
#' \item{thresholds}{Table with thresholds used} 
#' \item{AVS_tm}{Table with thresholds matrix per ASV} 
#'
#'@author Pedro Martinez Arbizu
#'
#'@import graphics 
#'@examples ASV_outliers(x=deepMeio[,9:27],by=data.frame(deepMeio$qcovs,deepMeio$length,deepMeio$pident),n_var=1)
#' 
#'
#'
#'
#'@export ASV_outliers
#'@seealso \code{\link{boxplot}} 


ASV_outliers <- function(x,by,n_var=1){

#get the thresholds table
thres <- c()

for(i in 1:length(by)){
thres <- cbind(thres, boxplot(by[i],plot=FALSE)$stats)
}
colnames(thres) <- names(by)
rownames(thres) <- c('lower.Whisker','1st.quartile','median','3rd.quartile','upper.Whisker')

#ASV-threshold matrix
ASV_thres <- c()
for(i in 1:length(by)){
ASV_thres <- cbind(ASV_thres,
ifelse(unlist(by[i])>thres[1,i],0,1))
}
ASV_thres <- data.frame(ASV_thres,apply(ASV_thres,FUN=sum,MARGIN=1))
row.names(ASV_thres) <- row.names(x)
colnames(ASV_thres) <- c(names(by),'sum')

keep <- data.frame(x[ ASV_thres$sum < n_var,])
outliers <- data.frame(x[ ASV_thres$sum >= n_var,])
names(keep) <- names(x)
names(outliers) <- names(x)

res <- list(keep=keep,outliers=outliers,thresholds=thres,ASV_tm=ASV_thres )
return(res)
}
 