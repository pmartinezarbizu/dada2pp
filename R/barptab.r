##new fuction barp.sample	
###########################################################
barp.sample <- function(x, col, by , fun,...) {
res <- matrix(ncol=1, nrow=length(unique(by)))

for(i in colnames(col)){
res <- cbind(res,aggregate(x[,i] ~ by, x, fun)[2])
}

rnames <- aggregate(x[,i] ~ by, x, fun)[1]
res <- as.matrix(res[,2:ncol(res)]) 
colnames(res) <- names(col)
rownames(res) <- rnames[,1]
res <- res[order(apply(res,1,sum),decreasing=TRUE),]
return(res)
}   
###########################################################
