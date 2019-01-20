# PermImp

perImp <- function(x, data, strata = NULL, permutations=1, ... ){

#describe parent call function 
	ststri <- ifelse(is.null(strata),'Null',strata)
	fostri <- as.character(x)

#copy model formula
	x1 <- x
# extract left hand side of formula
	lhs <- x1[[2]]
# extract factors on right hand side of formula 
	rhs <- x1[[3]]
# create model.frame matrix  
	x1[[2]] <- NULL   
	rhs.frame <- model.frame(x1, data, drop.unused.levels = TRUE) 


# calculate the empirical cumulative distribution
# function and its quantiles according to alpha
#	null.ecdf <- ecdf(ad.null)
#	qlow <- quantile(null.ecdf,alpha)
#	qup <- quantile(null.ecdf,1-alpha)
 
# variable names
	sv <-  colnames(eval(lhs))

# results matrix
	res <- matrix(ncol=5,nrow=length(sv))

###########
########### exclude species one by one
	for (elem in (1:length(sv))){
	
	#reduce model elements  
	if(inherits(eval(lhs),'dist')){	
	    xred <- as.dist(as.matrix(eval(lhs))[-elem,-elem])
	}else{
	xred <- eval(lhs)[,-elem]
	}
		
# redefine formula
	if(length(rhs) == 1){
		xnew <- as.formula(paste('xred',as.character(rhs),sep='~'))	
		}else{
		xnew <- as.formula(paste('xred' , 
					paste(rhs[-1],collapse= as.character(rhs[1])),
					sep='~'))}
					
#pass new formula to adonis
	if(is.null(strata)){
	ad <- adonis(xnew,data=data, permutations=permutations, ... )
	}else{ad <- adonis(xnew,data=data,strata= mdat1[,strata], permutations=permutations, ... )}
	
	res[elem,1] <- ad$aov.tab[1,2]
	res[elem,2] <- ad$aov.tab[1,3]
	res[elem,3] <- ad$aov.tab[1,4]
	res[elem,4] <- ad$aov.tab[1,5]
	res[elem,5] <- ad$aov.tab[1,6]
	
	#calculate confidence intervals of null model
	
	}
	
#create results data frame	
 res <- data.frame(sv,res)
 colnames(res) <- c('variable','SumsOfSqs','MeanSqs','F.Model','R2','Pr(>F)')

# jackkniffe
	jm <- mean(res$F.Model) # this is the unbiased jk estimate of
							# mean F erosion when removing 1 variable
	jv <- var(res$F.Model)/length(res$F.Model) # variance of the unbiased estimator
	jkup <- mean(res$F.Model) + qt(0.975,length(res$F.Model)-1)*sqrt(var(res$F.Model)/length(res$F.Model)) # upper 95% c.i.
	jklow <- mean(res$F.Model) - qt(0.975,length(res$F.Model)-1)*sqrt(var(res$F.Model)/length(res$F.Model)) # lower 95% c.i.
	
	jackkniffe <- data.frame(unbiased.F=jm,unbiased.var.F=jv,'up95CI'=jkup,'low95CI'=jklow)	

# add some columns to results table
	func <- rep('n.s.',length(res$F.Model))
	func[res$F.Model < jklow] <- "segregating"
	func[res$F.Model > jkup] <- "aggregating"
	
	res <- cbind(res, diff.F = res$F.Model - jm)
	res <- cbind(res, effect = func)
	res <- res[order(res$diff.F),]
 
return(list(res=res,jackkniffe=jackkniffe))
}

#perImp(iris[,1:4]~Species,data=iris)
#perImp(dune~Management,data=dune.env)

par(oma = c(1, 0, 1, 0))
plot(p$res$F.Model,nrow(p$res):1,type='n',axes=FALSE,xlab='',ylab='',xlim=c(min(p$res$F.Model)-0.5,max(p$res$F.Model)))
abline(v=p$jackkniffe[1],lty=2)
abline(v=c(p$jackkniffe[3],p$jackkniffe[4]))

points(p$res$F.Model,nrow(p$res):1)
for(elem in nrow(p$res):1){ text(min(p$res$F.Model)-0.5,elem,labels=p$res$variable[elem],pos=4)}
abline(h=-0.2,lwd=2)
abline(h=nrow(p$res)+1,lwd=2)
 