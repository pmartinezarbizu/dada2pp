#'@title Multiple Adonis Test for Taxon Table 
#'
#'@description  
#'
#'@param datasets a list of datasets
#'
#'@param fac factor for adonis (groups)
#'
#'@param nboot number of rarefied community datasets (bootstrap without replacement) to produce
#'
#'@param frac used to estimate the argument sample to be passed to rrarefy. Fraction (e.g.0.3) value corresponding to the minimum number of reads/obeservations per sample.
#'
#'@param ... additional arguments for adonis (e.g permutations)
#'
#'
#'
#'@details
#' 
#'@return A dataframe with results of adonis and results of adonis on rarefied dataset
#'
#'
#'@author Pedro Martinez Arbizu
#'
#'@import vegan splitstackshape
#'
#'@examples
#' datasets <- list('meio'= t(meio[,9:27]),'crust'= t(crust[,9:27]),'NeHaCyMi'= t(NeHaCyMi[,9:27]))
#' multiadonis(datasets,area,nboot=10,frac=0.5)
#' 
#'
#'
#'@export multiadonis
#'@seealso \code{\link{adonis}} 

multiadonis <- function(datasets, fac, nboot = 1, frac = 0.5, balance = FALSE, loghell = TRUE, distance= 'euclidean', ...   ){

##adonis permanova table
names <- names(datasets)
ad.F <- c()
ad.p <- c()

bd.F <- c()
bd.p <- c()

adpa.F <- c()
adpa.p <- c()

bdpa.F <- c()
bdpa.p <- c()


for (i in 1:length(datasets)){
dat <- datasets[[i]]

#remove factors having 0 observations
fac_dat <- fac[!(apply(dat,sum,MARGIN=2)==0)]
#remove samples having 0 observations
dat <- dat[,!(apply(dat,sum,MARGIN=2)==0)]

###################
##rarefy communitiy
if(nboot > 1){
drar <- c()
sumreads <- apply(dat,MARGIN=2,FUN=sum)
sample <- ceiling(min(sumreads[sumreads>0])*frac)
if(sample == 1){
sample <- min(sumreads[sumreads>0])
}

for(i in 1:nboot){
drar <- rbind(drar, rrarefy(t(dat),sample))
}
fac_dat <- rep(fac_dat,nboot)
dat <- t(drar)
} # end rarefy
##################

##################
##balance communtie
if(balance==TRUE){
minfac <- min(table(fac_dat))
datbal <-  stratified(data.frame(fac_dat,t(dat)),'fac_dat',minfac)
fac_dat <- datbal$fac_dat
dat <- t(datbal[,-1])
}
###################


###################
#transform loghell
if (loghell==TRUE){
dat <- decostand(log(dat+1),MARGIN= 2,method='hellinger')
}
###################

#calculate adonis and betadisper
ad <- adonis(t(dat) ~ fac_dat, distance=distance, ... )
ad.F <- c(ad.F,ad$aov.tab[1,4])
ad.p <- c(ad.p,ad$aov.tab[1,6])
bd <- permutest(betadisper(dist(t(dat)),fac_dat))
bd.F <- c(bd.F,bd[[1]][1,4])
bd.p <- c(bd.p,bd[[1]][1,6])

#now pa
datpa <- decostand(dat,'pa')
ad <- adonis(t(datpa) ~ fac_dat,distance=distance, ... )
adpa.F <- c(adpa.F,ad$aov.tab[1,4])
adpa.p <- c(adpa.p,ad$aov.tab[1,6])
bd <- permutest(betadisper(dist(t(datpa)),fac_dat))
bdpa.F <- c(bdpa.F,bd[[1]][1,4])
bdpa.p <- c(bdpa.p,bd[[1]][1,6])

} #end for clause

res <- data.frame(names,
ad.F = round(ad.F,2),
ad.p,
bd.F=round(bd.F,2),
bd.p = bd.p,
adpa.F=round(adpa.F,2),
adpa.p,
bdpa.F=round(bdpa.F,2),
bdpa.p)

return(data.frame(res))
}

