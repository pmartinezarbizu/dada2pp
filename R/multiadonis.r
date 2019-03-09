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
#'@import vegan 
#'
#'@examples
#' datasets <- list('meio'= t(meio[,9:27]),'crust'= t(crust[,9:27]),'NeHaCyMi'= t(NeHaCyMi[,9:27]))
#' multiadonis(datasets,area,nboot=10,frac=0.5)
#' 
#'
#'
#'@export multiadonis
#'@seealso \code{\link{adonis}} 

multiadonis <- function(datasets, fac, nboot = 1, frac = 0.3, ...   ){

##adonis permanova table
names <- names(datasets)
ad.F <- c()
ad.p <- c()
rar.ad.F <- c()
rar.ad.p <- c()

bd.F <- c()
bd.p <- c()
rar.bd.F <- c()
rar.bd.p <- c()

adpa.F <- c()
adpa.p <- c()
rar.adpa.F <- c()
rar.adpa.p <- c()


bdpa.F <- c()
bdpa.p <- c()
rar.bdpa.F <- c()
rar.bdpa.p <- c()


for (i in 1:length(datasets)){

dat <- datasets[[i]]
dat2 <- dat[!(apply(dat,sum,MARGIN=1)==0),]
dat2 <- decostand(log(dat2+1),method='hellinger')

ad <- adonis(dat2 ~ fac[!(apply(dat,sum,MARGIN=1)==0) ], ... )
ad.F <- c(ad.F,ad$aov.tab[1,4])
ad.p <- c(ad.p,ad$aov.tab[1,6])
bd <- permutest(betadisper(vegdist(dat2),fac[!(apply(dat,sum,MARGIN=1)==0) ]))
bd.F <- c(bd.F,bd[[1]][1,4])
bd.p <- c(bd.p,bd[[1]][1,6])

#now pa
datpa <- decostand(dat2,'pa')
ad <- adonis(datpa ~ fac[!(apply(dat,sum,MARGIN=1)==0) ],distance='euclidean', ... )
adpa.F <- c(adpa.F,ad$aov.tab[1,4])
adpa.p <- c(adpa.p,ad$aov.tab[1,6])
bd <- permutest(betadisper(vegdist(datpa,'euclidean'),fac[!(apply(dat,sum,MARGIN=1)==0) ]))
bdpa.F <- c(bdpa.F,bd[[1]][1,4])
bdpa.p <- c(bdpa.p,bd[[1]][1,6])

##rarefy communitiy
if(nboot>1){
drar <- c()
sumreads <- apply(dat,MARGIN=1,FUN=sum)
sample <- ceiling(min(sumreads[sumreads>0])*frac)
if(sample == 1){
sample <- min(sumreads[sumreads>0])
}

for(i in 1:nboot){
drar <- rbind(drar, rrarefy(dat,sample))
 }
fac2 <- rep(fac,nboot)
 
rar.dat2 <- drar[!(apply(drar,sum,MARGIN=1)==0),]
rar.dat2 <- decostand(log(rar.dat2+1),method='hellinger')

rar.ad <- adonis(rar.dat2 ~ fac2[!(apply(drar,sum,MARGIN=1)==0)], ... )
rar.ad.F <- c(rar.ad.F,rar.ad$aov.tab[1,4])
rar.ad.p <- c(rar.ad.p,rar.ad$aov.tab[1,6])
rar.bd <- permutest(betadisper(vegdist(rar.dat2),fac2[!(apply(drar,sum,MARGIN=1)==0) ]))
rar.bd.F <- c(rar.bd.F,rar.bd[[1]][1,4])
rar.bd.p <- c(rar.bd.p,rar.bd[[1]][1,6])

#now pa
rar.datpa <- decostand(rar.dat2,'pa')
rar.ad <- adonis(rar.datpa ~ fac2[!(apply(drar,sum,MARGIN=1)==0) ],distance='euclidean', ... )
rar.adpa.F <- c(rar.adpa.F,rar.ad$aov.tab[1,4])
rar.adpa.p <- c(rar.adpa.p,rar.ad$aov.tab[1,6])
rar.bd <- permutest(betadisper(vegdist(rar.datpa,'euclidean'),fac2[!(apply(drar,sum,MARGIN=1)==0) ]))
rar.bdpa.F <- c(rar.bdpa.F,rar.bd[[1]][1,4])
rar.bdpa.p <- c(rar.bdpa.p,rar.bd[[1]][1,6])
} #end if clause

} #end for clause

if(nboot>1){
res <- data.frame(names,
ad.F = round(ad.F,2),
ad.p,
rar.ad.F = round(rar.ad.F,2),
rar.ad.p= rar.ad.p,
bd.F=round(bd.F,2),
bd.p = bd.p,
rar.bd.F=round(rar.bd.F,2),
rar.bd.p,
adpa.F=round(adpa.F,2),
adpa.p,
rar.adpa.F=round(rar.adpa.F,2),
rar.adpa.p,
bdpa.F=round(bdpa.F,2),
bdpa.p,
rar.bdpa.F=round(bdpa.F,2),
rar.bdpa.p)
}else{
res <- data.frame(names,
ad.F = round(ad.F,2),
ad.p,
bd.F=round(bd.F,2),
bd.p = bd.p,
adpa.F=round(adpa.F,2),
adpa.p,
bdpa.F=round(bdpa.F,2),
bdpa.p)
}

return(data.frame(res))
}


