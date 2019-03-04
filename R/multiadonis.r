
datasets <- list('meio'=meio,'crust'=crust,'NeHaCyMi'=NeHaCyMi, 'worms'=worms)

multiadonis <- function(datasets, fac, ...   ){

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

dat <- t(datasets[[i]])
dat <- decostand(log(dat+1),method='hellinger')
dat2 <- dat[!(apply(dat,sum,MARGIN=1)==0),]
ad <- adonis(dat2 ~ fac[!(apply(dat,sum,MARGIN=1)==0) ])
ad.F <- c(ad.F,ad$aov.tab[1,4])
ad.p <- c(ad.p,ad$aov.tab[1,6])
bd <- permutest(betadisper(vegdist(dat2),fac[!(apply(dat,sum,MARGIN=1)==0) ]))
bd.F <- c(bd.F,bd[[1]][1,4])
bd.p <- c(bd.p,bd[[1]][1,6])

#now pa
datpa <- decostand(dat2,'pa')
ad <- adonis(datpa ~ fac[!(apply(dat,sum,MARGIN=1)==0) ],distance='euclidean')
adpa.F <- c(adpa.F,ad$aov.tab[1,4])
adpa.p <- c(adpa.p,ad$aov.tab[1,6])
bd <- permutest(betadisper(vegdist(datpa,'euclidean'),fac[!(apply(dat,sum,MARGIN=1)==0) ]))
bdpa.F <- c(bdpa.F,bd[[1]][1,4])
bdpa.p <- c(bdpa.p,bd[[1]][1,6])
}

res <- data.frame(names,ad.F = round(ad.F,2),ad.p= ad.p,bd.F=round(bd.F,2),
					bd.p,adpa.F=round(adpa.F,2),adpa.p,bdpa.F=round(bdpa.F,2),bdpa.p)
return(data.frame(res))
}

#example:
datasets <- list('meio'=meio[,9:27],'crust'=crust[,9:27],'NeHaCyMi'=NeHaCyMi[,9:27], 'worms'=worms[,9:27])
multiadonis(datasets,area)