# load relevant packages
library(brms)
library(wesanderson)
library(rethinking)
library(geosphere)

## download Binford dataset from https://github.com/benmarwick/binford
# load csv with Binford dataset
LRB<- read.csv("LRB.csv")

# extract relevant variables and omit NA's (two populations don't have storage data)
B<- na.omit(LRB[,c("class", "store", "owners", "huntfil2", "hunting", "tlpop", "wlocation", "long", "lati", "society")])

# re-code class to define egalitarianism as class = 1
B$egal<- B$class
B$egal[B$egal>1]<- 0

# recode storage
B$store[B$store>2]<- 2

# recode owners
B$owners[B$owners>2]<- 2

# center log tlpop
B$tlpop.c<- log(B$tlpop)-mean(log(B$tlpop))

# recode hunting as proportion and standardize
B$hunting.z<- scale(B$hunting/100)

## generate distance matrix 
dist<- distm(cbind(B$long, B$lati))
dist.scale<- 1-dist/max(dist)
rownames(dist.scale)<- B$society
colnames(dist.scale)<- B$society

# note that brms might produce an error saying that the covariance matrix is not positive definite
# in order for the matrix to be positive definite, all eigenvalues have to be >0
# in this case, the smallest eigenvalue is 8.6 e -18, which might be too close to 0
# a minor workaround fixes this problem without any change in estimates or inference:
library(Matrix)
dist.scale2<- nearPD(dist.scale, keepDiag=TRUE)$mat # make sure diagonal is still all 1's, as required for a phylogenetic covariance matrix


### run phylogenetic model ###
m1<- brm(egal~huntfil2+store+owners+hunting.z+tlpop.c+(1|wlocation)+(1|gr(society, cov = dist.scale2)), 
	data=B, family=bernoulli(), data2=list(dist.scale2=dist.scale2),
	prior=c(prior(normal(0,10), class=Intercept), prior(cauchy(0,2), class=sd), prior(normal(0,1), class=b)),
	chains = 4, cores = 4, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99, max_treedepth = 15))
plot(m1); summary(m1) # good convergence


# extract samples
post_m1<-posterior_samples(m1) 


## phylogenetic signal / icc
VarPhylo<- post_m1$sd_society__Intercept
VarLoc<- post_m1$sd_wlocation__Intercept
VarDistro<- pi^2 / 3 # distribution-specific variance for binary data, see Nakagawa & Schielzeth 2013 MEE

lambda<- VarPhylo/(VarPhylo+VarLoc+VarDistro) # calculate pagel's lambda, the phylogenetic signal (or ICC of the phylogenetic random effect)
mean(lambda); median(lambda); HPDI(lambda, prob=0.95)

icc.loc<- VarLoc/(VarPhylo+VarLoc+VarDistro) # calculate the ICC of the location random effect
mean(icc.loc); median(icc.loc); HPDI(icc.loc, prob=0.95)


# calculate odd's ratios
exp(mean(post_m1[,2]))
exp(mean(post_m1[,3]))
exp(mean(post_m1[,4]))
exp(mean(post_m1[,5]))
exp(mean(post_m1[,6]))


# predicted probability of egalitarianism as function of hunting, with all other predictors at baseline level (=1)
hunting.z<- seq(min(B$hunting.z), max(B$hunting.z), length=100)
newdata<- as.data.frame(hunting.z)
newdata$huntfil2<- 1
newdata$store<- 1
newdata$owners<- 1
newdata$tlpop.c<- 0
pred.1<- fitted(m1, newdata, re_formula=NA, summary=FALSE)

# predicted probability of egalitarianism as function of hunting, with all other predictors at high level (=2)
hunting.z<- seq(min(B$hunting.z), max(B$hunting.z), length=100)
newdata<- as.data.frame(hunting.z)
newdata$huntfil2<- 2
newdata$store<- 2
newdata$owners<- 2
newdata$tlpop.c<- 0
pred.2<- fitted(m1, newdata, re_formula=NA, summary=FALSE)

# predicted probability of egalitarianism as function of hunting.z, with all other predictors at intermediate level (=1.5)
hunting.z<- seq(min(B$hunting.z), max(B$hunting.z), length=100)
newdata<- as.data.frame(hunting.z)
newdata$huntfil2<- 1.5
newdata$store<- 1.5
newdata$owners<- 1.5
newdata$tlpop.c<- 0
pred.1.5<- fitted(m1, newdata, re_formula=NA, summary=FALSE)


pdf("Figure 4.pdf")
layout(matrix(1:3,ncol=3))
par(mar=c(5,2,4,1))

plot(B$egal~B$hunting.z, col="white", xlab="", ylab="", yaxt="n", xaxt="n", main="Low Defensibility")
axis(1, at=c(-1.652944,-0.407600453, 0.837742726, 2.083085905), labels=c("0", "25", "50", "75"))
axis(2, at=c(0,1), labels=c("No", "Yes"))
mtext("Egalitarianism", side = 2, line = 1, cex=0.8)
shade(apply(pred.1, 2, PI, 0.90), hunting.z, col=col.alpha(wes_palettes$Zissou[5], 0.18))
shade(apply(pred.1, 2, PI, 0.72), hunting.z, col=col.alpha(wes_palettes$Zissou[5], 0.18))
shade(apply(pred.1, 2, PI, 0.54), hunting.z, col=col.alpha(wes_palettes$Zissou[5], 0.18))
shade(apply(pred.1, 2, PI, 0.36), hunting.z, col=col.alpha(wes_palettes$Zissou[5], 0.18))
shade(apply(pred.1, 2, PI, 0.18), hunting.z, col=col.alpha(wes_palettes$Zissou[5], 0.18))
lines(x=hunting.z, y=apply(pred.1,2,median), col=wes_palettes$Zissou[5], lwd=3)

plot(B$egal~B$hunting.z, col="white", xlab="Dependence on hunting [%]", ylab="", yaxt="n", xaxt="n", main="Intermediate Defensibility")
axis(1, at=c(-1.652944,-0.407600453, 0.837742726, 2.083085905), labels=c("0", "25", "50", "75"))
axis(2, at=c(0,1), labels=c("", ""))
shade(apply(pred.1.5, 2, PI, 0.90), hunting.z, col=col.alpha(wes_palettes$Zissou[4], 0.18))
shade(apply(pred.1.5, 2, PI, 0.72), hunting.z, col=col.alpha(wes_palettes$Zissou[4], 0.18))
shade(apply(pred.1.5, 2, PI, 0.54), hunting.z, col=col.alpha(wes_palettes$Zissou[4], 0.18))
shade(apply(pred.1.5, 2, PI, 0.36), hunting.z, col=col.alpha(wes_palettes$Zissou[4], 0.18))
shade(apply(pred.1.5, 2, PI, 0.18), hunting.z, col=col.alpha(wes_palettes$Zissou[4], 0.18))
lines(x=hunting.z, y=apply(pred.1.5,2,median), col=wes_palettes$Zissou[4], lwd=3)

plot(B$egal~B$hunting.z, col="white", xlab="", ylab="", yaxt="n", xaxt="n", main="High Defensibility")
axis(1, at=c(-1.652944,-0.407600453, 0.837742726, 2.083085905), labels=c("0", "25", "50", "75"))
axis(2, at=c(0,1), labels=c("", ""))
shade(apply(pred.2, 2, PI, 0.90), hunting.z, col=col.alpha(wes_palettes$Zissou[1], 0.18))
shade(apply(pred.2, 2, PI, 0.72), hunting.z, col=col.alpha(wes_palettes$Zissou[1], 0.18))
shade(apply(pred.2, 2, PI, 0.54), hunting.z, col=col.alpha(wes_palettes$Zissou[1], 0.18))
shade(apply(pred.2, 2, PI, 0.36), hunting.z, col=col.alpha(wes_palettes$Zissou[1], 0.18))
shade(apply(pred.2, 2, PI, 0.18), hunting.z, col=col.alpha(wes_palettes$Zissou[1], 0.18))
lines(x=hunting.z, y=apply(pred.2,2,median), col=wes_palettes$Zissou[1], lwd=3)

dev.off()

