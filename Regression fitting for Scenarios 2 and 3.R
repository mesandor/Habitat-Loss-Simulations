######## For scenario 2 ######## 

#Load the Scenario 2 Workspace

#Reduce the dataset to just one area step per iteration - but first put everything back into matrices
OrphanedFULLmat <- matrix(OrphanedFULL, nr=18410, nc=12)
AreaLostmat <- matrix(AreaLostTot, nr=18410, nc=12)
AreaRemainmat <- mat.or.vec(nr=18410, nc=12)
AreaRemainmat[1,] <- c(6400-4800, 6400-4800-1200, 6400-4800-1200-300, 6400-4800-1200-300-75, 6400-4800-1200-300-75-18.75, 6400-4800-1200-300-75-18.75-4.6875) 
for(i in 2:18410){
  AreaRemainmat[i,] <- AreaRemainmat[1,]
}
StartConnectmat <- matrix(StartConnectTot, nr=18410, nc=12)
StepConnectmat <- matrix(StepConnectTot, nr=18410, nc=12)
TotalSppmat <- matrix(TotalSppTot, nr=18410, nc=12) 
zDiffmat <- matrix(zDiffTot, nr=18410, nc=12)
reduce <- sample(1:12, 18410, replace=TRUE)


OrphanedFULLRed <- mat.or.vec(nr=18410, nc=1)
AreaLostRed <- mat.or.vec(nr=18410, nc=1)
AreaRemainRed <- mat.or.vec(nr=18410, nc=1)
StartConnectRed <- mat.or.vec(nr=18410, nc=1)
StepConnectRed <- mat.or.vec(nr=18410, nc=1)
TotalSppRed <- mat.or.vec(nr=18410, nc=1)
zDiffRed <- mat.or.vec(nr=18410, nc=1)
for(i in 1:18410){
  OrphanedFULLRed[i] <- OrphanedFULLmat[i,reduce[i]]
  AreaLostRed[i] <- AreaLostmat[i,reduce[i]]
  AreaRemainRed[i] <- AreaRemainmat[i,reduce[i]]
  StartConnectRed[i] <- StartConnectmat[i,reduce[i]]
  StepConnectRed[i] <- StepConnectmat[i,reduce[i]]
  TotalSppRed[i] <- TotalSppmat[i,reduce[i]]
  zDiffRed[i] <- zDiffmat[i,reduce[i]]
}

#Setup for Bernoulli analysis
N <- 18410
OrphanedYNFULL <- OrphanedFULLRed
for(i in 1:N){
  if(OrphanedYNFULL[i] >1){
    OrphanedYNFULL[i] <- 1
  }
}
OrphanedTot2 <- OrphanedYNFULL
AreaLostTot2 <- AreaLostRed
AreaRemainTot2 <- AreaRemainRed
StartConnectTot2 <- StartConnectRed
zDiffTot2 <- zDiffRed
TotalSppTot2 <- TotalSppRed
M <- length(OrphanedTot2)


#center (standardize) zDiff
muzD <- mean(zDiffTot2)
sdzD <- sd(zDiffTot2)
zDiffTot2Cent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- zDiffTot2[i]-muzD
  zDiffTot2Cent[i] <- m[i]/sdzD
}

#center (standardize) StartConnect
muSC <- mean(StartConnectTot2)
sdSC <- sd(StartConnectTot2)
StartConnectTot2Cent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- StartConnectTot2[i]-muSC
  StartConnectTot2Cent[i] <- m[i]/sdSC
}

#center (standardize) AreaLost
muAL <- mean(AreaLostTot2)
sdAL <- sd(AreaLostTot2)
AreaLostTot2Cent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- AreaLostTot2[i]-muAL
  AreaLostTot2Cent[i] <- m[i]/sdAL
}

#center (standardize) AreaRemain
muAR <- mean(AreaRemainTot2)
sdAR <- sd(AreaRemainTot2)
AreaRemainTot2Cent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- AreaRemainTot2[i]-muAR
  AreaRemainTot2Cent[i] <- m[i]/sdAR
}

#center (standardize) TotalSpp
muTS <- mean(TotalSppTot2)
sdTS <- sd(TotalSppTot2)
TotalSppTot2Cent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- TotalSppTot2[i]-muTS
  TotalSppTot2Cent[i] <- m[i]/sdTS
}

#Setup for Poisson analysis

OrphanedPois <- OrphanedFULLRed[which(OrphanedFULLRed>0)]
AreaLostPois <- AreaLostRed[which(OrphanedFULLRed>0)]
AreaRemainPois <- AreaRemainRed[which(OrphanedFULLRed>0)]
StepConnectPois <- StepConnectRed[which(OrphanedFULLRed>0)]
TotalSppPois <- TotalSppRed[which(OrphanedFULLRed>0)]
zDiffPois <- zDiffRed[which(OrphanedFULLRed>0)]

OrphanedNZ2 <- OrphanedPois
AreaLostNZ2 <- AreaLostPois
AreaRemainNZ2 <- AreaRemainPois
StepConnectNZ2 <- StepConnectPois
TotalSppNZ2 <- TotalSppPois
zDiffNZ2 <- zDiffPois

P <- length(OrphanedNZ2)


#center (standardize) zDiff
muzD <- mean(zDiffNZ2)
sdzD <- sd(zDiffNZ2)
zDiffNZ2Cent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- zDiffNZ2[i]-muzD
  zDiffNZ2Cent[i] <- m[i]/sdzD
}

#center (standardize) StepConnect
muSC <- mean(StepConnectNZ2)
sdSC <- sd(StepConnectNZ2)
StepConnectNZ2Cent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- StepConnectNZ2[i]-muSC
  StepConnectNZ2Cent[i] <- m[i]/sdSC
}

#center (standardize) AreaLost
muAL <- mean(AreaLostNZ2)
sdAL <- sd(AreaLostNZ2)
AreaLostNZ2Cent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- AreaLostNZ2[i]-muAL
  AreaLostNZ2Cent[i] <- m[i]/sdAL
}

#center (standardize) AreaRemain
muAR <- mean(AreaRemainNZ2)
sdAR <- sd(AreaRemainNZ2)
AreaRemainNZ2Cent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- AreaRemainNZ2[i]-muAR
  AreaRemainNZ2Cent[i] <- m[i]/sdAR
}


#center (standardize) TotalSpp
muTS <- mean(TotalSppNZ2)
sdTS <- sd(TotalSppNZ2)
TotalSppNZ2Cent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- TotalSppNZ2[i]-muTS
  TotalSppNZ2Cent[i] <- m[i]/sdTS
}


#JAGS!
library(R2jags)
library(R2WinBUGS)

#Bernoulli analysis
y <- OrphanedTot2

#start the model here
#start the clock
ptm <- proc.time()
Pred <- function(){
  
  #priors
  a0 ~ dnorm(0, 0.001)
  a1 ~ dnorm(0, 0.001)
  a2 ~ dnorm(0, 0.001)
  a3 ~ dnorm(0, 0.001)
  a4 ~ dnorm(0, 0.001)
  
  
  for(i in 1:M){
    y[i] ~ dbern(lambda[i])
    logit(lambda[i]) <- a0 + a1*zDiffTot2Cent[i] + a2*StartConnectTot2Cent[i] + a3*AreaRemainTot2Cent[i] + a4*TotalSppTot2Cent[i]
  }
}


if(is.R()){
  filename <- file.path(tempdir(), "Pred.bug")}
write.model(Pred, filename)

inits <- list(list(a0=-1, a1=-1, a2=-1, a3=-1, a4=-1), list(a0=0, a1=0, a2=0, a3=0, a4=0), list(a0=1, a1=1, a2=1, a3=1, a4=1))
data <- list("y", "zDiffTot2Cent", "StartConnectTot2Cent", "AreaRemainTot2Cent", "TotalSppTot2Cent", "M") 
parameters <- c("a0", "a1", "a2", "a3", "a4") 
Pred <- jags(data=data, inits = inits, parameters.to.save=parameters, filename, n.burnin=75000, n.iter=100000, n.thin=1, n.chains=3) #burnin=75000, iter=100000
# Stop the clock
proc.time() - ptm 

print(Pred) #check Rhat

PredBern.mcmc <- as.mcmc(Pred)
summary(PredBern.mcmc)

Pred2Bernmat <- as.matrix(PredBern.mcmc[[3]])

#check convergence
traceplot(Pred) 


#Poisson analysis
y <- OrphanedNZ2
P <- length(OrphanedNZ2)

#start the model here
#start the clock
ptm <- proc.time()
Pred <- function(){
  
  #priors
  a0 ~ dnorm(0, 0.001)
  a1 ~ dnorm(0, 0.001)
  a2 ~ dnorm(0, 0.001)
  a3 ~ dnorm(0, 0.001)
  a4 ~ dnorm(0, 0.001)
  #a5 ~ dnorm(0, 0.001)
  
  for(i in 1:P){
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- a0 + a1*zDiffNZ2Cent[i] + a2*StepConnectNZ2Cent[i] + a3*AreaRemainNZ2Cent[i] + a4*TotalSppNZ2Cent[i] 
    #+ a5*zDiffNZ[i]*StepConnectNZ[i] #interaction term keeps overlapping zero - maybe taking it out will help everything converge
  }
}


if(is.R()){
  filename <- file.path(tempdir(), "Pred.bug")}
write.model(Pred, filename)

inits <- list(list(a0=-1, a1=-1, a2=-1, a3=-1, a4=-1), list(a0=0, a1=0, a2=0, a3=0, a4=0), list(a0=1, a1=1, a2=1, a3=1, a4=1)) #, a5=-1, a5=0, a5=1
data <- list("y", "zDiffNZ2Cent", "StepConnectNZ2Cent", "AreaRemainNZ2Cent", "TotalSppNZ2Cent", "P") 
parameters <- c("a0", "a1", "a2", "a3", "a4") #, "a5"
Pred <- jags(data=data, inits = inits, parameters.to.save=parameters, filename, n.burnin=75000, n.iter=100000, n.thin=1, n.chains=3) 
# Stop the clock
proc.time() - ptm 

print(Pred) #check Rhat

Pred.mcmc <- as.mcmc(Pred)
summary(Pred.mcmc)

#check convergence
traceplot(Pred) 

######## For scenario 3 ######## 

#Load the Scenario 3 Workspace

#Reduce the dataset to just one area step per iteration - first need to get everything back into matrices
#400 has 3 columns
#100 has 15 columns
#25 has 63 columns
#16 has 99 columns
#6 has 255 columns
OrphanedP6mat <- matrix(OrphanedP6, nr=1814, nc=255) 
OrphanedA6mat <- matrix(OrphanedA6, nr=1814, nc=255)
Orphaned6mat <- cbind(OrphanedP6mat, OrphanedA6mat)
AreaLost6mat <- matrix(AreaLost6[1:462570], nr=1814, nc=255)
AreaLostBoth6mat <- cbind(AreaLost6mat, AreaLost6mat)
StartConnect6mat <- matrix(StartConnect6, nr=1814, nc=255)
StartConnectBoth6mat <- cbind(StartConnect6mat, StartConnect6mat)
StepConnect6mat <- matrix(StepConnect6, nr=1814, nc=255)
StepConnectBoth6mat <- cbind(StepConnect6mat, StepConnect6mat)
TotalSpp6mat <- matrix(TotalSpp6, nr=1814, nc=255)
TotalSppBoth6mat <- cbind(TotalSpp6mat, TotalSpp6mat)
zDiffP6mat <- matrix(zDiffP6, nr=1814, nc=255)
zDiffA6mat <- matrix(zDiffA6, nr=1814, nc=255)
zDiff6mat <- cbind(zDiffP6mat, zDiffA6mat)
reduce <- sample(1:510, 1814, replace=TRUE)
Orphaned6Red <- mat.or.vec(nr=1814, nc=1)
AreaLost6Red <- mat.or.vec(nr=1814, nc=1)
StartConnect6Red <- mat.or.vec(nr=1814, nc=1)
StepConnect6Red <- mat.or.vec(nr=1814, nc=1)
TotalSpp6Red <- mat.or.vec(nr=1814, nc=1)
zDiff6Red <- mat.or.vec(nr=1814, nc=1)
for(i in 1:1814){
  Orphaned6Red[i] <- Orphaned6mat[i,reduce[i]]
  AreaLost6Red[i] <- AreaLostBoth6mat[i,reduce[i]]
  StartConnect6Red[i] <- StartConnectBoth6mat[i,reduce[i]]
  StepConnect6Red[i] <- StepConnectBoth6mat[i,reduce[i]]
  TotalSpp6Red[i] <- TotalSppBoth6mat[i,reduce[i]]
  zDiff6Red[i] <- zDiff6mat[i,reduce[i]]
}

OrphanedP16mat <- matrix(OrphanedP16, nr=1825, nc=99)
OrphanedA16mat <- matrix(OrphanedA16, nr=1825, nc=99)
Orphaned16mat <- cbind(OrphanedP16mat, OrphanedA16mat)
AreaLost16mat <- matrix(AreaLost16[1:180675], nr=1825, nc=99) 
AreaLostBoth16mat <- cbind(AreaLost16mat, AreaLost16mat)
StartConnect16mat <- matrix(StartConnect16, nr=1825, nc=99)
StartConnectBoth16mat <- cbind(StartConnect16mat, StartConnect16mat)
StepConnect16mat <- matrix(StepConnect16, nr=1825, nc=99)
StepConnectBoth16mat <- cbind(StepConnect16mat, StepConnect16mat)
TotalSpp16mat <- matrix(TotalSpp16, nr=1825, nc=99)
TotalSppBoth16mat <- cbind(TotalSpp16mat, TotalSpp16mat)
zDiffP16mat <- matrix(zDiffP16, nr=1825, nc=99)
zDiffA16mat <- matrix(zDiffA16, nr=1825, nc=99)
zDiff16mat <- cbind(zDiffP16mat, zDiffA16mat)
reduce <- sample(1:198, 1825, replace=TRUE)
Orphaned16Red <- mat.or.vec(nr=1825, nc=1)
AreaLost16Red <- mat.or.vec(nr=1825, nc=1)
StartConnect16Red <- mat.or.vec(nr=1825, nc=1)
StepConnect16Red <- mat.or.vec(nr=1825, nc=1)
TotalSpp16Red <- mat.or.vec(nr=1825, nc=1)
zDiff16Red <- mat.or.vec(nr=1825, nc=1)
for(i in 1:1825){
  Orphaned16Red[i] <- Orphaned16mat[i,reduce[i]]
  AreaLost16Red[i] <- AreaLostBoth16mat[i,reduce[i]]
  StartConnect16Red[i] <- StartConnectBoth16mat[i,reduce[i]]
  StepConnect16Red[i] <- StepConnectBoth16mat[i,reduce[i]]
  TotalSpp16Red[i] <- TotalSppBoth16mat[i,reduce[i]]
  zDiff16Red[i] <- zDiff16mat[i,reduce[i]]
}

OrphanedP25mat <- matrix(OrphanedP25, nr=1816, nc=63)
OrphanedA25mat <- matrix(OrphanedA25, nr=1816, nc=63)
Orphaned25mat <- cbind(OrphanedP25mat, OrphanedA25mat)
AreaLost25mat <- matrix(AreaLost25[1:114408], nr=1816, nc=63) 
AreaLostBoth25mat <- cbind(AreaLost25mat, AreaLost25mat)
StartConnect25mat <- matrix(StartConnect25, nr=1816, nc=63)
StartConnectBoth25mat <- cbind(StartConnect25mat, StartConnect25mat)
StepConnect25mat <- matrix(StepConnect25, nr=1816, nc=63)
StepConnectBoth25mat <- cbind(StepConnect25mat, StepConnect25mat)
TotalSpp25mat <- matrix(TotalSpp25, nr=1816, nc=63)
TotalSppBoth25mat <- cbind(TotalSpp25mat, TotalSpp25mat)
zDiffP25mat <- matrix(zDiffP25, nr=1816, nc=63)
zDiffA25mat <- matrix(zDiffA25, nr=1816, nc=63)
zDiff25mat <- cbind(zDiffP25mat, zDiffA25mat)
reduce <- sample(1:126, 1816, replace=TRUE)
Orphaned25Red <- mat.or.vec(nr=1816, nc=1)
AreaLost25Red <- mat.or.vec(nr=1816, nc=1)
StartConnect25Red <- mat.or.vec(nr=1816, nc=1)
StepConnect25Red <- mat.or.vec(nr=1816, nc=1)
TotalSpp25Red <- mat.or.vec(nr=1816, nc=1)
zDiff25Red <- mat.or.vec(nr=1816, nc=1)
for(i in 1:1816){
  Orphaned25Red[i] <- Orphaned25mat[i,reduce[i]]
  AreaLost25Red[i] <- AreaLostBoth25mat[i,reduce[i]]
  StartConnect25Red[i] <- StartConnectBoth25mat[i,reduce[i]]
  StepConnect25Red[i] <- StepConnectBoth25mat[i,reduce[i]]
  TotalSpp25Red[i] <- TotalSppBoth25mat[i,reduce[i]]
  zDiff25Red[i] <- zDiff25mat[i,reduce[i]]
}

OrphanedP100mat <- matrix(OrphanedP100, nr=1817, nc=15)
OrphanedA100mat <- matrix(OrphanedA100, nr=1817, nc=15)
Orphaned100mat <- cbind(OrphanedP100mat, OrphanedA100mat)
AreaLost100mat <- matrix(AreaLost100[1:27255], nr=1817, nc=15) 
AreaLostBoth100mat <- cbind(AreaLost100mat, AreaLost100mat)
StartConnect100mat <- matrix(StartConnect100, nr=1817, nc=15)
StartConnectBoth100mat <- cbind(StartConnect100mat, StartConnect100mat)
StepConnect100mat <- matrix(StepConnect100, nr=1817, nc=15)
StepConnectBoth100mat <- cbind(StepConnect100mat, StepConnect100mat)
TotalSpp100mat <- matrix(TotalSpp100, nr=1817, nc=15)
TotalSppBoth100mat <- cbind(TotalSpp100mat, TotalSpp100mat)
zDiffP100mat <- matrix(zDiffP100, nr=1817, nc=15)
zDiffA100mat <- matrix(zDiffA100, nr=1817, nc=15)
zDiff100mat <- cbind(zDiffP100mat, zDiffA100mat)
reduce <- sample(1:30, 1817, replace=TRUE)
Orphaned100Red <- mat.or.vec(nr=1817, nc=1)
AreaLost100Red <- mat.or.vec(nr=1817, nc=1)
StartConnect100Red <- mat.or.vec(nr=1817, nc=1)
StepConnect100Red <- mat.or.vec(nr=1817, nc=1)
TotalSpp100Red <- mat.or.vec(nr=1817, nc=1)
zDiff100Red <- mat.or.vec(nr=1817, nc=1)
for(i in 1:1817){
  Orphaned100Red[i] <- Orphaned100mat[i,reduce[i]]
  AreaLost100Red[i] <- AreaLostBoth100mat[i,reduce[i]]
  StartConnect100Red[i] <- StartConnectBoth100mat[i,reduce[i]]
  StepConnect100Red[i] <- StepConnectBoth100mat[i,reduce[i]]
  TotalSpp100Red[i] <- TotalSppBoth100mat[i,reduce[i]]
  zDiff100Red[i] <- zDiff100mat[i,reduce[i]]
}

OrphanedP400mat <- matrix(OrphanedP400, nr=1814, nc=3)
OrphanedA400mat <- matrix(OrphanedA400, nr=1814, nc=3)
Orphaned400mat <- cbind(OrphanedP400mat, OrphanedA400mat)
AreaLost400mat <- matrix(AreaLost400[1:5442], nr=1814, nc=3) 
AreaLostBoth400mat <- cbind(AreaLost400mat, AreaLost400mat)
StartConnect400mat <- matrix(StartConnect400, nr=1814, nc=3)
StartConnectBoth400mat <- cbind(StartConnect400mat, StartConnect400mat)
StepConnect400mat <- matrix(StepConnect400, nr=1814, nc=3)
StepConnectBoth400mat <- cbind(StepConnect400mat, StepConnect400mat)
TotalSpp400mat <- matrix(TotalSpp400, nr=1814, nc=3)
TotalSppBoth400mat <- cbind(TotalSpp400mat, TotalSpp400mat)
zDiffP400mat <- matrix(zDiffP400, nr=1814, nc=3)
zDiffA400mat <- matrix(zDiffA400, nr=1814, nc=3)
zDiff400mat <- cbind(zDiffP400mat, zDiffA400mat)
reduce <- sample(1:6, 1814, replace=TRUE)
Orphaned400Red <- mat.or.vec(nr=1814, nc=1)
AreaLost400Red <- mat.or.vec(nr=1814, nc=1)
StartConnect400Red <- mat.or.vec(nr=1814, nc=1)
StepConnect400Red <- mat.or.vec(nr=1814, nc=1)
TotalSpp400Red <- mat.or.vec(nr=1814, nc=1)
zDiff400Red <- mat.or.vec(nr=1814, nc=1)
for(i in 1:1814){
  Orphaned400Red[i] <- Orphaned400mat[i,reduce[i]]
  AreaLost400Red[i] <- AreaLostBoth400mat[i,reduce[i]]
  StartConnect400Red[i] <- StartConnectBoth400mat[i,reduce[i]]
  StepConnect400Red[i] <- StepConnectBoth400mat[i,reduce[i]]
  TotalSpp400Red[i] <- TotalSppBoth400mat[i,reduce[i]]
  zDiff400Red[i] <- zDiff400mat[i,reduce[i]]
}

#Put all of the datasets together 
OrphanedALL <- c(Orphaned6Red, Orphaned16Red, Orphaned25Red, Orphaned100Red, Orphaned400Red)
AreaLostALL <- c(AreaLost6Red, AreaLost16Red, AreaLost25Red, AreaLost100Red, AreaLost400Red)
StartConnectALL <- c(StartConnect6Red, StartConnect16Red, StartConnect25Red, StartConnect100Red, StartConnect400Red)
StepConnectALL <- c(StepConnect6Red, StepConnect16Red, StepConnect25Red, StepConnect100Red, StepConnect400Red)
TotalSppALL <- c(TotalSpp6Red, TotalSpp16Red, TotalSpp25Red, TotalSpp100Red, TotalSpp400Red)
zDiffALL <- c(zDiff6Red, zDiff16Red, zDiff25Red, zDiff100Red, zDiff400Red)
N <- length(OrphanedALL)

#Setup for Bernoulli analysis
OrphanedYNALL <- OrphanedALL
for(i in 1:N){
  if(OrphanedYNALL[i] >1){
    OrphanedYNALL[i] <- 1
  }
}
OrphanedTot <- OrphanedYNALL
AreaLostTot <- AreaLostALL
StartConnectTot <- StartConnectALL
zDiffTot <- zDiffALL
TotalSppTot <- TotalSppALL
M <- length(OrphanedTot)


#center (standardize) zDiff
muzD <- mean(zDiffTot)
sdzD <- sd(zDiffTot)
zDiffTotCent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- zDiffTot[i]-muzD
  zDiffTotCent[i] <- m[i]/sdzD
}

#center (standardize) StartConnect
muSC <- mean(StartConnectTot)
sdSC <- sd(StartConnectTot)
StartConnectTotCent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- StartConnectTot[i]-muSC
  StartConnectTotCent[i] <- m[i]/sdSC
}

#center (standardize) AreaLost
muAL <- mean(AreaLostTot)
sdAL <- sd(AreaLostTot)
AreaLostTotCent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- AreaLostTot[i]-muAL
  AreaLostTotCent[i] <- m[i]/sdAL
}

#center (standardize) TotalSpp
muTS <- mean(TotalSppTot)
sdTS <- sd(TotalSppTot)
TotalSppTotCent <- mat.or.vec(nr=M, nc=1)
m <- mat.or.vec(nr=M, nc=1) 
for(i in 1:M){
  m[i] <- TotalSppTot[i]-muTS
  TotalSppTotCent[i] <- m[i]/sdTS
}

#Setup for Poisson analysis 

OrphanedPois <- OrphanedALL[which(OrphanedALL>0)]
AreaLostPois <- AreaLostALL[which(OrphanedALL>0)]
StepConnectPois <- StepConnectALL[which(OrphanedALL>0)]
TotalSppPois <- TotalSppALL[which(OrphanedALL>0)]
zDiffPois <- zDiffALL[which(OrphanedALL>0)]

OrphanedNZ <- OrphanedPois
AreaLostNZ <- AreaLostPois
StepConnectNZ <- StepConnectPois
TotalSppNZ <- TotalSppPois
zDiffNZ <- zDiffPois

P <- length(OrphanedNZ)



#center (standardize) zDiff
muzD <- mean(zDiffNZ)
sdzD <- sd(zDiffNZ)
zDiffNZCent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- zDiffNZ[i]-muzD
  zDiffNZCent[i] <- m[i]/sdzD
}

#center (standardize) StepConnect
muSC <- mean(StepConnectNZ)
sdSC <- sd(StepConnectNZ)
StepConnectNZCent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- StepConnectNZ[i]-muSC
  StepConnectNZCent[i] <- m[i]/sdSC
}

#center (standardize) AreaLost
muAL <- mean(AreaLostNZ)
sdAL <- sd(AreaLostNZ)
AreaLostNZCent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- AreaLostNZ[i]-muAL
  AreaLostNZCent[i] <- m[i]/sdAL
}

#center (standardize) TotalSpp
muTS <- mean(TotalSppNZ)
sdTS <- sd(TotalSppNZ)
TotalSppNZCent <- mat.or.vec(nr=P, nc=1)
m <- mat.or.vec(nr=P, nc=1) 
for(i in 1:P){
  m[i] <- TotalSppNZ[i]-muTS
  TotalSppNZCent[i] <- m[i]/sdTS
}


#JAGS!
library(R2jags)
library(R2WinBUGS)

#Bernoulli analysis
y <- OrphanedTot

#start the model here
#start the clock
ptm <- proc.time()
Pred <- function(){
  
  #priors
  a0 ~ dnorm(0, 0.001)
  a1 ~ dnorm(0, 0.001)
  a2 ~ dnorm(0, 0.001)
  a3 ~ dnorm(0, 0.001)
  a4 ~ dnorm(0, 0.001)
  
  
  for(i in 1:M){
    y[i] ~ dbern(lambda[i])
    logit(lambda[i]) <- a0 + a1*zDiffTotCent[i] + a2*StartConnectTotCent[i] + a3*AreaLostTotCent[i] + a4*TotalSppTotCent[i]
  }
}


if(is.R()){
  filename <- file.path(tempdir(), "Pred.bug")}
write.model(Pred, filename)

inits <- list(list(a0=-1, a1=-1, a2=-1, a3=-1, a4=-1), list(a0=0, a1=0, a2=0, a3=0, a4=0), list(a0=1, a1=1, a2=1, a3=1, a4=1))
data <- list("y", "zDiffTotCent", "StartConnectTotCent", "AreaLostTotCent", "TotalSppTotCent", "M") 
parameters <- c("a0", "a1", "a2", "a3", "a4") 
Pred <- jags(data=data, inits = inits, parameters.to.save=parameters, filename, n.burnin=125000, n.iter=150000, n.thin=1, n.chains=3) #burnin=75, iter=10
# Stop the clock
proc.time() - ptm 

print(Pred) #check Rhat

PredBern.mcmc <- as.mcmc(Pred)
summary(PredBern.mcmc)

PredBernmat <- as.matrix(PredBern.mcmc[[3]])

#check convergence
traceplot(Pred) 

#Poisson analysis
y <- OrphanedNZ
P <- length(OrphanedNZ)

#start the model here
#start the clock
ptm <- proc.time()
Pred <- function(){
  
  #priors
  a0 ~ dnorm(0, 0.001)
  a1 ~ dnorm(0, 0.001)
  a2 ~ dnorm(0, 0.001)
  a3 ~ dnorm(0, 0.001)
  a4 ~ dnorm(0, 0.001)
  #a5 ~ dnorm(0, 0.001)
  
  for(i in 1:P){
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- a0 + a1*zDiffNZCent[i] + a2*StepConnectNZCent[i] + a3*AreaLostNZCent[i] + a4*TotalSppNZCent[i] 
    #+ a5*zDiffNZ[i]*StepConnectNZ[i] #interaction term keeps overlapping zero - maybe taking it out will help everything converge
  }
}


if(is.R()){
  filename <- file.path(tempdir(), "Pred.bug")}
write.model(Pred, filename)

inits <- list(list(a0=-1, a1=-1, a2=-1, a3=-1, a4=-1), list(a0=0, a1=0, a2=0, a3=0, a4=0), list(a0=1, a1=1, a2=1, a3=1, a4=1)) #, a5=-1, a5=0, a5=1
data <- list("y", "zDiffNZCent", "StepConnectNZCent", "AreaLostNZCent", "TotalSppNZCent", "P") 
parameters <- c("a0", "a1", "a2", "a3", "a4") #, "a5"
Pred <- jags(data=data, inits = inits, parameters.to.save=parameters, filename, n.burnin=75000, n.iter=100000, n.thin=1, n.chains=3) 
# Stop the clock
proc.time() - ptm 

print(Pred) #check Rhat

Pred.mcmc <- as.mcmc(Pred)
summary(Pred.mcmc)

#check convergence
traceplot(Pred) 
