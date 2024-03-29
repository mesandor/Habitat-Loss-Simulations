#Setup
#For use in the loops
runs <- 100
#connectance values
Conn <- c(rep(0.1, 21*runs), rep(0.2, 21*runs), rep(0.3, 21*runs), rep(0.4, 21*runs), rep(0.5, 21*runs), rep(0.6, 21*runs), rep(0.7, 21*runs), rep(0.8, 21*runs), rep(0.9, 21*runs)) 
#z-difference values
zdiff <- rep(c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2), 9*runs)
zindex <- rep(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21), 9*runs)

z <- list()
#each list item is a difference that corresponds to zdiff, each column is a possible combination
z[[1]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35))
z[[2]] <- rbind(c(0.15, 0.16, 0.16, 0.17, 0.17, 0.18, 0.18, 0.19, 0.19, 0.20, 0.20, 0.21, 0.21, 0.22, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 0.29, 0.29, 0.3, 0.3, 0.31, 0.31, 0.32, 0.32, 0.33, 0.33, 0.34, 0.34, 0.35), c(0.16, 0.15, 0.17, 0.16, 0.18, 0.17, 0.19, 0.18, 0.2, 0.19, 0.21, 0.2, 0.22, 0.21, 0.23, 0.22, 0.24, 0.23, 0.25, 0.24, 0.26, 0.25, 0.27, 0.26, 0.28, 0.27, 0.29, 0.28, 0.3, 0.29, 0.31, 0.3, 0.32, 0.31, 0.33, 0.32, 0.34, 0.33, 0.35, 0.34))
z[[3]] <- rbind(c(0.15, 0.16, 0.17, 0.17, 0.18, 0.18, 0.19, 0.19, 0.20, 0.20, 0.21, 0.21, 0.22, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 0.29, 0.29, 0.3, 0.3, 0.31, 0.31, 0.32, 0.32, 0.33, 0.33, 0.34, 0.35), c(0.17, 0.18, 0.15, 0.19, 0.16, 0.2, 0.17, 0.21, 0.18, 0.22, 0.19, 0.23, 0.2, 0.24, 0.21, 0.25, 0.22, 0.26, 0.23, 0.27, 0.24, 0.28, 0.25, 0.29, 0.26, 0.3, 0.27, 0.31, 0.28, 0.32, 0.29, 0.33, 0.3, 0.34, 0.31, 0.35, 0.32, 0.33))
z[[4]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.18, 0.19, 0.19, 0.20, 0.20, 0.21, 0.21, 0.22, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 0.29, 0.29, 0.3, 0.3, 0.31, 0.31, 0.32, 0.32, 0.33, 0.34, 0.35), c(0.18, 0.19, 0.2, 0.15, 0.21, 0.16, 0.22, 0.17, 0.23, 0.18, 0.24, 0.19, 0.25, 0.2, 0.26, 0.21, 0.27, 0.22, 0.28, 0.23, 0.29, 0.24, 0.3, 0.25, 0.31, 0.26, 0.32, 0.27, 0.33, 0.28, 0.34, 0.29, 0.35, 0.3, 0.31, 0.32))
z[[5]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.19, 0.20, 0.20, 0.21, 0.21, 0.22, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 0.29, 0.29, 0.3, 0.3, 0.31, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.19, 0.2, 0.21, 0.22, 0.15, 0.23, 0.16, 0.24, 0.17, 0.25, 0.18, 0.26, 0.19, 0.27, 0.2, 0.28, 0.21, 0.29, 0.22, 0.3, 0.23, 0.31, 0.24, 0.32, 0.25, 0.33, 0.26, 0.34, 0.27, 0.35, 0.28, 0.29, 0.3, 0.31))
z[[6]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.20, 0.21, 0.21, 0.22, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 0.29, 0.29, 0.3, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.2, 0.21, 0.22, 0.23, 0.24, 0.15, 0.25, 0.16, 0.26, 0.17, 0.27, 0.18, 0.28, 0.19, 0.29, 0.2, 0.3, 0.21, 0.31, 0.22, 0.32, 0.23, 0.33, 0.24, 0.34, 0.25, 0.35, 0.26, 0.27, 0.28, 0.29, 0.3))
z[[7]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.21, 0.22, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 0.29, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.15, 0.27, 0.16, 0.28, 0.17, 0.29, 0.18, 0.3, 0.19, 0.31, 0.2, 0.32, 0.21, 0.33, 0.22, 0.34, 0.23, 0.35, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29))
z[[8]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.15, 0.29, 0.16, 0.3, 0.17, 0.31, 0.18, 0.32, 0.19, 0.33, 0.2, 0.34, 0.21, 0.35, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28))
z[[9]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.15, 0.31, 0.16, 0.32, 0.17, 0.33, 0.18, 0.34, 0.19, 0.35, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27))
z[[10]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.24, 0.25, 0.25, 0.26, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.24, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.15, 0.33, 0.16, 0.34, 0.17, 0.35, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26))
z[[11]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.15, 0.35, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25))
z[[12]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24))
z[[13]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23))
z[[14]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22))
z[[15]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21))
z[[16]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2))
z[[17]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.19, 0.31, 0.32, 0.33, 0.34, 0.35), c(0.31, 0.32, 0.33, 0.34, 0.35, 0.15, 0.16, 0.17, 0.18, 0.19))
z[[18]] <- rbind(c(0.15, 0.16, 0.17, 0.18, 0.32, 0.33, 0.34, 0.35), c(0.32, 0.33, 0.34, 0.35, 0.15, 0.16, 0.17, 0.18))
z[[19]] <- rbind(c(0.15, 0.16, 0.17, 0.33, 0.34, 0.35), c(0.33, 0.34, 0.35, 0.15, 0.16, 0.17))
z[[20]] <- rbind(c(0.15, 0.16, 0.34, 0.35), c(0.34, 0.35, 0.15, 0.16))
z[[21]] <- rbind(c(0.15, 0.35), c(0.35, 0.15))


zlength <- c(length(z[[1]][1,]), length(z[[2]][1,]), length(z[[3]][1,]), length(z[[4]][1,]), length(z[[5]][1,]), length(z[[6]][1,]), length(z[[7]][1,]), length(z[[8]][1,]), length(z[[9]][1,]), length(z[[10]][1,]), length(z[[11]][1,]), length(z[[12]][1,]), length(z[[13]][1,]), length(z[[14]][1,]), length(z[[15]][1,]), length(z[[16]][1,]), length(z[[17]][1,]), length(z[[18]][1,]), length(z[[19]][1,]), length(z[[20]][1,]), length(z[[21]][1,]))

zwhich <- mat.or.vec(nr=1, nc=189*runs)
zcol <- mat.or.vec(nr=1, nc=189*runs)
zP <- mat.or.vec(nr=1, nc=189*runs)
zA <- mat.or.vec(nr=1, nc=189*runs)

M <- 7 #number of area loss steps

LogPlantSp <- mat.or.vec(nr=189*runs, nc=M)
LogAnimalSp <- mat.or.vec(nr=189*runs, nc=M)
PlantSp <- mat.or.vec(nr=189*runs, nc=M)
AnimalSp <- mat.or.vec(nr=189*runs, nc=M)

#For storing the end results
LogArea <- mat.or.vec(nr=189*runs, nc=M)
AreaLoss <- mat.or.vec(nr=189*runs, nc=M) #first column will always be 0, and this is in km2 (area size is in m2)
OrphanedP <- mat.or.vec(nr=189*runs, nc=M)
OrphanedA <- mat.or.vec(nr=189*runs, nc=M)
RemainP <- mat.or.vec(nr=189*runs, nc=M)
RemainA <- mat.or.vec(nr=189*runs, nc=M)
RemainN <- mat.or.vec(nr=189*runs, nc=M)
Connectance <- mat.or.vec(nr=189*runs, nc=M)

#Set c value - same for both "plants" and "animals", making an equal network possible
CPA <- -0.75

#Store area sizes
y <- c(6400000000, 1600000000, 400000000, 100000000, 25000000, 6250000, 1562500)
x <- 189*runs
AreaSize <- mat.or.vec(nr=x, nc=M)
for(i in 1:x){
  AreaSize[i,] <- y
}
LogArea <- log10(AreaSize)

N <- 189*runs
NetworkFull <- list()
#double lists are [[M]][[189*runs]] so they're actually [[j]][[i]]
Network <- rep(list(list()), N)
Network2 <- rep(list(list()), N)
A <- list()
Int <- mat.or.vec(nr=1, nc=189*runs)
Int2 <- mat.or.vec(nr=1, nc=189*runs)
AS1 <- list()
AS2 <- list()
AS3 <- list()
PlantInt <- list()
plants <- rep(list(list()), N)
animals <- rep(list(list()), N)
P <- list()
Anim <- list()
Plnt <- list()
Viable1 <- mat.or.vec(nr=1, nc=N)
Viable2 <- mat.or.vec(nr=1, nc=N)
Connect2 <- mat.or.vec(nr=1, nc=N)



# Start the clock!
ptm <- proc.time()
#Start the loop


for(i in 1:N){
  for(j in 2:M){
    AreaLoss[i,j] <- (AreaSize[i,j-1]-AreaSize[i,j])/1e06
  }
}

for(i in 1:N){
  #Pull zP
  zwhich[i] <- zlength[[zindex[i]]]
  zcol[i] <- sample(1:zwhich[i], 1) 
  zP[i] <- z[[zindex[i]]][1,zcol[i]]
  #Pull zA
  zA[i] <- z[[zindex[i]]][2,zcol[i]]
  
  #Calculate number of species from the species area curve
  LogPlantSp[i,] <- CPA + zP[i]*LogArea[i,] 
  PlantSp[i,] <- round(10^(LogPlantSp[i,]), digits=0)
  
  LogAnimalSp[i,] <- CPA + zA[i]*LogArea[i,]
  AnimalSp[i,] <- round(10^(LogAnimalSp[i,]), digits=0)
  
  #"Plants" in rows, "animals" in columns; populate the full network first
  NetworkFull[[i]] <- mat.or.vec(nr=PlantSp[i,1],nc=AnimalSp[i,1])
  Anim[[i]] <- c(1:AnimalSp[i,1]) #list of column numbers
  Int[i] <- ceiling(Conn[i]*AnimalSp[i,1]*PlantSp[i,1]) 
  
  PlantInt[[i]] <- round(abs(rnorm(PlantSp[i,1], Int[i]/PlantSp[i,1])), digits=0) #this gives a vector of the number of interactions with "animal" species, per "plant" species, drawn from a normal distribution
  for(k in 1:length(PlantInt[[i]])){
    if(PlantInt[[i]][k]==0){
      PlantInt[[i]][k] <- 1
    }
  }
  AS1[[i]] <- mat.or.vec(1,0)
  for(j in 1:PlantSp[i,1]){
    AS1[[i]] <- c(AS1[[i]], rep(j, PlantInt[[i]][j])) #this gives the row numbers for interactions
  }
  Int2[i] <- sum(PlantInt[[i]]) #this gives the revised number of total interactions
  AS2[[i]] <- mat.or.vec(1,0)
  if(length(Anim[[i]]) <= Int2[i]){
    AS2[[i]] <- sample(Anim[[i]], length(Anim[[i]]))
    if(length(Anim[[i]]) > (Int2[i]-AnimalSp[i,1])){
      AS2[[i]] <- c(AS2[[i]], sample(Anim[[i]], Int2[i]-AnimalSp[i,1])) #add in column numbers up to the number of interactions needed
    }
    if(length(Anim[[i]]) < (Int2[i]-AnimalSp[i,1])){
      AS3[[i]] <- rep(sample(Anim[[i]], length(Anim[[i]])), 1000)
      AS2[[i]] <- c(AS2[[i]], AS3[[i]][1:(Int2[i]-AnimalSp[i,1])])
    }
  }
  if(length(Anim[[i]]) > Int2[i]){
    AS2[[i]] <- sample(Anim[[i]], Int2[i])
  }
  p <- length(AS1[[i]])
  for(k in 1:p){
    NetworkFull[[i]][AS1[[i]][k],AS2[[i]][k]] <- 1 #replace those column numbers (in k row) with 1's
  }
  Network2[[i]] <- NetworkFull[[i]][which(apply(NetworkFull[[i]], 1, sum)>0),] #takes out all plant spp with no interactions
  Network2[[i]] <- NetworkFull[[i]][,which(apply(NetworkFull[[i]], 2, sum)>0)] #takes out all animal spp with no interactions
  P[[i]] <- c(1:PlantSp[i,1])
  A[[i]] <- c(1:AnimalSp[i,1])
  plants[[i]][[1]] <- P[[i]]
  animals[[i]][[1]] <- A[[i]]
  Network[[i]][[1]] <- NetworkFull[[i]]
  RemainP[i,1] <- PlantSp[i,1]
  RemainA[i,1] <- AnimalSp[i,1]
  RemainN[i,1] <- PlantSp[i,1]+AnimalSp[i,1]
  Connectance[i,1] <- Conn[i]
  Connect2[i] <- sum(apply(NetworkFull[[i]], 1, sum))/(AnimalSp[i,1]*PlantSp[i,1])
  #Create a vector of 1's and 0's - where 1 is a viable network and 0 is a non-viable network
  Viable1[i] <- ifelse(dim(as.matrix(Network2[[i]]))[1]-dim(NetworkFull[[i]])[1]==0, 1, 0)
  Viable2[i] <- ifelse(dim(as.matrix(Network2[[i]]))[2]-dim(NetworkFull[[i]])[2]==0, 1, 0)
  
  
  for(j in 2:M){
    
    if (dim(Network[[i]][[j-1]])[2]>0){
      plants[[i]][[j]] <- 1:length(Network[[i]][[j-1]][,1]) #create a vector of the last Network's rows
      
      if (length(which(apply(Network[[i]][[j-1]], 1, sum)==0))>0){
        plants[[i]][[j]] <- plants[[i]][[j]][-which(apply(Network[[i]][[j-1]], 1, sum)==0)]
      } #take out any rows of orphaned "plants"
      
      if (PlantSp[i,j]<length(plants[[i]][[j]])){ 
        plants[[i]][[j]] <- sort(sample(plants[[i]][[j]], PlantSp[i,j])) #sample down to new list of plant rows
      }
    }
    
    #the second dimension for the network is 0 (no columns) OR no rows, network is empty
    if (dim(Network[[i]][[j-1]])[2]==0 | dim(Network[[i]][[j-1]])[1]==0){
      plants[[i]][[j]] <- 1
    }
    
    if (dim(Network[[i]][[j-1]])[2]>0 & dim(Network[[i]][[j-1]])[1]>0){
      animals[[i]][[j]] <- 1:length(Network[[i]][[j-1]][1,])
      
      if (length(which(apply(Network[[i]][[j-1]], 2, sum)==0))>0){
        animals[[i]][[j]] <- animals[[i]][[j]][-which(apply(Network[[i]][[j-1]], 2, sum)==0)]
      } #take out any columns of orphaned "animals"  
      
      if (AnimalSp[i,j]<length(animals[[i]][[j]])){ 
        animals[[i]][[j]] <- sort(sample(animals[[i]][[j]], AnimalSp[i,j])) 
      }
    }
    
    #the second dimension for the network is 0 (no columns) OR no rows, network is empty
    if (dim(Network[[i]][[j-1]])[2]==0 | dim(Network[[i]][[j-1]])[1]==0){
      animals[[i]][[j]] <- 1
    }
    
    #both dimensions must be greater than zero (otherwise network is empty)
    if(dim(Network[[i]][[j-1]])[2]>0){
      if(dim(Network[[i]][[j-1]])[1]>0){
        Network[[i]][[j]] <- matrix(Network[[i]][[j-1]][plants[[i]][[j]], animals[[i]][[j]]], nrow=length(plants[[i]][[j]]), ncol=length(animals[[i]][[j]]))
      }
    }
    
    #the second dimension for the network is 0 (no columns) OR no rows, network is empty
    if (dim(Network[[i]][[j-1]])[2]==0 | dim(Network[[i]][[j-1]])[1]==0){
      Network[[i]][[j]] <- Network[[i]][[j-1]]
    }
    
    
    #both dimensions must be greater than zero (otherwise network is empty)
    if(dim(Network[[i]][[j-1]])[2]>0){
      if(dim(Network[[i]][[j-1]])[1]>0){
        OrphanedP[i,j] <- length(which(apply(Network[[i]][[j]], 1, sum)==0))
        OrphanedA[i,j] <- length(which(apply(Network[[i]][[j]], 2, sum)==0))
        RemainP[i,j] <- dim(Network[[i]][[j]])[1]
        RemainA[i,j] <- dim(Network[[i]][[j]])[2]
        RemainN[i,j] <- RemainP[i,j]+RemainA[i,j]
        Connectance[i,j] <- (sum(apply(Network[[i]][[j]], 1, sum)))/(RemainP[i,j]*RemainA[i,j]) 
      }
    }
    
    #the second dimension for the network is 0 (no columns) OR no rows, network is empty
    if (dim(Network[[i]][[j-1]])[2]==0 | dim(Network[[i]][[j-1]])[1]==0){
      OrphanedP[i,j] <- 0
      OrphanedA[i,j] <- 0
      RemainP[i,j] <- 0
      RemainA[i,j] <- 0
      RemainN[i,j] <- RemainP[i,j]+RemainA[i,j]
      Connectance[i,j] <- 0
    }
    
    
  }
}


# Stop the clock
proc.time() - ptm

#Determine which networks are viable - ones that have both plant and animal species remaining
Viable3 <- Viable1+Viable2
rm(AS3)


ConnNew <- mat.or.vec(nr=1, nc=N)
for(i in 1:N){
  ConnNew[i] <- sum(apply(NetworkFull[[i]], 1, sum))/(dim(NetworkFull[[i]])[1]*dim(NetworkFull[[i]])[2])
}

#Put actual connectance values into the Connectance matrix
Connectance[,1] <- Connect2

#About 97% of the networks are viable - remove any that are not
OrphanedP <- OrphanedP[which(Viable3==2),]
OrphanedA <- OrphanedA[which(Viable3==2),]
Connectance <- Connectance[which(Viable3==2),] 
zdiff <- zdiff[which(Viable3==2)]
RemainP <- RemainP[which(Viable3==2),]
RemainA <- RemainA[which(Viable3==2),]

