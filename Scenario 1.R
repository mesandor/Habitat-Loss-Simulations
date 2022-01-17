#Data files needed for analysis - all available in the Habitat-Loss-Simulations repository
#GridPathForAnalysis.csv 
#BirdsGridsJoin.csv - each bird record also has the Grid ID of where it is located for all 7 grid sizes; zipped file because original was too large
#PlantsGridsJoin.csv - same for plants
#BirdNames.csv
#PlantNames.csv


#Read in data files
FN <- read.csv("FullNetworkpresabs.csv")
BirdsEverything <- read.csv("BirdsGridsJoin.csv")
PlantsEverything <- read.csv("PlantsGridsJoin.csv")
BirdNames <- read.csv("BirdNames.csv")
PlantNames <- read.csv("PlantNames.csv")

Birds <- merge(BirdsEverything, BirdNames)
Plants <- merge(PlantsEverything, PlantNames)

#Limit bird observations to April-September
Birds <- subset(Birds, Birds$MONTH!=10)
Birds <- subset(Birds, Birds$MONTH!=11)
Birds <- subset(Birds, Birds$MONTH!=12)
Birds <- subset(Birds, Birds$MONTH!=1)
Birds <- subset(Birds, Birds$MONTH!=2)
Birds <- subset(Birds, Birds$MONTH!=3)

#there are 5 bird species in the eBird files that aren't found in the full network - ACWO, BBWO, CAGU, HOOR, SCOR
Birds <- subset(Birds, Birds$NameCode!="ACWO")
Birds <- subset(Birds, Birds$NameCode!="BBWO")
Birds <- subset(Birds, Birds$NameCode!="CAGU")
Birds <- subset(Birds, Birds$NameCode!="HOOR")
Birds <- subset(Birds, Birds$NameCode!="SCOR")

#Load in the paths data file for the area sizes
Paths <- read.csv("GridPathForAnalysis.csv")
Paths <- Paths[,c(5,6,11,12,17,18,23,24,29,30,35,36,41,42)]

Areas6 <- cbind(Paths$ID_6, Paths$Area_6)
colnames(Areas6) <- c("ID_6", "Area6")

Areas5 <- cbind(Paths$ID_5, Paths$Area_5)
colnames(Areas5) <- c("ID_5", "Area5")

Areas4 <- cbind(Paths$ID_4, Paths$Area_4)
colnames(Areas4) <- c("ID_4", "Area4")

Areas3 <- cbind(Paths$ID_3, Paths$Area_3)
colnames(Areas3) <- c("ID_3", "Area3")

Areas2 <- cbind(Paths$ID_2, Paths$Area_2)
colnames(Areas2) <- c("ID_2", "Area2")

Areas1 <- cbind(Paths$ID_1, Paths$Area_1)
colnames(Areas1) <- c("ID_1", "Area1")

Areas0 <- cbind(Paths$ID, Paths$Area)
colnames(Areas0) <- c("ID", "Area")


#Use the below to modify the plant and bird files
#remove all ID_6 that are smaller than the next grid size down (Area of ID_6 >= 1600000000)
AreasRemove6 <- subset(Areas6, Areas6[,2] >= 1600000000)
length(AreasRemove6[,1])
Areas6 <- AreasRemove6
#remove all of ID_5 that are smaller than the next grid size down (Area of ID_5 >= 400000000)
AreasRemove5 <- subset(Areas5, Areas5[,2] >= 400000000)
length(AreasRemove5[,1])
Areas5 <- AreasRemove5
#remove all of ID_4 that are smaller than the next grid size down (Area of ID_4 >= 100000000)
AreasRemove4 <- subset(Areas4, Areas4[,2] >= 100000000)
length(AreasRemove4[,1])
Areas4 <- AreasRemove4
#remove all of ID_3 that are smaller than the next grid size down (Area of ID_3 >= 25000000)
AreasRemove3 <- subset(Areas3, Areas3[,2] >= 25000000)
length(AreasRemove3[,1])
Areas3 <- AreasRemove3
#remove all of ID_2 that are smaller than the next grid size down (Area of ID_2 >= 6250000)
AreasRemove2 <- subset(Areas2, Areas2[,2] >= 6250000)
length(AreasRemove2[,1])
Areas2 <- AreasRemove2
#remove all of ID_1 that are smaller than the next grid size down (Area of ID_1 >= 1562500)
AreasRemove1 <- subset(Areas1, Areas1[,2] >= 1562500)
length(AreasRemove1[,1])
Areas1 <- AreasRemove1
#remove all of ID that are smaller than 1562500 (Area of ID_ >= 1562500)
AreasRemove0 <- subset(Areas0, Areas0[,2] >= 1562500)
length(AreasRemove0[,1])
Areas0 <- AreasRemove0


#separate into a file for each grid size - 6400 (Grid6), 1600 (Grid5), 400 (Grid4), 100 (Grid3), 25 (Grid2), 6.25 (Grid1), 1.5625 (Grid0) - and table of GridID (columns) by Spp (rows)

PlantsGrid6 <- table(Plants$NameCode, Plants$ID_6)
PlantsGrid5 <- table(Plants$NameCode, Plants$ID_5)
PlantsGrid4 <- table(Plants$NameCode, Plants$ID_4)
PlantsGrid3 <- table(Plants$NameCode, Plants$ID_3)
PlantsGrid2 <- table(Plants$NameCode, Plants$ID_2)
PlantsGrid1 <- table(Plants$NameCode, Plants$ID_1)
PlantsGrid0 <- table(Plants$NameCode, Plants$ID)

#check density of plant sampling in each grid cell

x <- apply(PlantsGrid6, 2, sum)
PlantsArea6 <- mat.or.vec(nr=length(x), nc=2)
PlantsArea6[,1] <- as.numeric(colnames(PlantsGrid6))
PlantsArea6[,2] <- x
colnames(PlantsArea6) <- c("ID_6", "SppNum")
Temp <- merge(PlantsArea6, Areas6)
Plants6 <- unique(Temp)

x <- apply(PlantsGrid5, 2, sum)
PlantsArea5 <- mat.or.vec(nr=length(x), nc=2)
PlantsArea5[,1] <- as.numeric(colnames(PlantsGrid5))
PlantsArea5[,2] <- x
colnames(PlantsArea5) <- c("ID_5", "SppNum")
Temp <- merge(PlantsArea5, Areas5)
Plants5 <- unique(Temp)

x <- apply(PlantsGrid4, 2, sum)
PlantsArea4 <- mat.or.vec(nr=length(x), nc=2)
PlantsArea4[,1] <- as.numeric(colnames(PlantsGrid4))
PlantsArea4[,2] <- x
colnames(PlantsArea4) <- c("ID_4", "SppNum")
Temp <- merge(PlantsArea4, Areas4)
Plants4 <- unique(Temp)

x <- apply(PlantsGrid3, 2, sum)
PlantsArea3 <- mat.or.vec(nr=length(x), nc=2)
PlantsArea3[,1] <- as.numeric(colnames(PlantsGrid3))
PlantsArea3[,2] <- x
colnames(PlantsArea3) <- c("ID_3", "SppNum")
Temp <- merge(PlantsArea3, Areas3)
Plants3 <- unique(Temp)

x <- apply(PlantsGrid2, 2, sum)
PlantsArea2 <- mat.or.vec(nr=length(x), nc=2)
PlantsArea2[,1] <- as.numeric(colnames(PlantsGrid2))
PlantsArea2[,2] <- x
colnames(PlantsArea2) <- c("ID_2", "SppNum")
Temp <- merge(PlantsArea2, Areas2)
Plants2 <- unique(Temp)

x <- apply(PlantsGrid1, 2, sum)
PlantsArea1 <- mat.or.vec(nr=length(x), nc=2)
PlantsArea1[,1] <- as.numeric(colnames(PlantsGrid1))
PlantsArea1[,2] <- x
colnames(PlantsArea1) <- c("ID_1", "SppNum")
Temp <- merge(PlantsArea1, Areas1)
Plants1 <- unique(Temp)

x <- apply(PlantsGrid0, 2, sum)
PlantsArea0 <- mat.or.vec(nr=length(x), nc=2)
PlantsArea0[,1] <- as.numeric(colnames(PlantsGrid0))
PlantsArea0[,2] <- x
colnames(PlantsArea0) <- c("ID", "SppNum")
Temp <- merge(PlantsArea0, Areas0)
Plants0 <- unique(Temp)


#remove the upper and lower (2.5%) quantiles of areas sampled

Density <- Plants6[,2]/Plants6[,3]
Plants6 <- cbind(Plants6, Density)
UQ <- quantile(Plants6[,2]/Plants6[,3], probs=0.975)
LQ <- quantile(Plants6[,2]/Plants6[,3], probs=0.025)
Plants6 <- Plants6[which(Plants6$Density>LQ),]
Plants6 <- Plants6[which(Plants6$Density<UQ),]

Plants <- Plants[which(Plants$ID_6 %in% Plants6$ID_6),]

Density <- Plants5[,2]/Plants5[,3]
Plants5 <- cbind(Plants5, Density)
UQ <- quantile(Plants5[,2]/Plants5[,3], probs=0.975)
LQ <- quantile(Plants5[,2]/Plants5[,3], probs=0.025)
Plants5 <- Plants5[which(Plants5$Density>LQ),]
Plants5 <- Plants5[which(Plants5$Density<UQ),]

Plants <- Plants[which(Plants$ID_5 %in% Plants5$ID_5),]

Density <- Plants4[,2]/Plants4[,3]
Plants4 <- cbind(Plants4, Density)
UQ <- quantile(Plants4[,2]/Plants4[,3], probs=0.975)
LQ <- quantile(Plants4[,2]/Plants4[,3], probs=0.025)
Plants4 <- Plants4[which(Plants4$Density>LQ),]
Plants4 <- Plants4[which(Plants4$Density<UQ),]

Plants <- Plants[which(Plants$ID_4 %in% Plants4$ID_4),]

Density <- Plants3[,2]/Plants3[,3]
Plants3 <- cbind(Plants3, Density)
UQ <- quantile(Plants3[,2]/Plants3[,3], probs=0.975)
LQ <- quantile(Plants3[,2]/Plants3[,3], probs=0.025)
Plants3 <- Plants3[which(Plants3$Density>LQ),]
Plants3 <- Plants3[which(Plants3$Density<UQ),]

Plants <- Plants[which(Plants$ID_3 %in% Plants3$ID_3),]

Density <- Plants2[,2]/Plants2[,3]
Plants2 <- cbind(Plants2, Density)
UQ <- quantile(Plants2[,2]/Plants2[,3], probs=0.975)
LQ <- quantile(Plants2[,2]/Plants2[,3], probs=0.025)
Plants2 <- Plants2[which(Plants2$Density>LQ),]
Plants2 <- Plants2[which(Plants2$Density<UQ),]

Plants <- Plants[which(Plants$ID_2 %in% Plants2$ID_2),]

Density <- Plants1[,2]/Plants1[,3]
Plants1 <- cbind(Plants1, Density)
UQ <- quantile(Plants1[,2]/Plants1[,3], probs=0.975)
LQ <- quantile(Plants1[,2]/Plants1[,3], probs=0.025)
Plants1 <- Plants1[which(Plants1$Density>LQ),]
Plants1 <- Plants1[which(Plants1$Density<UQ),]

Plants <- Plants[which(Plants$ID_1 %in% Plants1$ID_1),]

Density <- Plants0[,2]/Plants0[,3]
Plants0 <- cbind(Plants0, Density)
UQ <- quantile(Plants0[,2]/Plants0[,3], probs=0.975)
LQ <- quantile(Plants0[,2]/Plants0[,3], probs=0.025)
Plants0 <- Plants0[which(Plants0$Density>LQ),]
Plants0 <- Plants0[which(Plants0$Density<UQ),]

Plants <- Plants[which(Plants$ID %in% Plants0$ID),]

#Redo the plant tables to incorporate changes
PlantsGrid6 <- table(Plants$NameCode, Plants$ID_6)
PlantsGrid5 <- table(Plants$NameCode, Plants$ID_5)
PlantsGrid4 <- table(Plants$NameCode, Plants$ID_4)
PlantsGrid3 <- table(Plants$NameCode, Plants$ID_3)
PlantsGrid2 <- table(Plants$NameCode, Plants$ID_2)
PlantsGrid1 <- table(Plants$NameCode, Plants$ID_1)
PlantsGrid0 <- table(Plants$NameCode, Plants$ID)

#the above gives number of INDIVIDUALS of each spp - need to translate it to 0's and 1's so that just counting each spp ONCE (pres/abs)
PlantsGrid6 <- ifelse(PlantsGrid6>=1, 1, 0)
PlantsGrid5 <- ifelse(PlantsGrid5>=1, 1, 0)
PlantsGrid4 <- ifelse(PlantsGrid4>=1, 1, 0)
PlantsGrid3 <- ifelse(PlantsGrid3>=1, 1, 0)
PlantsGrid2 <- ifelse(PlantsGrid2>=1, 1, 0)
PlantsGrid1 <- ifelse(PlantsGrid1>=1, 1, 0)
PlantsGrid0 <- ifelse(PlantsGrid0>=1, 1, 0)

BirdsGrid6 <- table(Birds$NameCode, Birds$ID_6)
BirdsGrid5 <- table(Birds$NameCode, Birds$ID_5)
BirdsGrid4 <- table(Birds$NameCode, Birds$ID_4)
BirdsGrid3 <- table(Birds$NameCode, Birds$ID_3)
BirdsGrid2 <- table(Birds$NameCode, Birds$ID_2)
BirdsGrid1 <- table(Birds$NameCode, Birds$ID_1)
BirdsGrid0 <- table(Birds$NameCode, Birds$ID)

#check density of bird sampling in each grid cell

x <- apply(BirdsGrid6, 2, sum)
BirdsArea6 <- mat.or.vec(nr=length(x), nc=2)
BirdsArea6[,1] <- as.numeric(colnames(BirdsGrid6))
BirdsArea6[,2] <- x
colnames(BirdsArea6) <- c("ID_6", "SppNum")
Temp <- merge(BirdsArea6, Areas6)
Birds6 <- unique(Temp)

x <- apply(BirdsGrid5, 2, sum)
BirdsArea5 <- mat.or.vec(nr=length(x), nc=2)
BirdsArea5[,1] <- as.numeric(colnames(BirdsGrid5))
BirdsArea5[,2] <- x
colnames(BirdsArea5) <- c("ID_5", "SppNum")
Temp <- merge(BirdsArea5, Areas5)
Birds5 <- unique(Temp)

x <- apply(BirdsGrid4, 2, sum)
BirdsArea4 <- mat.or.vec(nr=length(x), nc=2)
BirdsArea4[,1] <- as.numeric(colnames(BirdsGrid4))
BirdsArea4[,2] <- x
colnames(BirdsArea4) <- c("ID_4", "SppNum")
Temp <- merge(BirdsArea4, Areas4)
Birds4 <- unique(Temp)

x <- apply(BirdsGrid3, 2, sum)
BirdsArea3 <- mat.or.vec(nr=length(x), nc=2)
BirdsArea3[,1] <- as.numeric(colnames(BirdsGrid3))
BirdsArea3[,2] <- x
colnames(BirdsArea3) <- c("ID_3", "SppNum")
Temp <- merge(BirdsArea3, Areas3)
Birds3 <- unique(Temp)

x <- apply(BirdsGrid2, 2, sum)
BirdsArea2 <- mat.or.vec(nr=length(x), nc=2)
BirdsArea2[,1] <- as.numeric(colnames(BirdsGrid2))
BirdsArea2[,2] <- x
colnames(BirdsArea2) <- c("ID_2", "SppNum")
Temp <- merge(BirdsArea2, Areas2)
Birds2 <- unique(Temp)

x <- apply(BirdsGrid1, 2, sum)
BirdsArea1 <- mat.or.vec(nr=length(x), nc=2)
BirdsArea1[,1] <- as.numeric(colnames(BirdsGrid1))
BirdsArea1[,2] <- x
colnames(BirdsArea1) <- c("ID_1", "SppNum")
Temp <- merge(BirdsArea1, Areas1)
Birds1 <- unique(Temp)

x <- apply(BirdsGrid0, 2, sum)
BirdsArea0 <- mat.or.vec(nr=length(x), nc=2)
BirdsArea0[,1] <- as.numeric(colnames(BirdsGrid0))
BirdsArea0[,2] <- x
colnames(BirdsArea0) <- c("ID", "SppNum")
Temp <- merge(BirdsArea0, Areas0)
Birds0 <- unique(Temp)


#remove the upper and lower (2.5%) quantiles of areas sampled

Density <- Birds6[,2]/Birds6[,3]
Birds6 <- cbind(Birds6, Density)
UQ <- quantile(Birds6[,2]/Birds6[,3], probs=0.975)
LQ <- quantile(Birds6[,2]/Birds6[,3], probs=0.025)
Birds6 <- Birds6[which(Birds6$Density>LQ),]
Birds6 <- Birds6[which(Birds6$Density<UQ),]

Birds <- Birds[which(Birds$ID_6 %in% Birds6$ID_6),]

Density <- Birds5[,2]/Birds5[,3]
Birds5 <- cbind(Birds5, Density)
UQ <- quantile(Birds5[,2]/Birds5[,3], probs=0.975)
LQ <- quantile(Birds5[,2]/Birds5[,3], probs=0.025)
Birds5 <- Birds5[which(Birds5$Density>LQ),]
Birds5 <- Birds5[which(Birds5$Density<UQ),]

Birds <- Birds[which(Birds$ID_5 %in% Birds5$ID_5),]

Density <- Birds4[,2]/Birds4[,3]
Birds4 <- cbind(Birds4, Density)
UQ <- quantile(Birds4[,2]/Birds4[,3], probs=0.975)
LQ <- quantile(Birds4[,2]/Birds4[,3], probs=0.025)
Birds4 <- Birds4[which(Birds4$Density>LQ),]
Birds4 <- Birds4[which(Birds4$Density<UQ),]

Birds <- Birds[which(Birds$ID_4 %in% Birds4$ID_4),]

Density <- Birds3[,2]/Birds3[,3]
Birds3 <- cbind(Birds3, Density)
UQ <- quantile(Birds3[,2]/Birds3[,3], probs=0.975)
LQ <- quantile(Birds3[,2]/Birds3[,3], probs=0.025)
Birds3 <- Birds3[which(Birds3$Density>LQ),]
Birds3 <- Birds3[which(Birds3$Density<UQ),]

Birds <- Birds[which(Birds$ID_3 %in% Birds3$ID_3),]

Density <- Birds2[,2]/Birds2[,3]
Birds2 <- cbind(Birds2, Density)
UQ <- quantile(Birds2[,2]/Birds2[,3], probs=0.975)
LQ <- quantile(Birds2[,2]/Birds2[,3], probs=0.025)
Birds2 <- Birds2[which(Birds2$Density>LQ),]
Birds2 <- Birds2[which(Birds2$Density<UQ),]

Birds <- Birds[which(Birds$ID_2 %in% Birds2$ID_2),]

Density <- Birds1[,2]/Birds1[,3]
Birds1 <- cbind(Birds1, Density)
UQ <- quantile(Birds1[,2]/Birds1[,3], probs=0.975)
LQ <- quantile(Birds1[,2]/Birds1[,3], probs=0.025)
Birds1 <- Birds1[which(Birds1$Density>LQ),]
Birds1 <- Birds1[which(Birds1$Density<UQ),]

Birds <- Birds[which(Birds$ID_1 %in% Birds1$ID_1),]

Density <- Birds0[,2]/Birds0[,3]
Birds0 <- cbind(Birds0, Density)
UQ <- quantile(Birds0[,2]/Birds0[,3], probs=0.975)
LQ <- quantile(Birds0[,2]/Birds0[,3], probs=0.025)
Birds0 <- Birds0[which(Birds0$Density>LQ),]
Birds0 <- Birds0[which(Birds0$Density<UQ),]

Birds <- Birds[which(Birds$ID %in% Birds0$ID),]

#Have to redo the tables to incorporate changes
BirdsGrid6 <- table(Birds$NameCode, Birds$ID_6)
BirdsGrid5 <- table(Birds$NameCode, Birds$ID_5)
BirdsGrid4 <- table(Birds$NameCode, Birds$ID_4)
BirdsGrid3 <- table(Birds$NameCode, Birds$ID_3)
BirdsGrid2 <- table(Birds$NameCode, Birds$ID_2)
BirdsGrid1 <- table(Birds$NameCode, Birds$ID_1)
BirdsGrid0 <- table(Birds$NameCode, Birds$ID)

BirdsGrid6 <- ifelse(BirdsGrid6>=1, 1, 0)
BirdsGrid5 <- ifelse(BirdsGrid5>=1, 1, 0)
BirdsGrid4 <- ifelse(BirdsGrid4>=1, 1, 0)
BirdsGrid3 <- ifelse(BirdsGrid3>=1, 1, 0)
BirdsGrid2 <- ifelse(BirdsGrid2>=1, 1, 0)
BirdsGrid1 <- ifelse(BirdsGrid1>=1, 1, 0)
BirdsGrid0 <- ifelse(BirdsGrid0>=1, 1, 0)


#Run the path analysis
Paths <- read.csv("GridPathForAnalysis.csv")


PathsRed6 <- Paths[which(Paths$ID_6 %in% colnames(BirdsGrid6) & Paths$ID_6 %in% colnames(PlantsGrid6)), ] 
PathsRed5 <- PathsRed6[which(PathsRed6$ID_5 %in% colnames(BirdsGrid5) & PathsRed6$ID_5 %in% colnames(PlantsGrid5)), ] 
PathsRed4 <- PathsRed5[which(PathsRed5$ID_4 %in% colnames(BirdsGrid4) & PathsRed5$ID_4 %in% colnames(PlantsGrid4)), ] 
PathsRed3 <- PathsRed4[which(PathsRed4$ID_3 %in% colnames(BirdsGrid3) & PathsRed4$ID_3 %in% colnames(PlantsGrid3)), ] 
PathsRed2 <- PathsRed3[which(PathsRed3$ID_2 %in% colnames(BirdsGrid2) & PathsRed3$ID_2 %in% colnames(PlantsGrid2)), ] 
PathsRed1 <- PathsRed2[which(PathsRed2$ID_1 %in% colnames(BirdsGrid1) & PathsRed2$ID_1 %in% colnames(PlantsGrid1)), ] 
PathsRed0 <- PathsRed1[which(PathsRed1$ID %in% colnames(BirdsGrid0) & PathsRed1$ID %in% colnames(PlantsGrid0)), ] #number of rows here is used below - (129)

#save for area
PathsRed0AREA <- PathsRed0

PathsRed0 <- PathsRed0[,c(5, 11, 17, 23, 29, 35, 41)]


Orphaned <- list()
OrphanedBIRD <- list()
Lost <- list()
LostBIRD <- list()
Original <- list()
Remain <- list()

#use the 1st (or whatever) row of paths to look up the correct (matching ID) col in the proper Veg file and the correct (matching ID) col in the proper Bird file
Veg1 <- list()
Veg2 <- list()
Veg3 <- list()
Veg4 <- list()
Veg5 <- list()
Veg6 <- list()
Veg7 <- list()
Bird1 <- list()
Bird2 <- list()
Bird3 <- list()
Bird4 <- list()
Bird5 <- list()
Bird6 <- list()
Bird7 <- list()
Network1 <- list()
Network2 <- list()
Network3 <- list()
Network4 <- list()
Network5 <- list()
Network6 <- list()
Network7 <- list()
Network1R <- list()
Network2R <- list()
Network3R <- list()
Network4R <- list()
Network5R <- list()
Network6R <- list()
Network7R <- list()
x1 <- list()
x2 <- list()
x3 <- list()
x4 <- list()
x5 <- list()
x6 <- list()
y1 <- list()
y2 <- list()
y3 <- list()
y4 <- list()
y5 <- list()
y6 <- list()
y7 <- list()
y8 <- list()
y9 <- list()
y10 <- list()
y11 <- list()
y12 <- list()
y13 <- list()
y14 <- list()
y15 <- list()
y16 <- list()
y17 <- list()
y18 <- list()
y19 <- list()
y20 <- list()
y21 <- list()
temp1 <- list()
temp2 <- list()
temp3 <- list()
temp4 <- list()
temp5 <- list()
temp6 <- list()
temp7 <- list()
temp8 <- list()
temp9 <- list()
temp10 <- list()
temp11 <- list()
temp12 <- list()
temp13 <- list()
temp14 <- list()
temp15 <- list()
temp16 <- list()
temp17 <- list()
temp18 <- list()
temp19 <- list()
temp20 <- list()
temp21 <- list()

for(j in 1:129){
  
  Veg1[[j]] <- as.data.frame(PlantsGrid6[,c(1,which(colnames(PlantsGrid6)==PathsRed0[j,7]))]) #col 7 is ID_6, which is Grid Size 6400
  Veg1[[j]] <- Veg1[[j]][which(Veg1[[j]][,2]==1),] #uses only the spp that are present
  
  Bird1[[j]] <- as.data.frame(BirdsGrid6[,c(1,which(colnames(BirdsGrid6)==PathsRed0[j,7]))]) #col 7 is ID_6, which is Grid Size 6400
  Bird1[[j]] <- Bird1[[j]][which(Bird1[[j]][,2]==1),]
  
  #construct initial network (Grid Size 6400)
  #merge Veg with the Full Network to reduce to just Veg spp present
  Network1[[j]] <- FN[which(FN$Scientific.Name %in% rownames(Veg1[[j]])),]
  #Reduce the network to just bird species that are present
  Network1R[[j]] <- Network1[[j]][,c(1,which(colnames(Network1[[j]])[-c(1:2)] %in% rownames(Bird1[[j]]))+2)]
  
  #construct next network (Grid Size 1600)
  Veg2[[j]] <- as.data.frame(PlantsGrid5[,c(1,which(colnames(PlantsGrid5)==PathsRed0[j,6]))]) #col 6 is ID_5, which is Grid Size 1600
  Veg2[[j]] <- Veg2[[j]][which(Veg2[[j]][,2]==1),]
  
  Bird2[[j]] <- as.data.frame(BirdsGrid5[,c(1,which(colnames(BirdsGrid5)==PathsRed0[j,6]))]) #col 6 is ID_5, which is Grid Size 1600
  Bird2[[j]] <- Bird2[[j]][which(Bird2[[j]][,2]==1),]
  
  #merge Veg with the Full Network to reduce to just Veg spp present
  Network2[[j]] <- FN[which(FN$Scientific.Name %in% rownames(Veg2[[j]])),]
  #Reduce the network to just bird species that are present
  Network2R[[j]] <- Network2[[j]][,c(1,which(colnames(Network2[[j]])[-c(1:2)] %in% rownames(Bird2[[j]]))+2)]
  
  #construct next network (Grid Size 400)
  Veg3[[j]] <- as.data.frame(PlantsGrid4[,c(1,which(colnames(PlantsGrid4)==PathsRed0[j,5]))]) #col 5 is ID_4, which is Grid Size 400
  Veg3[[j]] <- Veg3[[j]][which(Veg3[[j]][,2]==1),]
  
  Bird3[[j]] <- as.data.frame(BirdsGrid4[,c(1,which(colnames(BirdsGrid4)==PathsRed0[j,5]))]) #col 5 is ID_4, which is Grid Size 400
  Bird3[[j]] <- Bird3[[j]][which(Bird3[[j]][,2]==1),]
  
  #merge Veg with the Full Network to reduce to just Veg spp present
  Network3[[j]] <- FN[which(FN$Scientific.Name %in% rownames(Veg3[[j]])),]
  #Reduce the network to just bird species that are present
  Network3R[[j]] <- Network3[[j]][,c(1,which(colnames(Network3[[j]])[-c(1:2)] %in% rownames(Bird3[[j]]))+2)]
  
  #construct next network (Grid Size 100)
  Veg4[[j]] <- as.data.frame(PlantsGrid3[,c(1,which(colnames(PlantsGrid3)==PathsRed0[j,4]))]) #col 4 is ID_3, Grid Size 100
  Veg4[[j]] <- Veg4[[j]][which(Veg4[[j]][,2]==1),]
  
  Bird4[[j]] <- as.data.frame(BirdsGrid3[,c(1,which(colnames(BirdsGrid3)==PathsRed0[j,4]))]) #col 4 is ID_3, Grid Size 100
  Bird4[[j]] <- Bird4[[j]][which(Bird4[[j]][,2]==1),]
  
  #merge Veg with the Full Network to reduce to just Veg spp present
  Network4[[j]] <- FN[which(FN$Scientific.Name %in% rownames(Veg4[[j]])),]
  #Reduce the network to just bird species that are present
  Network4R[[j]] <- Network4[[j]][,c(1,which(colnames(Network4[[j]])[-c(1:2)] %in% rownames(Bird4[[j]]))+2)]
  
  #construct next network (Grid Size 25)
  Veg5[[j]] <- as.data.frame(PlantsGrid2[,c(1,which(colnames(PlantsGrid2)==PathsRed0[j,3]))]) #col 3 is ID_2, which is Grid Size 25
  Veg5[[j]] <- Veg5[[j]][which(Veg5[[j]][,2]==1),]
  
  Bird5[[j]] <- as.data.frame(BirdsGrid2[,c(1,which(colnames(BirdsGrid2)==PathsRed0[j,3]))]) #col 3 is ID_2, which is Grid Size 25
  Bird5[[j]] <- Bird5[[j]][which(Bird5[[j]][,2]==1),]
  
  #merge Veg with the Full Network to reduce to just Veg spp present
  Network5[[j]] <- FN[which(FN$Scientific.Name %in% rownames(Veg5[[j]])),]
  #Reduce the network to just bird species that are present
  Network5R[[j]] <- Network5[[j]][,c(1,which(colnames(Network5[[j]])[-c(1:2)] %in% rownames(Bird5[[j]]))+2)]
  
  #construct next network (Grid Size 6.25)
  Veg6[[j]] <- as.data.frame(PlantsGrid1[,c(1,which(colnames(PlantsGrid1)==PathsRed0[j,2]))]) #col 2 is ID_1, which is Grid Size 6.25 
  Veg6[[j]] <- Veg6[[j]][which(Veg6[[j]][,2]==1),]
  
  Bird6[[j]] <- as.data.frame(BirdsGrid1[,c(1,which(colnames(BirdsGrid1)==PathsRed0[j,2]))]) #col 2 is ID_1, which is Grid Size 6.25 
  Bird6[[j]] <- Bird6[[j]][which(Bird6[[j]][,2]==1),]
  
  #merge Veg with the Full Network to reduce to just Veg spp present
  Network6[[j]] <- FN[which(FN$Scientific.Name %in% rownames(Veg6[[j]])),]
  #Reduce the network to just bird species that are present
  Network6R[[j]] <- Network6[[j]][,c(1,which(colnames(Network6[[j]])[-c(1:2)] %in% rownames(Bird6[[j]]))+2)]
  Network6R[[j]] <- as.data.frame(Network6R[[j]])
  
  #construct next Network (1.5625)
  Veg7[[j]] <- as.data.frame(PlantsGrid0[,c(1,which(colnames(PlantsGrid0)==PathsRed0[j,1]))]) #col 1 is ID, which is Grid Size 1.5625
  Veg7[[j]] <- Veg7[[j]][which(Veg7[[j]][,2]==1),]
  
  Bird7[[j]] <- as.data.frame(BirdsGrid0[,c(1,which(colnames(BirdsGrid0)==PathsRed0[j,1]))]) #col 1 is ID, which is Grid Size 1.5625
  Bird7[[j]] <- Bird7[[j]][which(Bird7[[j]][,2]==1),]
  
  #merge Veg with the Full Network to reduce to just Veg spp present
  Network7[[j]] <- as.data.frame(FN[which(FN$Scientific.Name %in% rownames(Veg7[[j]])),])
  #Reduce the network to just bird species that are present
  Network7R[[j]] <- Network7[[j]][,c(1,which(colnames(Network7[[j]])[-c(1:2)] %in% rownames(Bird7[[j]]))+2)]
  Network7R[[j]] <- as.data.frame(Network7R[[j]])
}

for(j in 1:129){
  #count initial "Orphaned" plant species
  Orphaned[[j]] <- mat.or.vec(nr=1, nc=7)
  
  #take out the rows that were 0 (Orphaned spp) in the previous networks
  if (length(which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0))>0){
    Network2R[[j]] <- subset(Network2R[[j]], Network2R[[j]][,1]!=
                               Network1R[[j]][which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0),1])
    Network3R[[j]] <- subset(Network3R[[j]], Network3R[[j]][,1]!=
                               Network1R[[j]][which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0),1])
    Network4R[[j]] <- subset(Network4R[[j]], Network4R[[j]][,1]!=
                               Network1R[[j]][which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0),1])
    Network5R[[j]] <- subset(Network5R[[j]], Network5R[[j]][,1]!=
                               Network1R[[j]][which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0),1])
    Network6R[[j]] <- subset(Network6R[[j]], Network6R[[j]][,1]!=
                               Network1R[[j]][which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0),1])
    Network7R[[j]] <- subset(Network7R[[j]], Network7R[[j]][,1]!=
                               Network1R[[j]][which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0),1])
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network2R[[j]][,-1]), 1, sum)==0))>0){
    Network3R[[j]] <- subset(Network3R[[j]], Network3R[[j]][,1]!=
                               Network2R[[j]][which(apply(data.matrix(Network2R[[j]][,-1]), 1, sum)==0),1])
    Network4R[[j]] <- subset(Network4R[[j]], Network4R[[j]][,1]!=
                               Network2R[[j]][which(apply(data.matrix(Network2R[[j]][,-1]), 1, sum)==0),1])
    Network5R[[j]] <- subset(Network5R[[j]], Network5R[[j]][,1]!=
                               Network2R[[j]][which(apply(data.matrix(Network2R[[j]][,-1]), 1, sum)==0),1])
    Network6R[[j]] <- subset(Network6R[[j]], Network6R[[j]][,1]!=
                               Network2R[[j]][which(apply(data.matrix(Network2R[[j]][,-1]), 1, sum)==0),1])
    Network7R[[j]] <- subset(Network7R[[j]], Network7R[[j]][,1]!=
                               Network2R[[j]][which(apply(data.matrix(Network2R[[j]][,-1]), 1, sum)==0),1])
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network3R[[j]][,-1]), 1, sum)==0))>0){
    Network4R[[j]] <- subset(Network4R[[j]], Network4R[[j]][,1]!=
                               Network3R[[j]][which(apply(data.matrix(Network3R[[j]][,-1]), 1, sum)==0),1])
    Network5R[[j]] <- subset(Network5R[[j]], Network5R[[j]][,1]!=
                               Network3R[[j]][which(apply(data.matrix(Network3R[[j]][,-1]), 1, sum)==0),1])
    Network6R[[j]] <- subset(Network6R[[j]], Network6R[[j]][,1]!=
                               Network3R[[j]][which(apply(data.matrix(Network3R[[j]][,-1]), 1, sum)==0),1])
    Network7R[[j]] <- subset(Network7R[[j]], Network7R[[j]][,1]!=
                               Network3R[[j]][which(apply(data.matrix(Network3R[[j]][,-1]), 1, sum)==0),1])
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network4R[[j]][,-1]), 1, sum)==0))>0){
    Network5R[[j]] <- subset(Network5R[[j]], Network5R[[j]][,1]!=
                               Network4R[[j]][which(apply(data.matrix(Network4R[[j]][,-1]), 1, sum)==0),1])
    Network6R[[j]] <- subset(Network6R[[j]], Network6R[[j]][,1]!=
                               Network4R[[j]][which(apply(data.matrix(Network4R[[j]][,-1]), 1, sum)==0),1])
    Network7R[[j]] <- subset(Network7R[[j]], Network7R[[j]][,1]!=
                               Network4R[[j]][which(apply(data.matrix(Network4R[[j]][,-1]), 1, sum)==0),1])
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network5R[[j]][,-1]), 1, sum)==0))>0){
    Network6R[[j]] <- subset(Network6R[[j]], Network6R[[j]][,1]!=
                               Network5R[[j]][which(apply(data.matrix(Network5R[[j]][,-1]), 1, sum)==0),1])
    Network7R[[j]] <- subset(Network7R[[j]], Network7R[[j]][,1]!=
                               Network5R[[j]][which(apply(data.matrix(Network5R[[j]][,-1]), 1, sum)==0),1])
  }
}
#22 is empty for Network6R, #36 throws a warning message (species only present in one taxonomic group)
N <- c(1:129)
N <- N[-c(22,36)]
for(j in 1:127){
  if (length(which(apply(data.matrix(Network6R[[N[j]]][,-1]), 1, sum)==0))>0){
    Network7R[[N[j]]] <- subset(Network7R[[N[j]]], Network7R[[N[j]]][,1]!=
                                  Network6R[[N[j]]][which(apply(data.matrix(Network6R[[N[j]]][,-1]), 1, sum)==0),1])
  }
}
#because #36 throws a warning message
Network7R[[36]] <- Network6R[[36]][2,]

#take out the COLUMNS that were 0 (Orphaned spp) in the previous networks
for(j in 1:129){
  if (length(which(apply(data.matrix(Network1R[[j]][,-1]), 2, sum)==0))>0){
    x1[[j]] <- colnames(Network1R[[j]][,-1])
    x1[[j]] <- x1[[j]][which(apply(data.matrix(Network1R[[j]][,-1]), 2, sum)==0)]
    y1[[j]] <- colnames(Network2R[[j]])
    temp1[[j]] <- which(y1[[j]] %in% x1[[j]])
    if(length(temp1[[j]]>0)){
      Network2R[[j]] <- Network2R[[j]][,-temp1[[j]]]
    }
    y2[[j]] <- colnames(Network3R[[j]])
    temp2[[j]] <- which(y2[[j]] %in% x1[[j]])
    if(length(temp2[[j]]>0)){
      Network3R[[j]] <- Network3R[[j]][,-temp2[[j]]]
    }
    y3[[j]] <- colnames(Network4R[[j]])
    temp3[[j]] <- which(y3[[j]] %in% x1[[j]])
    if(length(temp3[[j]]>0)){
      Network4R[[j]] <- Network4R[[j]][,-temp3[[j]]]
    }
    y4[[j]] <- colnames(Network5R[[j]])
    temp4[[j]] <- which(y4[[j]] %in% x1[[j]])
    if(length(temp4[[j]]>0)){
      Network5R[[j]] <- Network5R[[j]][,-temp4[[j]]]
    }
    y5[[j]] <- colnames(Network6R[[j]])
    temp5[[j]] <- which(y5[[j]] %in% x1[[j]])
    if(length(temp5[[j]]>0)){
      Network6R[[j]] <- Network6R[[j]][,-temp5[[j]]]
    }
    y6[[j]] <- colnames(Network7R[[j]])
    temp6[[j]] <- which(y6[[j]] %in% x1[[j]])
    if(length(temp6[[j]]>0)){
      Network7R[[j]] <- Network7R[[j]][,-temp6[[j]]]
    }
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network2R[[j]][,-1]), 2, sum)==0))>0){
    x2[[j]] <- colnames(Network2R[[j]][,-1])
    x2[[j]] <- x2[[j]][which(apply(data.matrix(Network2R[[j]][,-1]), 2, sum)==0)]
    y7[[j]] <- colnames(Network3R[[j]])
    temp7[[j]] <- which(y7[[j]] %in% x2[[j]])
    if(length(temp7[[j]]>0)){
      Network3R[[j]] <- Network3R[[j]][,-temp7[[j]]]
    }
    y8[[j]] <- colnames(Network4R[[j]])
    temp8[[j]] <- which(y8[[j]] %in% x2[[j]])
    if(length(temp8[[j]]>0)){
      Network4R[[j]] <- Network4R[[j]][,-temp8[[j]]]
    }
    y9[[j]] <- colnames(Network5R[[j]])
    temp9[[j]] <- which(y9[[j]] %in% x2[[j]])
    if(length(temp9[[j]]>0)){
      Network5R[[j]] <- Network5R[[j]][,-temp9[[j]]]
    }
    y10[[j]] <- colnames(Network6R[[j]])
    temp10[[j]] <- which(y10[[j]] %in% x2[[j]])
    if(length(temp10[[j]]>0)){
      Network6R[[j]] <- Network6R[[j]][,-temp10[[j]]]
    }
    y11[[j]] <- colnames(Network7R[[j]])
    temp11[[j]] <- which(y11[[j]] %in% x2[[j]])
    if(length(temp11[[j]]>0)){
      Network7R[[j]] <- Network7R[[j]][,-temp11[[j]]]
    }
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network3R[[j]][,-1]), 2, sum)==0))>0){
    x3[[j]] <- colnames(Network3R[[j]][,-1])
    x3[[j]] <- x3[[j]][which(apply(data.matrix(Network3R[[j]][,-1]), 2, sum)==0)]
    y12[[j]] <- colnames(Network4R[[j]])
    temp12[[j]] <- which(y12[[j]] %in% x3[[j]])
    if(length(temp12[[j]]>0)){
      Network4R[[j]] <- Network4R[[j]][,-temp12[[j]]]
    }
    y13[[j]] <- colnames(Network5R[[j]])
    temp13[[j]] <- which(y13[[j]] %in% x3[[j]])
    if(length(temp13[[j]]>0)){
      Network5R[[j]] <- Network5R[[j]][,-temp13[[j]]]
    }
    y14[[j]] <- colnames(Network6R[[j]])
    temp14[[j]] <- which(y14[[j]] %in% x3[[j]])
    if(length(temp14[[j]]>0)){
      Network6R[[j]] <- Network6R[[j]][,-temp14[[j]]]
    }
    y15[[j]] <- colnames(Network7R[[j]])
    temp15[[j]] <- which(y15[[j]] %in% x3[[j]])
    if(length(temp15[[j]]>0)){
      Network7R[[j]] <- Network7R[[j]][,-temp15[[j]]]
    }
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network4R[[j]][,-1]), 2, sum)==0))>0){
    x4[[j]] <- colnames(Network4R[[j]][,-1])
    x4[[j]] <- x4[[j]][which(apply(data.matrix(Network4R[[j]][,-1]), 2, sum)==0)]
    y16[[j]] <- colnames(Network5R[[j]])
    temp16[[j]] <- which(y16[[j]] %in% x4[[j]])
    if(length(temp16[[j]]>0)){
      Network5R[[j]] <- Network5R[[j]][,-temp16[[j]]]
    }
  }
}
#Network6R[[68]] is empty. But Network6R[[22]] is also empty and does not give warning messages.
Network6R[[68]] <- Network6R[[22]]
Network7R[[68]] <- Network7R[[22]]
for(j in 1:129){
  if (length(which(apply(data.matrix(Network4R[[j]][,-1]), 2, sum)==0))>0){
    y17[[j]] <- colnames(Network6R[[j]])
    temp17[[j]] <- which(y17[[j]] %in% x4[[j]])
    if(length(temp17[[j]]>0)){
      Network6R[[j]] <- Network6R[[j]][,-temp17[[j]]]
    }
    y18[[j]] <- colnames(Network7R[[j]])
    temp18[[j]] <- which(y18[[j]] %in% x4[[j]])
    if(length(temp18[[j]]>0)){
      Network7R[[j]] <- Network7R[[j]][,-temp18[[j]]]
    }
  }
}
for(j in 1:129){
  if (length(which(apply(data.matrix(Network5R[[j]][,-1]), 2, sum)==0))>0){
    x5[[j]] <- colnames(Network5R[[j]])[-1]
    x5[[j]] <- x5[[j]][which(apply(data.matrix(Network5R[[j]][,-1]), 2, sum)==0)]
    y19[[j]] <- colnames(Network6R[[j]])
    temp19[[j]] <- which(y19[[j]] %in% x5[[j]])
    if(length(temp19[[j]]>0)){
      Network6R[[j]] <- Network6R[[j]][,-temp19[[j]]]
    }
  }
}
#Network7R[[19]] and Network7R[[18]] are empty. But Network7R[[22]] is also empty and does not give warning messages. 
Network7R[[19]] <- Network7R[[22]]
Network7R[[18]] <- Network7R[[22]]
for(j in 1:129){
  if (length(which(apply(data.matrix(Network5R[[j]][,-1]), 2, sum)==0))>0){
    y20[[j]] <- colnames(Network7R[[j]])
    temp20[[j]] <- which(y20[[j]] %in% x5[[j]])
    if(length(temp20[[j]]>0)){
      Network7R[[j]] <- Network7R[[j]][,-temp20[[j]]]
    }
  }
}
#Network6R[[3]] is empty. But Network7R[[68]] is also empty and does not give warning messages.
Network6R[[3]] <- Network6R[[68]]
Network7R[[3]] <- Network7R[[68]]
Network6R[[18]] <- Network6R[[68]]
Network7R[[18]] <- Network7R[[68]]
Network6R[[19]] <- Network6R[[68]]
Network7R[[19]] <- Network7R[[68]]
Network6R[[22]] <- Network6R[[68]]
Network7R[[22]] <- Network7R[[68]]
for(j in 1:129){
  if (length(which(apply(data.matrix(Network6R[[j]][,-1]), 1, sum)==0))>0){
    x6[[j]] <- colnames(Network6R[[j]])[-1]
    x6[[j]] <- x6[[j]][which(apply(data.matrix(Network6R[[j]][,-1]), 2, sum)==0)]
    y21[[j]] <- colnames(Network7R[[j]])
    temp21[[j]] <- which(y21[[j]] %in% x6[[j]])
    if(length(temp21[[j]]>0)){
      Network7R[[j]] <- Network7R[[j]][,-temp21[[j]]]
    }
  }
}

#Fix any of the other empty 7R networks by replacing with an empty network that does not give warning messages.
Network7R[[13]] <- Network7R[[22]]
Network7R[[97]] <- Network7R[[22]]
Network7R[[102]] <- Network7R[[22]]
Network7R[[110]] <- Network7R[[22]]
Network7R[[112]] <- Network7R[[22]]

for(j in 1:129){
  Orphaned[[j]] <- mat.or.vec(nr=1, nc=7)
  #count plant Orphaned species
  Orphaned[[j]][1] <- length(which(apply(data.matrix(Network1R[[j]][,-1]), 1, sum)==0))
  Orphaned[[j]][2] <- length(which(apply(data.matrix(Network2R[[j]][,-1]), 1, sum)==0))
  Orphaned[[j]][3] <- length(which(apply(data.matrix(Network3R[[j]][,-1]), 1, sum)==0))
  Orphaned[[j]][4] <- length(which(apply(data.matrix(Network4R[[j]][,-1]), 1, sum)==0))
  Orphaned[[j]][5] <- length(which(apply(data.matrix(Network5R[[j]][,-1]), 1, sum)==0))
  Orphaned[[j]][6] <- length(which(apply(data.matrix(Network6R[[j]][,-1]), 1, sum)==0))
  Orphaned[[j]][7] <- length(which(apply(data.matrix(Network7R[[j]][,-1]), 1, sum)==0))
}

for(j in 1:129){
  #count bird Orphaned species
  OrphanedBIRD[[j]] <- mat.or.vec(nr=1, nc=7)
  OrphanedBIRD[[j]][1] <- length(which(apply(data.matrix(Network1R[[j]][,-1]), 2, sum)==0))
  OrphanedBIRD[[j]][2] <- length(which(apply(data.matrix(Network2R[[j]][,-1]), 2, sum)==0))
  OrphanedBIRD[[j]][3] <- length(which(apply(data.matrix(Network3R[[j]][,-1]), 2, sum)==0))
  OrphanedBIRD[[j]][4] <- length(which(apply(data.matrix(Network4R[[j]][,-1]), 2, sum)==0))
  OrphanedBIRD[[j]][5] <- length(which(apply(data.matrix(Network5R[[j]][,-1]), 2, sum)==0))
  OrphanedBIRD[[j]][6] <- length(which(apply(data.matrix(Network6R[[j]][,-1]), 2, sum)==0))
  OrphanedBIRD[[j]][7] <- length(which(apply(data.matrix(Network7R[[j]][,-1]), 2, sum)==0))
  
  
  #count lost (extinct) plant species
  Lost[[j]] <- mat.or.vec(nr=1, nc=6)
  
  #count lost (extinct) PLANT species
  Lost[[j]][1] <- length(Network1R[[j]][,1])-length(Network2R[[j]][,1])
  Lost[[j]][2] <- length(Network2R[[j]][,1])-length(Network3R[[j]][,1])
  Lost[[j]][3] <- length(Network3R[[j]][,1])-length(Network4R[[j]][,1])
  Lost[[j]][4] <- length(Network4R[[j]][,1])-length(Network5R[[j]][,1])
  Lost[[j]][5] <- length(Network5R[[j]][,1])-length(Network6R[[j]][,1])
  Lost[[j]][6] <- length(Network6R[[j]][,1])-length(Network7R[[j]][,1])
  
  #count lost (extinct) bird species
  LostBIRD[[j]] <- mat.or.vec(nr=1, nc=6)
  
  #count lost (extinct) BIRD species
  LostBIRD[[j]][1] <- length(Network1R[[j]][1,])-length(Network2R[[j]][1,])
  LostBIRD[[j]][2] <- length(Network2R[[j]][1,])-length(Network3R[[j]][1,])
  LostBIRD[[j]][3] <- length(Network3R[[j]][1,])-length(Network4R[[j]][1,])
  LostBIRD[[j]][4] <- length(Network4R[[j]][1,])-length(Network5R[[j]][1,])
  LostBIRD[[j]][5] <- length(Network5R[[j]][1,])-length(Network6R[[j]][1,])
  LostBIRD[[j]][6] <- length(Network6R[[j]][1,])-length(Network7R[[j]][1,])
  
  
  Original[[j]] <- mat.or.vec(nr=1, nc=6)
  Original[[j]][1] <- length(Network1R[[j]][,1])
  Original[[j]][2] <- length(Network2R[[j]][,1])
  Original[[j]][3] <- length(Network3R[[j]][,1])
  Original[[j]][4] <- length(Network4R[[j]][,1])
  Original[[j]][5] <- length(Network5R[[j]][,1])
  Original[[j]][6] <- length(Network6R[[j]][,1])
  
  #count initial "remaining" plant species
  Remain[[j]] <- mat.or.vec(nr=1, nc=6)
  
  #count remaining species
  Remain[[j]][1] <- length(Network2R[[j]][,1])
  Remain[[j]][2] <- length(Network3R[[j]][,1])
  Remain[[j]][3] <- length(Network4R[[j]][,1])
  Remain[[j]][4] <- length(Network5R[[j]][,1])
  Remain[[j]][5] <- length(Network6R[[j]][,1])
  Remain[[j]][6] <- length(Network7R[[j]][,1])
  
}
#this is the end of the 129 simulations



#when networks collapsed to no species within the simulations, we had to manually input the number of extinct and orphaned species 
#these steps are necessary because numbers of species lost and orphaned for these networks is incorrect otherwise
Lost[[68]][,5] <- 1 #at 6, 1 species is lost, 2 are orphaned
Lost[[68]][,6] <- 2 #at 7, 2 species are lost, none are orphaned
LostBIRD[[68]][,5] <- 1 #at 6, 1 species is lost, none are orphaned
LostBIRD[[68]][,6] <- 0 #at 7, 0 species is lost, none are orphaned
Lost[[18]][,5] <- 2 #at 6, 2 species lost, 2 species orphaned
LostBIRD[[18]][,5] <- 17 #at 6, 17 species lost, no species orphaned
Lost[[18]][,6] <- 2 #at 7, 2 species lost, no species orphaned
LostBIRD[[18]][,6] <- 0 #at 7, no species lost, no species orphaned
Lost[[19]][,5] <- 2 #at 6, 2 species lost, 2 species orphaned
LostBIRD[[19]][,5] <- 17 #at 6, 17 species lost, no species orphaned
Lost[[19]][,6] <- 2 #at 7, 2 species lost, no species orphaned
LostBIRD[[19]][,6] <- 0 #at 7, no species lost, no species orphaned
Lost[[3]][,5] <- 0 #at 6, no species lost, no species orphaned
Lost[[3]][,6] <- 1 #at 7, 1 species lost, no species orphaned
LostBIRD[[3]][,5] <- 14 #at 6, 14 species lost, no species orphaned
LostBIRD[[3]][,6] <- 0 #at 7, 0 species lost, no species orphaned
Lost[[13]][,6] <- 0 #no species lost, 1 species orphaned
LostBIRD[[13]][,6] <- 1 #1 species lost, no species orphaned
Lost[[97]][,6] <- 0 #no species lost, 1 species orphaned
LostBIRD[[97]][,6] <- 3 #3 species lost, no species orphaned
Lost[[102]][,6] <- 0 #no species lost, 1 species orphaned
LostBIRD[[102]][,6] <- 5 #5 species lost, no species orphaned
Lost[[110]][,6] <- 0 #no species lost, 2 species orphaned
LostBIRD[[110]][,6] <- 3 #3 species lost, no species orphaned
Lost[[112]][,6] <- 0 #no species lost, 4 species orphaned
LostBIRD[[112]][,6] <- 10 #10 species lost, no species orphaned


OrphanedBIRD[[3]][,6] <- 0 #this actually collapses at 6
OrphanedBIRD[[3]][,7] <- 0 #this actually collapses at 6
OrphanedBIRD[[13]][,7] <- 0
OrphanedBIRD[[18]][,6] <- 0 #this actually collapses at 6
OrphanedBIRD[[19]][,6] <- 0 #this actually collapses at 6
OrphanedBIRD[[18]][,7] <- 0 #this actually collapses at 6
OrphanedBIRD[[19]][,7] <- 0 #this actually collapses at 6
OrphanedBIRD[[22]][,6] <- 0 #this actually collapses at 6 - all of these species are orphaned at 5
OrphanedBIRD[[22]][,7] <- 0 #this actually collapses at 6
OrphanedBIRD[[68]][,6] <- 2 #this actually collapses at 6
OrphanedBIRD[[68]][,7] <- 0 #this actually collapses at 6
OrphanedBIRD[[97]][,7] <- 0
OrphanedBIRD[[102]][,6] <- 1 
OrphanedBIRD[[102]][,7] <- 0
OrphanedBIRD[[110]][,7] <- 0
OrphanedBIRD[[112]][,7] <- 0
Orphaned[[3]][,6] <- 0 #this actually collapses at 6
Orphaned[[3]][,7] <- 0 #this actually collapses at 6
Orphaned[[13]][,7] <- 1 
Orphaned[[18]][,6] <- 2 #this actually collapses at 6
Orphaned[[19]][,6] <- 2 #this actually collapses at 6
Orphaned[[18]][,7] <- 0 #this actually collapses at 6
Orphaned[[19]][,7] <- 0 #this actually collapses at 6
Orphaned[[22]][,6] <- 0 #this actually collapses at 6 - the one species is orphaned at 5
Orphaned[[22]][,7] <- 0 #this actually collapses at 6
Orphaned[[68]][,6] <- 2 #this actually collapses at 6
Orphaned[[68]][,7] <- 0 #this actually collapses at 6
Orphaned[[97]][,7] <- 1
Orphaned[[102]][,7] <- 1
Orphaned[[110]][,7] <- 2
Orphaned[[112]][,7] <- 4


#Number of species in network before area loss at each step
OriginalMat <- mat.or.vec(nr=129, nc=6)
for(i in 1:129){
  OriginalMat[i,] <- Original[[i]]
}

#Number of species remaining in network after area loss at each step
RemainMat <- mat.or.vec(nr=129, nc=6)
for(i in 1:129){
  RemainMat[i,] <- Remain[[i]]
}

#Number of plant species extirpated by area loss at each step
LostMat <- mat.or.vec(nr=129, nc=6)
for(i in 1:129){
  LostMat[i,] <- Lost[[i]]
}

#Number of bird species extirpated by area loss at each step
LostBIRDMat <- mat.or.vec(nr=129, nc=6)
for(i in 1:129){
  LostBIRDMat[i,] <- LostBIRD[[i]]
}

#Number of plant species orphaned by area loss at each step
OrphanedMat <- mat.or.vec(nr=129, nc=7)
for(i in 1:129){
  OrphanedMat[i,] <- Orphaned[[i]]
}
#All orphaned in the first column are Umbcal (Umbellularia californica), which is not connected to any frugivores in the network. The first column is the initial network, before the first area loss step. The first column was not used in analyses.

#Number of bird species orphaned by area loss at each step
OrphanedBIRDMat <- mat.or.vec(nr=129, nc=7)
for(i in 1:129){
  OrphanedBIRDMat[i,] <- OrphanedBIRD[[i]]
}