################################################################
##### Describing Pairs of Episodes and Potential Infectors #####
################################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Original or Transformed Weights\n")
Transformed=T

cat("Upload Department Contact Network\n")
if(Transformed){
  cat("Upload Department Network with Transformed Weights\n")
  load("Data/Department Network (Transformed).RData")
}else{
  cat("Upload Department Contact Network without Transformations\n")
  load("../Hospital_Network/HospitalNetwork/Data/Department Network.RData")
}

###################
#### LOAD DATA ####

cat("Choose year to analyze\n")
Year="2015"

cat("Choose start date\n")
startDate=paste0(Year, "-01-01")

cat("Choose end date\n")
endDate=paste0(Year, "-12-30")

cat("Get CPE Data with Mechanism and Class Info\n")
data=getCPEData()

#########################
#### LOAD CANDIDATES ####

cat(paste("Load", Year, "Candidates\n"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Days 1-30)/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled.RData"))

cat(paste("Load", Year, "Minimum Distances\n"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Days 1-30)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled.RData"))

#######################
#### GET ALL PAIRS ####

cat("Numer of cores to use\n")
cores=5

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

cat("RUN PARALLEL\n")
AllPairs=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding), .packages = c("igraph", "foreach")) %dopar% {
  getPairsByWindow(i, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

cat("Save or load data\n")
save(AllPairs, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week) Class/AllPairs.RData"))
load(file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Days 1-30)/AllPairs.RData"))

########################
#### Seperate Pairs ####

cat("Get first element i of each list\n")
AllPairs_Episodes=foreach(i=1:length(AllPairs[[1]])) %do% lapply(AllPairs, `[[`, i) 
Pairs_Episodes=AllPairs_Episodes[[1]]
Pairs_Episodes_Imported=AllPairs_Episodes[[2]]

###############################
#### Proportions Explained ####

ProportionsTable=foreach(i=1:length(Pairs_Episodes), .combine = "rbind") %do% {
  cat("Total episodes linked out of total episodes\n")
  TotalLinked=nrow(Pairs_Episodes[[i]])/nrow(data)
  
  cat("Total imported episodes linked out of total imported episodes\n")
  data_Imported=data[which(data$Imported == "O"),]
  TotalLinkedImported=nrow(Pairs_Episodes_Imported[[i]])/nrow(data_Imported)
  
  cat("Total non-imported episodes linked out of total non-imported episodes\n")
  NonImportedEpisodes=data$Episode[which(data$Imported == "N")]
  NonImportedEpisodesWithSource=NonImportedEpisodes[NonImportedEpisodes %in% Pairs_Episodes[[i]][,2]]
  TotalLinkedNonImported=length(NonImportedEpisodesWithSource)/length(NonImportedEpisodes)
  
  cat("Cbind\n")
  Table=c(TotalLinked, TotalLinkedImported, TotalLinkedNonImported)
}

ProportionsTable=as.data.frame(ProportionsTable)
colnames(ProportionsTable)=c("% Episodes Linked Out of Total",
                             "% Imported Episodes as Source",
                             "% Non-Imported Episodes as Targets")
write.csv(ProportionsTable, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week) Class/Proportion Table of All Windows.csv"))

Mechanisms1=as.data.frame(cbind(table(data$Mechanism), prop.table(table(data$Mechanism))))
Mechanisms2=as.data.frame(cbind(table(data$Mechanism[which(data$Imported == "N")]), prop.table(table(data$Mechanism[which(data$Imported == "N")]))))
write.csv(Mechanisms, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week) Class/Mechanisms.csv"))

FirstClass=as.data.frame(cbind(table(data$FirstClass), prop.table(table(data$FirstClass))))
write.csv(FirstClass, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week) Class/FirstClasss.csv"))

SecondClass=as.data.frame(cbind(table(data$SecondClass), prop.table(table(data$SecondClass))))
ThirdClass=as.data.frame(cbind(table(data$ThirdClass), prop.table(table(data$ThirdClass))))


##############################
#### Description of Pairs ####

cat("Get distances between pairs\n")
PairsDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Window]]
NonNAPairsDistances=PairsDistances[complete.cases(PairsDistances)]

cat("Get Table of Pairs and Distances\n")
PairsDistanceTable=as.data.frame(cbind(Pairs_Episodes, NonNAPairsDistances))
cat("Number of different sources\n")
length(levels(PairsDistanceTable$V1))
cat("Number of different targets\n")
length(levels(PairsDistanceTable$V2))

cat("Add info to table\n")
PairsDistanceTable$V1=as.character(PairsDistanceTable$V1)
PairsDistanceTable$V2=as.character(PairsDistanceTable$V2)
PairsDistanceTable$NonNAPairsDistances=as.numeric(as.character(PairsDistanceTable$NonNAPairsDistances))
PairsDistanceTable$SourceImported=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Imported[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$SourceDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$TargetDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V2[[i]])]
PairsDistanceTable$Mechanism=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Mechanism[which(data$Episode == PairsDistanceTable$V1[[i]])]
colnames(PairsDistanceTable)=c("Source","Target","ShortestPathDistance","SourceImported","SourceDepartment","TargetDepartment","Mechanism")
PairsDistanceTable=PairsDistanceTable[,c("Source","SourceImported","SourceDepartment","ShortestPathDistance","TargetDepartment","Target","Mechanism")]

cat("Save or load\n")
# write.csv(PairsDistanceTable, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/PairsDistanceInfoTable.csv"))
# load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/PairsDistanceInfoTable.csv"))

##########################
#### Summary of Pairs ####


cat("Average Length Between Pairs\n")
Mean=mean(NonNAPairsDistances)
cat("Range\n")
Min=min(NonNAPairsDistances)
NonZeroMin=min(NonNAPairsDistances[NonNAPairsDistances > 0])
Max=max(NonNAPairsDistances)
cat("CI\n")
Upper_CI=Mean + 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))
Lower_CI=Mean - 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))

cat("Total episodes linked out of total episodes\n")
TotalLinked=nrow(Pairs_Episodes)/nrow(data)
cat("How many episodes are explained\n")
prop.table(table(unlist(lapply(PotentialInfectors, is.null))))

cat("Total imported episodes linked out of total imported episodes\n")
TotalLinkedImported=nrow(Pairs_Episodes_Imported)/nrow(data_Imported)

cat("Total non-imported episodes linked out of total non-imported episodes\n")
NonImportedEpisodes=data$Episode[which(data$Imported == "N")]
NonImportedEpisodesWithSource=NonImportedEpisodes[NonImportedEpisodes %in% Pairs_Episodes[,2]]
TotalLinkedNonImported=length(NonImportedEpisodesWithSource)/length(NonImportedEpisodes)

cat("Summary Table\n")
SummaryTable=as.data.frame(rbind(nrow(data), nrow(data_Imported), length(NonImportedEpisodes), length(NonNAPairsDistances), Mean, Min, NonZeroMin, Max, Upper_CI, Lower_CI, TotalLinked,
                                 TotalLinkedImported, TotalLinkedNonImported))
rownames(SummaryTable)=c(paste("All", Year, "Episodes"), paste("All", Year, "Imported Episodes"),"All 2015 Non-Imported Episodes","Number of Pairs","Mean Distance","Min","NonZeroMin","Max","Upper_CI","Lower_CI","% Episodes Linked Out of Total",
                         "% Imported Episodes as Source","% Non-Imported Episodes as Targets")
colnames(SummaryTable)=paste(Year, "Pairs at Day 21-28")

cat("Save or load\n")
# write.csv(SummaryTable, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/SummaryTable.csv"))
# load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/SummaryTable.csv"))
