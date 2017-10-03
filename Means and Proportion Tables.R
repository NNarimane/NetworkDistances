#############################################################################################
#### Using Distribution of N simulations: Compare where potential infector values fall ######
################ Calculate proportions, and values of shortest path #########################
#############################################################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

##################
#### GET DATA ####

# load("Results/CandidateTransmitters_byWeek_byMechanism.RData")
# load("Results/50 Permutations (Reshuffled Department)/AllRuns_CandidateTransmitters_Permutations_byMechanism.RData")

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

cat("Load Data\n")
load("Data/CPE Data with Mechanism Info.RData")

cat("Fix Department Variable")
data$Department=str_pad(data$Department, 2, pad = "0")

cat("Removing Episodes Occuring in Depts Other Than Depts in Network")
data=data[which(data$Department %in% V(directed.graph_Dept)$name),]

cat("Renaming Rows and Episode Numbers")
data$Episode=1:nrow(data)

########################
#### SET PARAMETERS ####

cat("Set Maximum Number of Days to Test\n")
Week=8

cat("Weighted or Un-Weighted Shortest Path Calculations\n")
Weighted=T
if(Weighted){
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  load("Data/Department Network (Transformed).RData")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
}else{
  cat("Disable Weights and Algorithm to Calculate Shortest Paths\n")
  weights = NA
  algorithm = "automatic"
}

cat("Runs tested\n")
Nruns=50

#########################
#### GET MEAN TABLES ####

cat("Get Min Distances Weighted and corresponding UnWeighted and GeoDist\n")
# MinimumDistances_PotentialInfector_byWeek_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
# MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeightedEquivalent=foreach(i=1:Week) %do% getMinimumUnWeightedDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
# MinimumDistances_PotentialInfector_byWeek_byMechanism_GeoDistEquivalent=foreach(i=1:Week) %do% getMinimumGeoDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
# load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism.RData")
# load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeightedEquivalent.RData")
# load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_GeoDistEquivalent.RData")
load("Results/50 Permutations (Corrected)/MinimumDistances_PotentialInfector_byWeek_byMechanism.RData")

cat("Get Permutations Min Distances Weighted and corresponding UnWeighted and GeoDist\n")
# load(paste0(writingDir,"50 Permutations (Reshuffled Department)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism.RData"))
# load(paste0(writingDir,"Mean Tables/AllRuns_MinimumUnWeightedDistances_CandidateTransmitters_Permutations_byMechanism.RData"))
# load(paste0(writingDir,"Mean Tables/AllRuns_MinimumGeoDistances_CandidateTransmitters_Permutations_byMechanism.RData"))
load("Results/50 Permutations (Corrected)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment.RData")


#########################
#### Get Mean Tables ####
#########################

# cat("Get Mean Table\n")
# MeanMinimumDistancesTable=getMeanMinimumDistances(MinimumDistances_PotentialInfector_byWeek_byMechanism,
#                                                   MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeightedEquivalent,
#                                                   MinimumDistances_PotentialInfector_byWeek_byMechanism_GeoDistEquivalent,
#                                                   AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism,
#                                                   AllRuns_MinimumUnWeightedDistances_CandidateTransmitters_Permutations_byMechanism,
#                                                   AllRuns_MinimumGeoDistances_CandidateTransmitters_Permutations_byMechanism)

cat("Get Mean Table\n")
MeanMinimumDistancesTable=getMeanMinimumDistances_Simple(MinimumDistances_PotentialInfector_byWeek_byMechanism, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment)
cat("Get Range Table\n")
RangeTable=getRange(Week, MinimumDistances_PotentialInfector_byWeek_byMechanism, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment)

FinalTable=cbind(MeanMinimumDistancesTable, RangeTable)
rownames(FinalTable)=c("Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8")
write.csv(FinalTable, paste0(writingDir,"50 Permutations (Corrected)/FinalTable.csv"))

# cat("Get Proportion Tables\n")
# ProportionTables_byMechanism=get5thQuantiles(Week, MinimumDistances_PotentialInfector_byWeek_byMechanism, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment)
# 
# cat("Merge Tables\n")
# FinalTable=cbind(ProportionTables_byMechanism[2,], MeanMinimumDistancesTable)
# names(FinalTable)
# colnames(FinalTable)=c("PropMinDistUnder5thQuantPermutations",
#                        "MinDist","UnWeightedMinDist","GeoDistMinDist",
#                        "AvgPermMinDist","UnWeightedAvgPerm","GeoDistAvgPerm")
# FinalTable=FinalTable[,c("PropMinDistUnder5thQuantPermutations",
#                          "MinDist","AvgPermMinDist",
#                          "UnWeightedMinDist","UnWeightedAvgPerm",
#                          "GeoDistMinDist","GeoDistAvgPerm")]
# rownames(FinalTable)=c("Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8")
# 
# cat("Merge Tables for NonZero Distances\n")
# FinalTable_NonZeroDistances=cbind(ProportionTables_byMechanism_NonZeroDistances[2,], MeanMinimumDistancesTable_NonZeroDistances)
# names(FinalTable_NonZeroDistances)
# colnames(FinalTable_NonZeroDistances)=c("PropMinDistUnder5thQuantPermutations",
#                        "MinDist","UnWeightedMinDist","GeoDistMinDist",
#                        "AvgPermMinDist","UnWeightedAvgPerm","GeoDistAvgPerm")
# FinalTable_NonZeroDistances=FinalTable_NonZeroDistances[,c("PropMinDistUnder5thQuantPermutations",
#                          "MinDist","AvgPermMinDist",
#                          "UnWeightedMinDist","UnWeightedAvgPerm",
#                          "GeoDistMinDist","GeoDistAvgPerm")]
# rownames(FinalTable_NonZeroDistances)=c("Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8")
# 
# cat("Save Tables\n")
# write.csv(FinalTable, paste0(writingDir,"FinalTable.csv"))
# write.csv(FinalTable_NonZeroDistances, paste0(writingDir,"FinalTable_NonZeroDistances.csv"))
# 
