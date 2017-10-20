#############################################################################################
#### Using Distribution of N simulations: Compare where potential infector values fall ######
################ Calculate proportions, and values of shortest path #########################
#############################################################################################

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

####################
#### PARAMETERS ####


cat("Set Maximum Number of Days or Weeks to Test\n")
Week=F
if(Week){
  cat("Set Maximum Number of Weeks to Test\n")
  Week=8
}else{
  cat("Set Maximum Number of Days to Test\n")
  Day=30
}

cat("Weighted or Un-Weighted Shortest Path Calculations\n")
Weighted=T
if(Weighted){
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
}else{
  cat("Disable Weights and Algorithm to Calculate Shortest Paths\n")
  weights = NA
  algorithm = "automatic"
}

cat("Number of Simulations\n")
Nruns=50

cat("Mechanism or Class\n")
Mechanism=T

cat("Shared Department Between Incident Episodes and Candidates?\n")
SharedDepartment=T

cat("Choose start date\n")
startDate="2015-01-01"

cat("Choose end date\n")
endDate="2015-12-30"

cat("Get CPE Data with Mechanism and Class Info\n")
data=getCPEData()

####################################
#### GET MIN SHORTEST DISTANCES ####

cat("Get Observed Min Distances\n")
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat("Get Permutations Min Distances\n")
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

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
MeanMinimumDistancesTable=getMeanMinimumDistances_Simple(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding)
cat("Get Range Table\n")
RangeTable=getRange(Day, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding)
cat("Get Proportion Tables\n")
ProportionTables=get5thQuantiles(Day, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding)

cat("Merge Tables\n")
FinalTable=cbind(Results, MeanMinimumDistancesTable, RangeTable, ProportionTables[2,])
write.table(FinalTable, file = paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/FinalTable.csv"), sep=",")


