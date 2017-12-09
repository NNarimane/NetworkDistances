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
#### WORKING DIRECTORY ####

folder=folder

####################################
#### GET MIN SHORTEST DISTANCES ####

cat("Get Observed Min Distances\n")
load(paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat("Get Permutations Min Distances\n")
load(paste0(writingDir,folder,"/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#########################
#### Get Mean Tables ####
#########################

cat("Get Mean Table\n")
MeanMinimumDistancesTable=getMeanMinimumDistances(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding)

cat("Get Range Table\n")
RangeTable=getRange(Day, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding)

cat("Get Proportion Tables\n")
ProportionTables=get5thQuantiles(Day, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding)

cat("Merge Tables\n")
FinalTable=cbind(Results, MeanMinimumDistancesTable, RangeTable, ProportionTables[2,])

cat("Save\n")
write.table(FinalTable, file = paste0(writingDir,folder,"/FinalTable.csv"), sep=",")


