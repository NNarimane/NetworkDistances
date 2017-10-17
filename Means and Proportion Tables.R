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
Week=T
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
SharedDepartment=F



#########################
#### GET MEAN TABLES ####

cat("Get Min Distances Weighted and corresponding UnWeighted and GeoDist\n")
# MinimumDistances_PotentialInfector_byWeek_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
# MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeightedEquivalent=foreach(i=1:Week) %do% getMinimumUnWeightedDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
# MinimumDistances_PotentialInfector_byWeek_byMechanism_GeoDistEquivalent=foreach(i=1:Week) %do% getMinimumGeoDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
# load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism.RData")
# load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeightedEquivalent.RData")
# load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_GeoDistEquivalent.RData")
# load("Results/50 Permutations (Corrected)/MinimumDistances_PotentialInfector_byWeek_byMechanism.RData")
load(paste0(writingDir,"NonShared Departments/MinimumDistances_byWeek_byMechanism_byNonSharedDept.RData"))

cat("Get Permutations Min Distances Weighted and corresponding UnWeighted and GeoDist\n")
# load(paste0(writingDir,"50 Permutations (Reshuffled Department)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism.RData"))
# load(paste0(writingDir,"Mean Tables/AllRuns_MinimumUnWeightedDistances_CandidateTransmitters_Permutations_byMechanism.RData"))
# load(paste0(writingDir,"Mean Tables/AllRuns_MinimumGeoDistances_CandidateTransmitters_Permutations_byMechanism.RData"))
# load("Results/50 Permutations (Corrected)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment.RData")
load(paste0(writingDir,"NonShared Departments/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byWeek_byMechanism_byNonSharedDept.RData"))


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
# rownames(FinalTable)=c("Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8")
# names(FinalTable)=c("Means_Observed","Means_Permutations","Min_Observed",
#                     "Max_Observed","Min_Permutations", "Max_Permutations",
#                     "ProportionUnder5thPercentile")
write.csv(FinalTable, file = paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2011 Data Sliding Week)/FinalTable.csv"))


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



##############Plots

#2011
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2011 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2011 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#Observed Data
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
cat("Mean distance for every Week N for candidate transmitters\n")
MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

#Permutations
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 

#Plot
par(mfrow=c(3,2))
plot(unlist(MinimumDistances_MeansByNWeeks), type='l', col="red", ylim=c(3.4,7.2), 
     ylab="Avg. Shortest Dist. All Ep.", 
     xlab="1st Day of 1-Week Sliding Window",
     main="2011 CPE Episodes (n=108) & 50 Permutations")
for(i in 1:length(RandomSimulationsByWeeks)){
  lines(unlist(RandomSimulationsByWeeks_Means[[i]]), col=rgb(0, 0, 1, 0.4))
}
grid(nx = 15, ny = 5, col = "gray")

#2012
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2012 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2012 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#Observed Data
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
cat("Mean distance for every Week N for candidate transmitters\n")
MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

#Permutations
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 

#Plot
plot(unlist(MinimumDistances_MeansByNWeeks), type='l', col="red", ylim=c(3, 4.5), 
     ylab="Avg. Shortest Dist. All Ep.", 
     xlab="1st Day of 1-Week Sliding Window",
     main="2012 CPE Episodes (n=240) & 50 Permutations")
for(i in 1:length(RandomSimulationsByWeeks)){
  lines(unlist(RandomSimulationsByWeeks_Means[[i]]), col=rgb(0, 0, 1, 0.4))
}
grid(nx = 15, ny = 5, col = "gray")

#2013
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2013 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2013 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#Observed Data
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
cat("Mean distance for every Week N for candidate transmitters\n")
MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

#Permutations
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 

#Plot
# par(mfrow=c(2,2))
plot(unlist(MinimumDistances_MeansByNWeeks), type='l', col="red", ylim=c(2.8, 3.85), 
     ylab="Avg. Shortest Dist. All Ep.", 
     xlab="1st Day of 1-Week Sliding Window",
     main="2013 CPE Episodes (n=401) & 50 Permutations")
for(i in 1:length(RandomSimulationsByWeeks)){
  lines(unlist(RandomSimulationsByWeeks_Means[[i]]), col=rgb(0, 0, 1, 0.4))
}
grid(nx = 15, ny = 5, col = "gray")

#2014
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#Observed Data
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
cat("Mean distance for every Week N for candidate transmitters\n")
MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

#Permutations
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 

#Plot
# par(mfrow=c(2,1))
plot(unlist(MinimumDistances_MeansByNWeeks), type='l', col="red", ylim=c(3, 3.85), 
     ylab="Avg. Shortest Dist. All Ep.", 
     xlab="1st Day of 1-Week Sliding Window",
     main="2014 CPE Episodes (n=668) & 50 Permutations")
for(i in 1:length(RandomSimulationsByWeeks)){
  lines(unlist(RandomSimulationsByWeeks_Means[[i]]), col=rgb(0, 0, 1, 0.4))
}
grid(nx = 15, ny = 5, col = "gray")

#2015
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#Observed Data
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
cat("Mean distance for every Week N for candidate transmitters\n")
MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

#Permutations
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 

#Plot
plot(unlist(MinimumDistances_MeansByNWeeks), type='l', col="red", ylim=c(2.3, 3.2), 
     ylab="Avg. Shortest Dist. All Ep.", 
     xlab="1st Day of 1-Week Sliding Window",
     main="2015 CPE Episodes (n=948) & 50 Permutations")
for(i in 1:length(RandomSimulationsByWeeks)){
  lines(unlist(RandomSimulationsByWeeks_Means[[i]]), col=rgb(0, 0, 1, 0.4))
}
grid(nx = 15, ny = 5, col = "gray")

#2016
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2016 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2016 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#Observed Data
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
cat("Mean distance for every Week N for candidate transmitters\n")
MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

#Permutations
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 

#Plot
plot(unlist(MinimumDistances_MeansByNWeeks), type='l', col="red", ylim=c(2.8, 3.8), 
     ylab="Avg. Shortest Dist. All Ep.", 
     xlab="1st Day of 1-Week Sliding Window",
     main="2016 CPE Episodes (n=378) & 50 Permutations")
for(i in 1:length(RandomSimulationsByWeeks)){
  lines(unlist(RandomSimulationsByWeeks_Means[[i]]), col=rgb(0, 0, 1, 0.4))
}
grid(nx = 15, ny = 5, col = "gray")
