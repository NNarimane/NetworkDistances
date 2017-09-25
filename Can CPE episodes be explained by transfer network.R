##############################################################
##### Can CPE episodes be explained by transfer network? #####
##############################################################

#################################
#### INSTALL & LOAD PACKAGES ####

cat("Load Packages\n")
library("igraph")
library("stringr")
library("foreach")
library("doSNOW")
library("snow")

#########################
#### SET ENVIRONMENT ####

cat("Set Working Environment\n")
envNN=T
envNNwindows=T
if(envNN){
  if(envNNwindows){
    currentwd=setwd("C:/Users/Narimane/Dropbox/CPE Transmission Chains/")
  }else{
    currentwd=setwd("/Users/narimanenekkab/Dropbox/CPE Transmission Chains/")
  }
}else{
  currentwd=setwd("/Users/pascalcrepey/Google Drive/1-EPC/stageNN/") 
}

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("CPETransmissionChains/Generation Time Sensitivity Analysis Functions.R", 
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

##########################
#### STEP 1: CPE DATA ####

# cat("Get CPE Data with Mechanism and Class Info\n")
# data=getCPEData()

# cat("Save Data\n")
# save(data, file = "Data/CPE Data with Mechanism Info.RData")

cat("Load Data\n")
load("Data/CPE Data with Mechanism Info.RData")

cat("Fix Department Variable")
data$Department=str_pad(data$Department, 2, pad = "0")

cat("Removing Episodes Occuring in Depts Other Than Depts in Network")
data=data[which(data$Department %in% V(directed.graph_Dept)$name),]

cat("Renaming Rows and Episode Numbers")
data$Episode=1:nrow(data)
rownames(data)=1:nrow(data)

############################################
#### STEP 2: GET CANDIDATE TRANSMITTERS ####

cat("Set Maximum Number of Days to Test\n")
Week=8

cat(paste("Get Candidate Transmitters for Week 1 to", Week, "by Mechanism\n"))
CandidateTransmitters_byWeek_byMechanism=foreach(i=1:Week) %do% getCandidateTransmitters_byWeek_byMechanism(i)

###################################
#### STEP 3: GET MIN DISTANCES ####

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

cat("Get Min Distances\n")
if(Weighted){
      cat("Get Weighted Distances for Candidate Transmitters by Mechanism\n")
      MinimumDistances_PotentialInfector_byWeek_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
    }else{
      cat("Get UnWeighted Distances for Candidate Transmitters by Mechanism\n")
      MinimumDistances_PotentialInfector_byWeek_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
    }
 

#####################################################################
#### STEP 4a: GET RANDOM CANDIDATE TRANSMITTERS FROM PREMUATIONS ####

#cat("Number of Simulations\n")
#Nruns=200

#cat("Get All Runs of Weekly Candidate Transmitters from Permutations\n")
#AllRuns_CandidateTransmitters_Permutations_byMechanism <- foreach(runs=rep(1,Nruns)) %do% {
#  cat("Get Weekly Random Candidate Transmitters from Permutations\n")
#  CandidateTransmitters_Permutations_byWeek=foreach(i=1:Week) %do% getCandidateTransmitters_Permutations_byWeek(i)
#} 

#Parallel
cat("Numer of cores to use\n")
cores=5

cat("Maximum meanGT time to test\n")
Nruns=5

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

cat("RUN PARALLEL\n")
AllRuns_CandidateTransmitters_Permutations_byMechanism <- foreach(icount(Nruns), .packages=c('igraph', 'foreach')) %dopar% {
  cat("Get Weekly Random Candidate Transmitters from Permutations\n")
  CandidateTransmitters_Permutations_byWeek=foreach(j=1:Week) %do% getCandidateTransmitters_Permutations_byWeek(j)
} 

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

#########################################################
#### STEP 4b: GET MINIMUM DISTANCES FROM PREMUATIONS ####

#cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
#AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(run=1:Nruns) %do% {
#  OneRun_CandidateTransmitters_Permutations_byMechanism=AllRuns_CandidateTransmitters_Permutations_byMechanism[[run]]
#  MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, OneRun_CandidateTransmitters_Permutations_byMechanism[[i]], weights = weights, algorithm = algorithm)
#} 


#Parallel
cat("Numer of cores to use\n")
cores=5

cat("Maximum meanGT time to test\n")
#Nruns=5

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

cat("RUN PARALLEL\n")
AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
  cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
  OneRun_CandidateTransmitters_Permutations_byMechanism=AllRuns_CandidateTransmitters_Permutations_byMechanism[[run]]
  MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(j=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(j, OneRun_CandidateTransmitters_Permutations_byMechanism[[j]], weights = weights, algorithm = algorithm)
} 

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

#############################################################
#### STEP 4c: AVERAGE MINIMUM DISTANCES FROM PREMUATIONS ####

cat("Get Average of Minimum Distance (Between Potential Infector and Case)\n")
Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=getAverageMinDistances_CandidateTransmitters_Permutations_byWeek(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)


#########################################
#### WILCOXON PAIRED RANKED SUM TEST ####

cat("Wilcoxon Rank Sum Test\n")
CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:Week) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_PotentialInfector_byWeek_byMechanism, Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)

cat("Create Table of Statistically Significant and Non-Sig Results\n")
WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
Results=as.data.frame(WilcoxonPairedRankTestPValues)
Results$Week=1:Week
Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05
#If stat. sig. p-value results (TRUE), reject H0 (meaning that the distributions differ)

cat("Save Results\n")
write.csv(Results, file="Data/Wilcoxon Rank Sum Test Results for Week 1 to",Week," for", Nruns," Permutations by Mechanism.csv", row.names = F)

#######################################
#### INTERPRETATION OF THE RESULTS ####

# OBJECTIVE: Determine if transfer network is linked to CPE episodes
# Step 1: Calculate the minimum network distances of all potential infectors
# Step 2: Does simulation of random networks give us a different result?
# Step 3: Results: some results do show a difference
# Step 4: Conclude: Department transfer network does not better explain CPE episodes


#############################################################################################
#### Using Distribution of 100 comulations: Compare where potential infector values fall ####
################ Calculate proportions, and values of shortest path #########################
#############################################################################################

cat("Get Proportion Tables\n")
ProportionTables_byMechanism=get5thQuantiles(Week, MinimumDistances_PotentialInfector_byWeek_byMechanism, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)
MeanMinimumDistances_byMechanism=getMeanMinimumDistances(MinimumDistances_PotentialInfector_byWeek_byMechanism, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)
ProportionTables_withMinDistances_byMechanism=cbind(ProportionTables_byMechanism[2,], MeanMinimumDistances_byMechanism)
colnames(ProportionTables_withMinDistances_byMechanism)=c("ProportionLessThan5thPercentile", "MinimumDistances_MeansByNWeeks", "RandomPermutationsByDays_MeansByNWeeks")


write.csv(ProportionTables_withMinDistances_byMechanism, file = "Data/ProportionTables_withMinDistances_byMechanism.csv")
