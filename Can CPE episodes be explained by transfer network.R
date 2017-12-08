##############################################################
##### Can CPE episodes be explained by transfer network? #####
##############################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Original or Transformed Weights\n")
Transformed=T
cat("If Trans Original or Inverse Transfer Weights\n")
InvTrans=T
Prob=T

cat("Upload Department Contact Network\n")
if(Transformed){
  if(InvTrans){
    if(Prob){
      cat("Upload Department Network with Transformed Prob of Transfer Weights\n")
      load("Data/Department Network (Prob of Transfers).RData")
    }else{
      cat("Upload Department Network with Transformed Inverse Transfer Weights\n")
      load("Data/Department Network (Inverse Transfers).RData")
    }
  }else{
    cat("Upload Department Network with Transformed Weights\n")
    load("Data/Department Network (Transformed).RData")
  }
}else{
  cat("Upload Department Contact Network without Transformations\n")
  load("../Hospital_Network/HospitalNetwork/Data/Department Network.RData")
}

load("Data/Department Network (Prob K).RData")

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
  cat("Sliding Week(s) by Days\n")
  Sliding=T
  WeekSlide=1
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
Nruns=100

cat("Mechanism or Class\n")
Mechanism=T

cat("Shared Department Between Incident Episodes and Candidates?\n")
SharedDepartment=T

cat("Reshuffled or Reassigned Departments?\n")
Reshuffled=T


##########################
#### STEP 1: CPE DATA ####

cat("Choose start date\n")
startDate="2012-01-01"

cat("Choose end date\n")
endDate="2012-12-31"

cat("Get CPE Data with Mechanism and Class Info\n")
data=getCPEData()

#####################
#### SAVE FOLDER ####

folder="2012 Final Observed and Permutation Results DEPT NET TRANS"

############################################
#### STEP 2: GET CANDIDATE TRANSMITTERS ####

NonPermutation=T
CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding=foreach(i=1:Day) %do% getCandidateTransmitters(data=data, i)
MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding=foreach(i=1:length(CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding)) %do% getMinimumDistances(i, CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[i]], weights = weights, algorithm = algorithm)

save(CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding, file = paste0(writingDir,folder,"/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
save(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, file = paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

############################################

# cat("Numer of cores to use\n")
# cores=5
# 
# cat("Make clusters for parallel\n")
# cl=makeCluster(cores)
# registerDoSNOW(cl)
# getDoParWorkers()
# 
# cat("RUN PARALLEL\n")
# AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding <- foreach(icount(Nruns), .packages=c('igraph', 'foreach')) %dopar% {
#   NonPermutation=F
#   Week=F
#   Day=30
#   Sliding=T
#   WeekSlide=2
#   cat("Get  Random Candidate Transmitters from Permutations\n")
#   CandidateTransmitters_Permutations=foreach(j=1:Day) %do% getCandidateTransmitters(data=data, j)
# }
# 
# cat("Stop parallel\n")
# stopCluster(cl)
# print("Cluster stopped")
# registerDoSEQ()
# 
# save(AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding Week)/AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
# load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding 3 Week)/AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

############################################

#Parallel
cat("Numer of cores to use\n")
cores=5

cat("Runs to test\n")
Nruns=Nruns

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
  cat("Parameters\n")
  NonPermutation=F
  Week=F
  Day=30
  Sliding=T
  WeekSlide=1
  
  cat("Get  Random Candidate Transmitters from Permutations\n")
  CandidateTransmitters_Permutations=foreach(j=1:Day) %do% getCandidateTransmitters(data=data, j)
  
  cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
  MinimumDistances_CandidateTransmitters_Permutations=foreach(k=1:length(CandidateTransmitters_Permutations)) %do% getMinimumDistances(k, CandidateTransmitters_Permutations[[k]], weights = weights, algorithm = algorithm)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding, file=paste0(writingDir,folder,"/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#############################################################
#### STEP 3: AVERAGE MINIMUM DISTANCES FROM PERMUATIONS ####

cat("Get Average of Minimum Distance (Between Potential Infector and Case)\n")
Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=getAverageMinDistances_CandidateTransmitters_Permutations(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding)

#################################################
#### STEP 4: WILCOXON PAIRED RANKED SUM TEST ####

cat("Wilcoxon Rank Sum Test\n")
CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding)) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)

cat("Create Table of Statistically Significant and Non-Sig Results\n")
WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
Results=as.data.frame(WilcoxonPairedRankTestPValues)
Results$LowerBoundDay=1:Day
Results$UpperBound=7+1:Day
# Results$Day=1:30
Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05
#If stat. sig. p-value results (TRUE), reject H0 (meaning that the distributions differ)

cat("Save Results\n")
write.table(Results, file=paste0(writingDir,folder,"/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")

#######################################
#### INTERPRETATION OF THE RESULTS ####

# OBJECTIVE: Determine if transfer network is linked to CPE episodes
# Step 1: Calculate the minimum network distances of all potential infectors
# Step 2: Does simulation of random networks give us a different result?
# Step 3: Results: some results do show a difference
# Step 4: Conclude: Department transfer network does not better explain CPE episodes



