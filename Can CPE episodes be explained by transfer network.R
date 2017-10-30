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
  cat("Sliding Week(s) by Days\n")
  Sliding=F
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
Nruns=50

cat("Mechanism or Class\n")
Mechanism=F

cat("Shared Department Between Incident Episodes and Candidates?\n")
SharedDepartment=T

cat("Reshuffled or Reassigned Departments?\n")
Reshuffled=T


##########################
#### STEP 1: CPE DATA ####

cat("Choose start date\n")
startDate="2015-01-01"

cat("Choose end date\n")
endDate="2015-12-30"

cat("Get CPE Data with Mechanism and Class Info\n")
data=getCPEData()

# cat("Save Data\n")
# save(data, file = "Data/CPE Data with Mechanism Info.RData")

# cat("Load Data\n")
# load("Data/CPE Data with Mechanism Info.RData")

############################################
#### STEP 2: GET CANDIDATE TRANSMITTERS ####

NonPermutation=T
CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled=foreach(i=1:Day) %do% getCandidateTransmitters(data=data, i)
MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled=foreach(i=1:length(CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled)) %do% getMinimumDistances(i, CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled[[i]], weights = weights, algorithm = algorithm)

save(CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled, file = paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week) Class/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled.RData"))
save(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled, file = paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week) Class/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled.RData"))

load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled.RData"))

############################################

cat("Numer of cores to use\n")
cores=5

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

cat("RUN PARALLEL\n")
AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled <- foreach(icount(Nruns), .packages=c('igraph', 'foreach')) %dopar% {
  NonPermutation=F
  Week=F
  Day=30
  Sliding=F
  cat("Get  Random Candidate Transmitters from Permutations\n")
  CandidateTransmitters_Permutations=foreach(j=1:Day) %do% getCandidateTransmitters(data=data, j)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled.RData"))

############################################

#Parallel
cat("Numer of cores to use\n")
cores=4

cat("Runs to test\n")
# Nruns=Nruns
Nruns=length(AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled)-25

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_P2=foreach(run=26:50, .packages=c('igraph', 'foreach')) %dopar% {
  cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
  OneRun_CandidateTransmitters_Permutations=AllRuns_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled[[run]]
  MinimumDistances_CandidateTransmitters_Permutations=foreach(j=1:length(OneRun_CandidateTransmitters_Permutations)) %do% getMinimumDistances(j, OneRun_CandidateTransmitters_Permutations[[j]], weights = weights, algorithm = algorithm)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_P2, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_P2.RData"))

load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_P1.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_P2.RData"))

AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled=append(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_P1, AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_P2)
save(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled.RData"))

###########################################

# if(Week){
#   cat(paste("Get Candidate Transmitters for Week 1 to", Week, "by Mechanism\n"))
#   NonPermutation=T
#   # CandidateTransmitters_byWeek_byMechanism=foreach(i=1:Week) %do% getCandidateTransmitters_byWeek_byMechanism_Reassignment(data=data, i)
#   # save(CandidateTransmitters_byWeek_byMechanism, file="Results/50 Permutations (Corrected)/CandidateTransmitters_byWeek_byMechanism.RData")
#   load("Results/50 Permutations (Corrected)/CandidateTransmitters_byWeek_byMechanism.RData")
# }else{
#   cat(paste("Get Candidate Transmitters for Day 1 to", Day, "by Mechanism\n"))
#   NonPermutation=T
#   CandidateTransmitters_byDay_byMechanism=foreach(i=1:Day) %do% getCandidateTransmitters_byDay_byMechanism_Reassignment(data=data, i)
#   save(CandidateTransmitters_byDay_byMechanism, file="Results/30 Permutations (Corrected) Days/CandidateTransmitters_byDay_byMechanism.RData")
#   # load("Results/50 Permutations (Corrected)/CandidateTransmitters_byDay_byMechanism.RData")
# }

###################################
#### STEP 3: GET MIN DISTANCES ####

# if(Week){
#   cat("Get Weekly Min Distances\n")
#   if(Weighted){
#     cat("Get Weighted Distances for Candidate Transmitters by Mechanism\n")
#     # MinimumDistances_PotentialInfector_byWeek_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
#     # save(MinimumDistances_PotentialInfector_byWeek_byMechanism, file="Results/50 Permutations (Corrected)/MinimumDistances_PotentialInfector_byWeek_byMechanism.RData")
#     load("Results/50 Permutations (Corrected)/MinimumDistances_PotentialInfector_byWeek_byMechanism.RData")
#   }else{
#     cat("Get Un-Weighted Distances for Candidate Transmitters by Mechanism\n")
#     # MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
#     # save(MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted, file="Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted.RData")
#     # load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted.RData")
#   }
# }else{
#   cat("Get Daily Min Distances\n")
#   if(Weighted){
#     cat("Get Weighted Distances for Candidate Transmitters by Mechanism\n")
#     MinimumDistances_PotentialInfector_byDay_byMechanism_byDay=foreach(i=1:Day) %do% getMinimumDistances_CandidateTransmitters_byDay(i, CandidateTransmitters_byDay_byMechanism[[i]], weights = weights, algorithm = algorithm)
#     save(MinimumDistances_PotentialInfector_byDay_byMechanism_byDay, file="Results/30 Permutations (Corrected) Days/MinimumDistances_PotentialInfector_byDay_byMechanism_byDay.RData")
#     load("Results/30 Permutations (Corrected) Days/MinimumDistances_PotentialInfector_byDay_byMechanism_byDay.RData")
#   }else{
#     cat("Get Un-Weighted Distances for Candidate Transmitters by Mechanism\n")
#     # MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, CandidateTransmitters_byWeek_byMechanism[[i]], weights = weights, algorithm = algorithm)
#     # save(MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted, file="Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted.RData")
#     # load("Results/MinimumDistances_PotentialInfector_byWeek_byMechanism_UnWeighted.RData")
#   }
#   
# }
# 
# 
# #####################################################################
# #### STEP 4a: GET RANDOM CANDIDATE TRANSMITTERS FROM PREMUATIONS ####
# 
# if(Week){
#   #cat("Number of Simulations\n")
#   #Nruns=200
#   
#   #cat("Get All Runs of Weekly Candidate Transmitters from Permutations\n")
#   #AllRuns_CandidateTransmitters_Permutations_byMechanism <- foreach(runs=rep(1,Nruns)) %do% {
#   #  cat("Get Weekly Random Candidate Transmitters from Permutations\n")
#   #  CandidateTransmitters_Permutations_byWeek=foreach(i=1:Week) %do% getCandidateTransmitters_byWeek_byMechanism_Reassignment(i)
#   #} 
#   
#   #Parallel
#   # cat("Numer of cores to use\n")
#   # cores=5
#   # 
#   # cat("Make clusters for parallel\n")
#   # cl=makeCluster(cores)
#   # registerDoSNOW(cl)
#   # getDoParWorkers()
#   # 
#   # cat("RUN PARALLEL\n")
#   # AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment <- foreach(icount(Nruns), .packages=c('igraph', 'foreach')) %dopar% {
#   #   NonPermutation=F
#   #   cat("Get Weekly Random Candidate Transmitters from Permutations\n")
#   #   CandidateTransmitters_Permutations_byWeek=foreach(j=1:Week) %do% getCandidateTransmitters_byWeek_byMechanism_Reassignment(data, j)
#   # }
#   # 
#   # cat("Stop parallel\n")
#   # stopCluster(cl)
#   # print("Cluster stopped")
#   # registerDoSEQ()
#   
#   # save(AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment, file="Results/50 Permutations (Corrected)/AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment.RData")
#   load("Results/50 Permutations (Corrected)/AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment.RData")
# }else{
#   #cat("Number of Simulations\n")
#   #Nruns=200
#   
#   #cat("Get All Runs of Daily Candidate Transmitters from Permutations\n")
#   #AllRuns_CandidateTransmitters_Permutations_byMechanism <- foreach(runs=rep(1,Nruns)) %do% {
#   #  cat("Get Daily Random Candidate Transmitters from Permutations\n")
#   #  CandidateTransmitters_Permutations_byWeek=foreach(i=1:Week) %do% getCandidateTransmitters_byDay_byMechanism_Reassignment(i)
#   #} 
#   
#   #Parallel
#   # cat("Numer of cores to use\n")
#   # cores=5
#   # 
#   # cat("Make clusters for parallel\n")
#   # cl=makeCluster(cores)
#   # registerDoSNOW(cl)
#   # getDoParWorkers()
#   # 
#   # cat("RUN PARALLEL\n")
#   # AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay <- foreach(icount(Nruns), .packages=c('igraph', 'foreach')) %dopar% {
#   #   NonPermutation=F
#   #   cat("Get Daily Random Candidate Transmitters from Permutations\n")
#   #   CandidateTransmitters_Permutations_byDay=foreach(j=1:Day) %do% getCandidateTransmitters_byDay_byMechanism_Reassignment(data, j)
#   # }
#   # 
#   # cat("Stop parallel\n")
#   # stopCluster(cl)
#   # print("Cluster stopped")
#   # registerDoSEQ()
#   # 
#   # save(AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay, file="Results/30 Permutations (Corrected) Days/AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay.RData")
#   load("Results/30 Permutations (Corrected) Days/AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay.RData")
# }
# 
# AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay=AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay[c(1:10)]

#########################################################
#### STEP 4b: GET MINIMUM DISTANCES FROM PREMUATIONS ####

# if(Week){
#   #cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
#   #AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(run=1:Nruns) %do% {
#   #  OneRun_CandidateTransmitters_Permutations_byMechanism=AllRuns_CandidateTransmitters_Permutations_byMechanism[[run]]
#   #  MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, OneRun_CandidateTransmitters_Permutations_byMechanism[[i]], weights = weights, algorithm = algorithm)
#   #} 
#   
#   
#   #Parallel
#   # cat("Numer of cores to use\n")
#   # cores=5
#   # 
#   # cat("Maximum meanGT time to test\n")
#   # Nruns=Nruns
#   # 
#   # cat("Make clusters for parallel\n")
#   # cl=makeCluster(cores)
#   # registerDoSNOW(cl)
#   # getDoParWorkers()
#   # 
#   # AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
#   #   cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
#   #   OneRun_CandidateTransmitters_Permutations_byMechanism_Reassingment=AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment[[run]]
#   #   MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment=foreach(j=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(j, OneRun_CandidateTransmitters_Permutations_byMechanism_Reassingment[[j]], weights = weights, algorithm = algorithm)
#   # }
#   # 
#   # cat("Stop parallel\n")
#   # stopCluster(cl)
#   # print("Cluster stopped")
#   # registerDoSEQ()
#   
#   # save(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment, file="Results/50 Permutations (Corrected)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment.RData")
#   load("Results/50 Permutations (Corrected)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment.RData")
# }else{
#   #cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
#   #AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(run=1:Nruns) %do% {
#   #  OneRun_CandidateTransmitters_Permutations_byMechanism=AllRuns_CandidateTransmitters_Permutations_byMechanism[[run]]
#   #  MinimumDistances_CandidateTransmitters_Permutations_byMechanism=foreach(i=1:Week) %do% getMinimumDistances_CandidateTransmitters_byWeek(i, OneRun_CandidateTransmitters_Permutations_byMechanism[[i]], weights = weights, algorithm = algorithm)
#   #} 
#   
#   
#   #Parallel
#   cat("Numer of cores to use\n")
#   cores=4
# 
#   cat("Maximum meanGT time to test\n")
#   Nruns=Nruns
# 
#   cat("Make clusters for parallel\n")
#   cl=makeCluster(cores)
#   registerDoSNOW(cl)
#   getDoParWorkers()
# 
#   AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
#     cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
#     OneRun_CandidateTransmitters_Permutations_byMechanism_Reassingment=AllRuns_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay[[run]]
#     MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment=foreach(j=1:Day) %do% getMinimumDistances_CandidateTransmitters_byDay(j, OneRun_CandidateTransmitters_Permutations_byMechanism_Reassingment[[j]], weights = weights, algorithm = algorithm)
#   }
# 
#   cat("Stop parallel\n")
#   stopCluster(cl)
#   print("Cluster stopped")
#   registerDoSEQ()
#   
#   save(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay, file="Results/30 Permutations (Corrected) Days/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay_10runs.RData")
#   # load("Results/30 Permutations (Corrected) Days/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism_Reassingment_byDay.RData")
# }


#############################################################
#### STEP 4c: AVERAGE MINIMUM DISTANCES FROM PERMUATIONS ####

cat("Get Average of Minimum Distance (Between Potential Infector and Case)\n")
# Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=getAverageMinDistances_CandidateTransmitters_Permutations(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)
Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=getAverageMinDistances_CandidateTransmitters_Permutations(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled)


#########################################
#### WILCOXON PAIRED RANKED SUM TEST ####

cat("Wilcoxon Rank Sum Test\n")
CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled)) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled, Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)

cat("Create Table of Statistically Significant and Non-Sig Results\n")
WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
Results=as.data.frame(WilcoxonPairedRankTestPValues)
# Results$LowerBoundDay=1:Day
# Results$UpperBound=7+1:Day
Results$Day=1:30
Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05
#If stat. sig. p-value results (TRUE), reject H0 (meaning that the distributions differ)

cat("Save Results\n")
write.table(Results, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Days 1-30)/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")

#######################################
#### INTERPRETATION OF THE RESULTS ####

# OBJECTIVE: Determine if transfer network is linked to CPE episodes
# Step 1: Calculate the minimum network distances of all potential infectors
# Step 2: Does simulation of random networks give us a different result?
# Step 3: Results: some results do show a difference
# Step 4: Conclude: Department transfer network does not better explain CPE episodes



