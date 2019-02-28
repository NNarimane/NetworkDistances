##############################################################
##### Can CPE episodes be explained by transfer network? #####
######################## Permutations ########################
##############################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

#############################
#### DATA AND PARAMETERS ####

source("NetworkDistances/DataParameters.R")

###############################
#### STEP 2: OBSERVED DATA ####

NonPermutation=T
CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding=foreach(i=1:Day) %do% getCandidateTransmitters(data=data, i)
MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding_Potential_Infectors=foreach(i=1:length(CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding)) %do% getMinimumDistances(i, CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[i]], weights = weights, algorithm = algorithm)

Split_Distances_Infectors=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding_Potential_Infectors[[1]])) %do% lapply(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding_Potential_Infectors, `[[`, i) #get first elements i of each list
MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding=Split_Distances_Infectors[[1]]
PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding=Split_Distances_Infectors[[2]]

save(CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding, file = paste0(writingDir,folder,"/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
save(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, file = paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
save(PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding, file = paste0(writingDir,folder,"/PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

load(file = paste0(writingDir,folder,"/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(file = paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(file = paste0(writingDir,folder,"/PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#############################
#### STEP 3: PERMUATIONS ####

#Parallel
cat("Numer of cores to use\n")
cores=6

cat("Runs to test\n")
Nruns=50

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
  ####################
  #### PARAMETERS ####
  
  NonPermutation=F
  Week=F
  Day=30
  Sliding=T
  WeekSlide=1
  
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
  
  cat("Get  Random Candidate Transmitters from Permutations\n")
  CandidateTransmitters_Permutations=foreach(j=1:Day) %do% getCandidateTransmitters(data=data, j)
  
  cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
  MinimumDistances_CandidateTransmitters_Permutations=foreach(k=1:length(CandidateTransmitters_Permutations)) %do% getMinimumDistances(k, CandidateTransmitters_Permutations[[k]], weights = weights, algorithm = algorithm)
  
  Split_Distances_Infectors=foreach(i=1:length(MinimumDistances_CandidateTransmitters_Permutations[[1]])) %do% lapply(MinimumDistances_CandidateTransmitters_Permutations, `[[`, i) #get first elements i of each list
  MinimumDistances=Split_Distances_Infectors[[1]]
  PotentialInfectors=Split_Distances_Infectors[[2]]
  
  AllRuns=list(MinimumDistances, PotentialInfectors)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1)

####################

#Parallel
cat("Numer of cores to use\n")
cores=6

cat("Runs to test\n")
Nruns=50

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
  ####################
  #### PARAMETERS ####
  
  NonPermutation=F
  Week=F
  Day=30
  Sliding=T
  WeekSlide=1
  
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
  
  cat("Get  Random Candidate Transmitters from Permutations\n")
  CandidateTransmitters_Permutations=foreach(j=1:Day) %do% getCandidateTransmitters(data=data, j)
  
  cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
  MinimumDistances_CandidateTransmitters_Permutations=foreach(k=1:length(CandidateTransmitters_Permutations)) %do% getMinimumDistances(k, CandidateTransmitters_Permutations[[k]], weights = weights, algorithm = algorithm)
  
  Split_Distances_Infectors=foreach(i=1:length(MinimumDistances_CandidateTransmitters_Permutations[[1]])) %do% lapply(MinimumDistances_CandidateTransmitters_Permutations, `[[`, i) #get first elements i of each list
  MinimumDistances=Split_Distances_Infectors[[1]]
  PotentialInfectors=Split_Distances_Infectors[[2]]
  
  AllRuns=list(MinimumDistances, PotentialInfectors)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2)

####################

#Parallel
cat("Numer of cores to use\n")
cores=6

cat("Runs to test\n")
Nruns=50

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
  ####################
  #### PARAMETERS ####
  
  NonPermutation=F
  Week=F
  Day=30
  Sliding=T
  WeekSlide=1
  
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
  
  cat("Get  Random Candidate Transmitters from Permutations\n")
  CandidateTransmitters_Permutations=foreach(j=1:Day) %do% getCandidateTransmitters(data=data, j)
  
  cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
  MinimumDistances_CandidateTransmitters_Permutations=foreach(k=1:length(CandidateTransmitters_Permutations)) %do% getMinimumDistances(k, CandidateTransmitters_Permutations[[k]], weights = weights, algorithm = algorithm)
  
  Split_Distances_Infectors=foreach(i=1:length(MinimumDistances_CandidateTransmitters_Permutations[[1]])) %do% lapply(MinimumDistances_CandidateTransmitters_Permutations, `[[`, i) #get first elements i of each list
  MinimumDistances=Split_Distances_Infectors[[1]]
  PotentialInfectors=Split_Distances_Infectors[[2]]
  
  AllRuns=list(MinimumDistances, PotentialInfectors)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3)

####################

#Parallel
cat("Numer of cores to use\n")
cores=6

cat("Runs to test\n")
Nruns=50

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
  ####################
  #### PARAMETERS ####
  
  NonPermutation=F
  Week=F
  Day=30
  Sliding=T
  WeekSlide=1
  
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
  
  cat("Get  Random Candidate Transmitters from Permutations\n")
  CandidateTransmitters_Permutations=foreach(j=1:Day) %do% getCandidateTransmitters(data=data, j)
  
  cat("Get Minimum Distance (Between Potential Infector and Case) for Permutations\n")
  MinimumDistances_CandidateTransmitters_Permutations=foreach(k=1:length(CandidateTransmitters_Permutations)) %do% getMinimumDistances(k, CandidateTransmitters_Permutations[[k]], weights = weights, algorithm = algorithm)
  
  Split_Distances_Infectors=foreach(i=1:length(MinimumDistances_CandidateTransmitters_Permutations[[1]])) %do% lapply(MinimumDistances_CandidateTransmitters_Permutations, `[[`, i) #get first elements i of each list
  MinimumDistances=Split_Distances_Infectors[[1]]
  PotentialInfectors=Split_Distances_Infectors[[2]]
  
  AllRuns=list(MinimumDistances, PotentialInfectors)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4)

#############################################################
#### STEP 5: AVERAGE MINIMUM DISTANCES FROM PERMUATIONS #####

cat("Get Average of Minimum Distance (Between Potential Infector and Case)\n")
Nruns=500
load(file=paste0(writingDir,folder,"/AllDistances.RData"))
Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=getAverageMinDistances_CandidateTransmitters_Permutations(AllDistances)

#################################################
#### STEP 6: WILCOXON PAIRED RANKED SUM TEST ####

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


#####################
#### ALL RESULTS ####

# Table2012=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
# Table2013=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
# Table2014=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
# Table2015=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
# 
# AllPValuesTables=data.frame(Day=1:30, Year2012=Table2012[,1], Year2013=Table2013[,1], Year2014=Table2014[,1], Year2015=Table2015[,1])
# write.csv(AllPValuesTables, file=paste0(writingDir,"Feb 14 Results/AllPValuesTables.csv"))
# 
# 
# ggplot(AllPValuesTables, aes(Day)) + 
#   geom_line(aes(y = Year2012, colour = "2012"), size = 1) + 
#   geom_line(aes(y = Year2013, colour = "2013"), size = 1) +
#   geom_line(aes(y = Year2014, colour = "2014"), size = 1) +
#   geom_line(aes(y = Year2015, colour = "2015"), size = 1) +
#   geom_hline(aes(yintercept=0.05, colour = "P-value = 0.05"), alpha=0.8, size=1) +
#   geom_vline(aes(xintercept=21, colour = "Baseline"), alpha=0.8, size=1) +
#   ylab("Wilcoxon paired rank sum test p-values") +
#   xlab(expression('Window'[n])) +
#   scale_colour_manual(name="", values=c("aquamarine3", "aquamarine4", "steelblue3", "steelblue4", "orange", "red")) +
#   scale_x_continuous(breaks=seq(from=1, to=30, by=1)) +
#   guides(color=guide_legend(order=1)) +
#   theme_bw()

#####################
#### ALL RESULTS ####

Table2012=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/FinalTable.csv"), sep=",")
Table2013=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/FinalTable.csv"), sep=",")
Table2014=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/FinalTable.csv"), sep=",")
Table2015=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/FinalTable.csv"), sep=",")

AllPValuesTables=data.frame(Year2012=Table2012[,c(2,3,5,6,1)], 
                            Year2013=Table2013[,c(5,6,1)], 
                            Year2014=Table2014[,c(5,6,1)], 
                            Year2015=Table2015[,c(5,6,1)])



write.csv(AllPValuesTables, file=paste0(writingDir,"500 Permutations/AllPValuesTables.csv"))
