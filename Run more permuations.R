
rm(CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding)
rm(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding)
rm(PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding)

rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4)

# Run more permuations


#Parallel
cat("Numer of cores to use\n")
cores=6

cat("Runs to test\n")
Nruns=50

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
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

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5)

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

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
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

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6)

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

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
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

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7)

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

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
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

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8)

#Parallel
cat("Numer of cores to use\n")
cores=6

cat("Runs to test\n")
Nruns=50

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
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

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9)

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

AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10=foreach(run=1:Nruns, .packages=c('igraph', 'foreach')) %dopar% {
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

save(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10, file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10.RData"))
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10)


###################################
#### Split Permutation Results ####

load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9.RData"))
load(file=paste0(writingDir,folder,"/AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10.RData"))

AllRuns1=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1, `[[`, i) #get first elements i of each list
AllRuns2=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2, `[[`, i) #get first elements i of each list
AllRuns3=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3, `[[`, i) #get first elements i of each list
AllRuns4=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4, `[[`, i) #get first elements i of each list
AllRuns5=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5, `[[`, i) #get first elements i of each list
AllRuns6=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6, `[[`, i) #get first elements i of each list
AllRuns7=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7, `[[`, i) #get first elements i of each list
AllRuns8=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8, `[[`, i) #get first elements i of each list
AllRuns9=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9, `[[`, i) #get first elements i of each list
AllRuns10=foreach(i=1:length(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10[[1]])) %do% lapply(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10, `[[`, i) #get first elements i of each list

#Distances
AllDistances1=AllRuns1[[1]]
AllDistances2=AllRuns2[[1]]
AllDistances3=AllRuns3[[1]]
AllDistances4=AllRuns4[[1]]
AllDistances5=AllRuns5[[1]]
AllDistances6=AllRuns6[[1]]
AllDistances7=AllRuns7[[1]]
AllDistances8=AllRuns8[[1]]
AllDistances9=AllRuns9[[1]]
AllDistances10=AllRuns10[[1]]

rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors1)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors2)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors3)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors4)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors5)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors6)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors7)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors8)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors9)
rm(AllRuns_CandidateTransmitters_MinimumDistances_PotentialInfectors10)
rm(AllRuns1)
rm(AllRuns2)
rm(AllRuns3)
rm(AllRuns4)
rm(AllRuns5)
rm(AllRuns6)
rm(AllRuns7)
rm(AllRuns8)
rm(AllRuns9)
rm(AllRuns10)

AllDistancesA=append(AllDistances1, AllDistances2)
AllDistancesB=append(AllDistances3, AllDistances4)
AllDistancesC=append(AllDistances5, AllDistances6)
AllDistancesD=append(AllDistances7, AllDistances8)
AllDistancesE=append(AllDistances9, AllDistances10)

AllDistancesAA=append(AllDistancesA, AllDistancesB)
AllDistancesBB=append(AllDistancesC, AllDistancesD)
AllDistancesCC=append(AllDistancesBB, AllDistancesE)
AllDistances=append(AllDistancesAA, AllDistancesCC)
save(AllDistances, file=paste0(writingDir,folder,"/AllDistances.RData"))


###################################################################

load(file=paste0(writingDir,folder,"/AllDistances200.RData"))
load(file=paste0(writingDir,folder,"/AllDistances400.RData"))
load(file=paste0(writingDir,folder,"/AllDistances600.RData"))
load(file=paste0(writingDir,folder,"/AllDistances800.RData"))
load(file=paste0(writingDir,folder,"/AllDistances1000.RData"))

AllDistancesAA=append(AllDistances200, AllDistances400)
AllDistancesBB=append(AllDistances600, AllDistances800)
AllDistancesCC=append(AllDistancesBB, AllDistances1000)
AllDistances=append(AllDistancesAA, AllDistancesCC)

Nruns=seq(from=100, to=1000, by=100)
AllResults=foreach(i=Nruns, .combine="cbind") %do% {
  cat("Get Average of Minimum Distance (Between Potential Infector and Case)\n")
  Nruns=i
  Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism=getAverageMinDistances_CandidateTransmitters_Permutations(AllDistances[1:Nruns])

  cat("Wilcoxon Rank Sum Test\n")
  CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding)) %do% getWilcoxonPairedRankTestPValues(i, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, Average_MinimumDistances_CandidateTransmitters_Permutations_byMechanism)

  cat("Create Table of Statistically Significant and Non-Sig Results\n")
  WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
  Results=as.data.frame(WilcoxonPairedRankTestPValues)
}


cat("Save Results\n")
write.table(AllResults, file=paste0(writingDir,folder,"/AllResults500.csv"), sep=",")
