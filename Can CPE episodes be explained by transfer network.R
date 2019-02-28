##############################################################
##### Can CPE episodes be explained by transfer network? #####
##############################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

#############################
#### DATA AND PARAMETERS ####

source("NetworkDistances/DataParameters.R")

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


#####################
#### ALL RESULTS ####

Table2012=read.csv(file=paste0(writingDir,"Feb 9.2 Results/", as.character(2012), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
Table2013=read.csv(file=paste0(writingDir,"Feb 9.2 Results/", as.character(2013), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
Table2014=read.csv(file=paste0(writingDir,"Feb 9.2 Results/", as.character(2014), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
Table2015=read.csv(file=paste0(writingDir,"Feb 9.2 Results/", as.character(2015), " Results","/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")

AllPValuesTables=data.frame(Day=1:30, Year2012=Table2012[,1], Year2013=Table2013[,1], Year2014=Table2014[,1], Year2015=Table2015[,1])
write.csv(AllPValuesTables, file=paste0(writingDir,"Feb 9.2 Results/AllPValuesTables.csv"))


ggplot(AllPValuesTables, aes(Day)) + 
  geom_line(aes(y = Year2012, colour = "2012"), size = 1) + 
  geom_line(aes(y = Year2013, colour = "2013"), size = 1) +
  geom_line(aes(y = Year2014, colour = "2014"), size = 1) +
  geom_line(aes(y = Year2015, colour = "2015"), size = 1) +
  geom_hline(aes(yintercept=0.05, colour = "P-value = 0.05"), alpha=0.8, size=1) +
  geom_vline(aes(xintercept=21, colour = "Baseline"), alpha=0.8, size=1) +
  ylab("Wilcoxon paired rank sum test p-values") +
  xlab("1st Day of 1-Week Sliding Window") +
  scale_colour_manual(name="", values=c("aquamarine3", "aquamarine4", "steelblue3", "steelblue4", "black", "red")) +
  scale_x_continuous(breaks=seq(from=1, to=30, by=1)) +
  guides(color=guide_legend(order=1)) +
  theme_bw()


