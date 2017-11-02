###################################################################
##### Assess difference in means for diff number permutations #####
###################################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD PERMUTATIONS ####

load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#########################
#### REORGANIZE DATA ####

cat("Get first elements i of each list\n")
CandidateTransmitters_Permutations_byDay=foreach(i=1:length(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[1]])) %do% lapply(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding, `[[`, i) 

cat("Run Cleaning Function for Random Simulations and Original Results\n")
CandidateTransmitters_Permutations_byDay_Clean=foreach(i=1:length(CandidateTransmitters_Permutations_byDay)) %do% CleaningFunction(CandidateTransmitters_Permutations_byDay[[i]])

cat("Convert sub-list of values into dataframe to facilitate per case averages\n")
CandidateTransmitters_Permutations_byDay_Dataframes=foreach(i=1:length(CandidateTransmitters_Permutations_byDay_Clean)) %do% data.frame(CandidateTransmitters_Permutations_byDay_Clean[[i]], row.names = NULL) 

#####################
#### SELECT DATA ####

Day=21

Day21=CandidateTransmitters_Permutations_byDay_Dataframes[[Day]]
RowMeansDay21=rowMeans(Day21, na.rm=TRUE)
histogram(RowMeansDay21)

RowMeansDay21_10runs=rowMeans(Day21[,1:10], na.rm=TRUE)
RowMeansDay21_20runs=rowMeans(Day21[,1:20], na.rm=TRUE)
RowMeansDay21_25runs=rowMeans(Day21[,1:25], na.rm=TRUE)
RowMeansDay21_30runs=rowMeans(Day21[,1:30], na.rm=TRUE)
RowMeansDay21_40runs=rowMeans(Day21[,1:40], na.rm=TRUE)

AllRowMeans=cbind(RowMeansDay21_10runs, RowMeansDay21_20runs, RowMeansDay21_25runs, RowMeansDay21_30runs,
                  RowMeansDay21_40runs, RowMeansDay21)

##############################
#### TEST FOR DIFFERENCES ####

Comparing10to50=wilcox.test(AllRowMeans[,1], AllRowMeans[,6], paired = T, na.action=na.omit)
Comparing10to50$p.value

Comparing20to50=wilcox.test(AllRowMeans[,2], AllRowMeans[,6], paired = T, na.action=na.omit)
Comparing20to50$p.value

Comparing25to50=wilcox.test(AllRowMeans[,3], AllRowMeans[,6], paired = T, na.action=na.omit)
Comparing25to50$p.value

Comparing30to50=wilcox.test(AllRowMeans[,4], AllRowMeans[,6], paired = T, na.action=na.omit)
Comparing30to50$p.value

Comparing40to50=wilcox.test(AllRowMeans[,5], AllRowMeans[,6], paired = T, na.action=na.omit)
Comparing40to50$p.value
