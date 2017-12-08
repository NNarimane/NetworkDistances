################################################################
##### Describing Pairs of Episodes and Potential Infectors #####
################################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### WORKING DIRECTORY ####

folder="2016 Final Observed and Permutation Results"

cat("Choose year to analyze\n")
Year="2012"

#########################
#### LOAD CANDIDATES ####

cat(paste("Load", Year, "Candidates\n"))
load(paste0(writingDir,folder,"/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat(paste("Load", Year, "Minimum Distances\n"))
load(paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#######################
#### GET ALL PAIRS ####

cat("Numer of cores to use\n")
cores=5

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

cat("RUN PARALLEL\n")
AllPairs=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding), .packages = c("igraph", "foreach")) %dopar% {
  getPairsByWindow(i, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding)
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

cat("Save or load data\n")
save(AllPairs, file=paste0(writingDir,folder,"/AllPairs.RData"))

########################
#### Seperate Pairs ####

cat("Get first element i of each list\n")
AllPairs_Episodes=foreach(i=1:length(AllPairs[[1]])) %do% lapply(AllPairs, `[[`, i) 
Pairs_Episodes=AllPairs_Episodes[[1]]
Pairs_Episodes_Imported=AllPairs_Episodes[[2]]

###############################
#### Proportions Explained ####

ProportionsTable=foreach(i=1:length(Pairs_Episodes), .combine = "rbind") %do% {
  cat("Total episodes linked out of total episodes\n")
  TotalLinked=nrow(Pairs_Episodes[[i]])/nrow(data)
  
  cat("Total imported episodes linked out of total imported episodes\n")
  data_Imported=data[which(data$Imported == "O"),]
  TotalLinkedImported=nrow(Pairs_Episodes_Imported[[i]])/nrow(data_Imported)
  
  cat("Total non-imported episodes linked out of total non-imported episodes\n")
  NonImportedEpisodes=data$Episode[which(data$Imported == "N")]
  NonImportedEpisodesWithSource=NonImportedEpisodes[NonImportedEpisodes %in% Pairs_Episodes[[i]][,2]]
  TotalLinkedNonImported=length(NonImportedEpisodesWithSource)/length(NonImportedEpisodes)
  
  cat("Cbind\n")
  Table=c(TotalLinked, TotalLinkedImported, TotalLinkedNonImported)
}

ProportionsTable=as.data.frame(ProportionsTable)
colnames(ProportionsTable)=c("% Episodes Linked Out of Total",
                             "% Imported Episodes as Source",
                             "% Non-Imported Episodes as Targets")
write.csv(ProportionsTable, file=paste0(writingDir,folder,"/Proportion Table of All Windows.csv"))

################################
#### Select Baseline Window ####

Window=21

##############################
#### Description of Pairs ####

cat("Get distances between pairs\n")
PairsDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Window]]
NonNAPairsDistances=PairsDistances[complete.cases(PairsDistances)]

cat("Get Table of Pairs and Distances\n")
PairsDistanceTable=as.data.frame(cbind(Pairs_Episodes[[Window]], NonNAPairsDistances))
cat("Number of different sources\n")
length(levels(PairsDistanceTable$V1))
cat("Number of different targets\n")
length(levels(PairsDistanceTable$V2))

cat("Add info to table\n")
PairsDistanceTable$V1=as.character(PairsDistanceTable$V1)
PairsDistanceTable$V2=as.character(PairsDistanceTable$V2)
PairsDistanceTable$NonNAPairsDistances=as.numeric(as.character(PairsDistanceTable$NonNAPairsDistances))
PairsDistanceTable$SourceImported=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Imported[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$SourceDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$SourceCases=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$TotalCases[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$TargetDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V2[[i]])]
PairsDistanceTable$Mechanism=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Mechanism[which(data$Episode == PairsDistanceTable$V1[[i]])]
colnames(PairsDistanceTable)=c("Source","Target","ShortestPathDistance","SourceImported","SourceDepartment","SourceCases","TargetDepartment","Mechanism")
PairsDistanceTable=PairsDistanceTable[,c("Source","SourceImported","SourceDepartment","ShortestPathDistance","SourceCases","TargetDepartment","Target","Mechanism")]

cat("Save or load\n")
write.csv(PairsDistanceTable, file=paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))
# load(paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))


######################################
#### Source Total Number of Cases ####

SourceFreqTable=as.data.frame(table(PairsDistanceTable$Source))
SourceFreqTable$TotalCases=foreach(i=1:nrow(SourceFreqTable), .combine = 'c') %do% data$TotalCases[which(data$Episode == SourceFreqTable$Var1[[i]])]


##########################
#### Summary of Pairs ####

cat("Average Length Between Pairs\n")
Mean=mean(NonNAPairsDistances)
cat("Range\n")
Min=min(NonNAPairsDistances)
NonZeroMin=min(NonNAPairsDistances[NonNAPairsDistances > 0])
Max=max(NonNAPairsDistances)
cat("CI\n")
Upper_CI=Mean + 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))
Lower_CI=Mean - 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))

EpisodesExplained=t(ProportionsTable[Window,])
SummaryTable=as.data.frame(rbind(nrow(data), nrow(data_Imported), length(NonImportedEpisodes), 
                                 length(NonNAPairsDistances), Mean, Min, NonZeroMin, Max, Upper_CI, 
                                 Lower_CI, EpisodesExplained))
                          

rownames(SummaryTable)=c("Total Episodes", "Imported Episodes", "Non-Imported Episodes", 
                         "Number of Pairs","Mean Distance","Min","NonZeroMin","Max",
                         "Upper_CI","Lower_CI","% Episodes Linked Out of Total",
                         "% Imported Episodes as Source","% Non-Imported Episodes as Targets")
colnames(SummaryTable)=Year

cat("Save or load\n")
write.csv(SummaryTable, file=paste0(writingDir,folder,"/SummaryTable.csv"))
# load(paste0(writingDir,folder,"/SummaryTable.csv"))


#############################################
#### Table of 2015-2015 Summary of Pairs ####

Table2012=read.csv(file=paste0(writingDir,"2012 Final Observed and Permutation Results PROBTRANS NET/SummaryTable.csv"))
Table2013=read.csv(file=paste0(writingDir,"2013 Final Observed and Permutation Results PROBTRANS NET/SummaryTable.csv"))
Table2014=read.csv(file=paste0(writingDir,"2014 Final Observed and Permutation Results PROBTRANS NET/SummaryTable.csv"))
Table2015=read.csv(file=paste0(writingDir,"2015 Final Observed and Permutation Results PROBTRANS NET/SummaryTable.csv"))
# Table2016=read.csv(file=paste0(writingDir,"2016 Final Observed and Permutation Results/SummaryTable.csv"))

load(file=paste0(writingDir,"2012 Final Observed and Permutation Results PROBTRANS NET/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
Permutations2012=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
Permutations2012=foreach(i=1:length(Permutations2012[[1]])) %do% lapply(Permutations2012, `[[`, i) #get first elements i of each list
Permutations2012=foreach(i=1:length(Permutations2012)) %do% CleaningFunction(Permutations2012[[i]])
Permutations2012=Permutations2012[[Window]]
Permutations2012=unlist(Permutations2012)
Permutations2012=Permutations2012[!is.na(Permutations2012)]

MeanPermutation2012=mean(Permutations2012, na.rm = T)
Upper_CI_P2012=MeanPermutation2012 + 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
Lower_CI_P2012=MeanPermutation2012 - 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))


load(file=paste0(writingDir,"2013 Final Observed and Permutation Results PROBTRANS NET/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
Permutations2013=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
Permutations2013=foreach(i=1:length(Permutations2013[[1]])) %do% lapply(Permutations2013, `[[`, i) #get first elements i of each list
Permutations2013=foreach(i=1:length(Permutations2013)) %do% CleaningFunction(Permutations2013[[i]])
Permutations2013=Permutations2013[[Window]]
Permutations2013=unlist(Permutations2013)
Permutations2013=Permutations2013[!is.na(Permutations2013)]

MeanPermutation2013=mean(Permutations2013, na.rm = T)
Upper_CI_P2013=MeanPermutation2013 + 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
Lower_CI_P2013=MeanPermutation2013 - 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))


load(file=paste0(writingDir,"2014 Final Observed and Permutation Results PROBTRANS NET/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
Permutations2014=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
Permutations2014=foreach(i=1:length(Permutations2014[[1]])) %do% lapply(Permutations2014, `[[`, i) #get first elements i of each list
Permutations2014=foreach(i=1:length(Permutations2014)) %do% CleaningFunction(Permutations2014[[i]])
Permutations2014=Permutations2014[[Window]]
Permutations2014=unlist(Permutations2014)
Permutations2014=Permutations2014[!is.na(Permutations2014)]

MeanPermutation2014=mean(Permutations2014, na.rm = T)
Upper_CI_P2014=MeanPermutation2014 + 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
Lower_CI_P2014=MeanPermutation2014 - 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))


load(file=paste0(writingDir,"2015 Final Observed and Permutation Results PROBTRANS NET/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
Permutations2015=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
Permutations2015=foreach(i=1:length(Permutations2015[[1]])) %do% lapply(Permutations2015, `[[`, i) #get first elements i of each list
Permutations2015=foreach(i=1:length(Permutations2015)) %do% CleaningFunction(Permutations2015[[i]])
Permutations2015=Permutations2015[[Window]]
Permutations2015=unlist(Permutations2015)
Permutations2015=Permutations2015[!is.na(Permutations2015)]

MeanPermutation2015=mean(Permutations2015, na.rm = T)
Upper_CI_P2015=MeanPermutation2015 + 1.96*sd(Permutations2015, na.rm = T)/sqrt(length(Permutations2015))
Lower_CI_P2015=MeanPermutation2015 - 1.96*sd(Permutations2015, na.rm = T)/sqrt(length(Permutations2015))

# #2016
# load(file=paste0(writingDir,"2016 Final Observed and Permutation Results/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
# Permutations2016=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
# Permutations2016=foreach(i=1:length(Permutations2016[[1]])) %do% lapply(Permutations2016, `[[`, i) #get first elements i of each list
# Permutations2016=foreach(i=1:length(Permutations2016)) %do% CleaningFunction(Permutations2016[[i]])
# Permutations2016=Permutations2016[[Window]]
# Permutations2016=unlist(Permutations2016)
# Permutations2016=Permutations2016[!is.na(Permutations2016)]
# 
# MeanPermutation2016=mean(Permutations2016, na.rm = T)
# Upper_CI_P2016=MeanPermutation2016 + 1.96*sd(Permutations2016, na.rm = T)/sqrt(length(Permutations2016))
# Lower_CI_P2016=MeanPermutation2016 - 1.96*sd(Permutations2016, na.rm = T)/sqrt(length(Permutations2016))


AllPermutationMeans=cbind(MeanPermutation2012, MeanPermutation2013, MeanPermutation2014, MeanPermutation2015)
AllPermutationMeans=cbind("Permutation Means", AllPermutationMeans)
colnames(AllPermutationMeans)=c("Year","2012","2013","2014","2015")
AllUpper_CI=cbind(Upper_CI_P2012, Upper_CI_P2013, Upper_CI_P2014, Upper_CI_P2015)
AllUpper_CI=cbind("Permutations Upper_CI", AllUpper_CI)
colnames(AllUpper_CI)=c("Year","2012","2013","2014","2015")
AllLower_CI=cbind(Lower_CI_P2012, Lower_CI_P2013, Lower_CI_P2014, Lower_CI_P2015)
AllLower_CI=cbind("Permutations Lower_CI", AllLower_CI)
colnames(AllLower_CI)=c("Year","2012","2013","2014","2015")

AllPermutationsInfo=rbind(AllPermutationMeans, AllUpper_CI, AllLower_CI)

AllSummaries=cbind(Table2012, Table2013[,2], Table2014[,2], Table2015[,2])
colnames(AllSummaries)=c("Year","2012","2013","2014","2015")

AllSummaries=rbind(AllSummaries[c(1:5),], AllPermutationsInfo, AllSummaries[c(6:13),])

write.csv(AllSummaries, file=paste0(writingDir,"AllSummaries PROBTRANS 07.12.2017.csv"))



