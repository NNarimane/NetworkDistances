################################################################
##### Describing Pairs of Episodes and Potential Infectors #####
####################### For Permutations #######################
################################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

#############################
#### DATA AND PARAMETERS ####

source("NetworkDistances/DataParameters.R")

#########################
#### LOAD CANDIDATES ####

# load(file=paste0(writingDir,folder,"/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding1.RData"))
# load(file=paste0(writingDir,folder,"/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding2.RData"))
# load(file=paste0(writingDir,folder,"/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding3.RData"))
# load(file=paste0(writingDir,folder,"/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding4.RData"))
# 
# AllRuns1=foreach(i=1:length(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding1[[1]])) %do% lapply(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding1, `[[`, i) #get first elements i of each list
# AllCandidates1=AllRuns1[[1]]
# AllRuns2=foreach(i=1:length(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding2[[1]])) %do% lapply(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding2, `[[`, i) #get first elements i of each list
# AllCandidates2=AllRuns2[[1]]
# AllRuns3=foreach(i=1:length(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding3[[1]])) %do% lapply(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding3, `[[`, i) #get first elements i of each list
# AllCandidates3=AllRuns3[[1]]
# AllRuns4=foreach(i=1:length(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding4[[1]])) %do% lapply(AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding4, `[[`, i) #get first elements i of each list
# AllCandidates4=AllRuns4[[1]]
# 
# AllCandidates5=append(AllCandidates1, AllCandidates2)
# AllCandidates6=append(AllCandidates3, AllCandidates4)
# AllCandidates=append(AllCandidates5, AllCandidates6)
# 
# load(file=paste0(writingDir,folder,"/AllDistances.RData"))
# # AllDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
# 
# #######################
# #### GET ALL PAIRS ####
# 
# run=T
# 
# if(run){
#   cat("Numer of cores to use\n")
#   cores=5
#   
#   cat("Make clusters for parallel\n")
#   cl=makeCluster(cores)
#   registerDoSNOW(cl)
#   getDoParWorkers()
#   
#   cat("RUN PARALLEL\n")
#   AllPairs_Final=foreach(j=1:25, .packages = c("igraph", "foreach")) %do% {
#     AllPairs=foreach(i=1:30, .packages = c("igraph", "foreach")) %dopar% {
#       getPairsByWindow(i, AllDistances[[j]], AllCandidates[[j]])
#   }
#     }
#   
#   cat("Stop parallel\n")
#   stopCluster(cl)
#   print("Cluster stopped")
#   registerDoSEQ()
#   
#   cat("Save or load data\n")
#   save(AllPairs, file=paste0(writingDir,folder,"/AllPairs (Permutations).RData"))
# }else{
#   load(file=paste0(writingDir,folder,"/AllPairs (Permutations).RData"))
# }
# 
# 
# ##############################
# #### DESCRIPTIVE ANALYSIS ####
# 
# cat("Numer of cores to use\n")
# cores=5
# 
# cat("Make clusters for parallel\n")
# cl=makeCluster(cores)
# registerDoSNOW(cl)
# getDoParWorkers()
# 
# cat("RUN PARALLEL\n")
# PropTable_Final=foreach(j=1:2, .packages = c("igraph", "foreach")) %dopar% {
#   
#   cat("Get first element i of each list\n")
#   AllPairs_Episodes=foreach(i=1:length(AllPairs_Final[[j]][[1]])) %do% lapply(AllPairs_Final[[j]], `[[`, i) 
#   Pairs_Episodes=Pairs_Episodes=AllPairs_Episodes[[1]]
#   Pairs_Episodes_Imported=AllPairs_Episodes[[2]]
#   
#   ###############################
#   #### Proportions Explained ####
#   
#   ProportionsTable=foreach(i=1:length(Pairs_Episodes), .combine = "rbind") %do% {
#     cat("Total episodes linked out of total episodes\n")
#     TotalLinked=length(unique(Pairs_Episodes[[i]][complete.cases(Pairs_Episodes[[i]])]))/nrow(data)
#     
#     cat("Total imported episodes linked out of total imported episodes\n")
#     data_Imported=data[which(data$Imported == "O"),]
#     TotalLinkedImported=length(unique(Pairs_Episodes_Imported[[i]][complete.cases(Pairs_Episodes_Imported[[i]])]))/nrow(data_Imported)
#     
#     cat("Total non-imported episodes linked out of total non-imported episodes\n")
#     NonImportedEpisodes=data$Episode[which(data$Imported == "N")]
#     NonImportedEpisodesWithSource=NonImportedEpisodes[NonImportedEpisodes %in% Pairs_Episodes[[i]][,2]]
#     TotalLinkedNonImported=length(NonImportedEpisodesWithSource)/length(NonImportedEpisodes)
#     
#     cat("Cbind\n")
#     Table=c(TotalLinked, TotalLinkedImported, TotalLinkedNonImported)
#   }
#   
#   ProportionsTable=as.data.frame(ProportionsTable)
#   colnames(ProportionsTable)=c("% Episodes Linked Out of Total",
#                                "% Imported Episodes as Source",
#                                "% Non-Imported Episodes as Targets")
#   ProportionsTable
#   
# }
#   
# 
# cat("Stop parallel\n")
# stopCluster(cl)
# print("Cluster stopped")
# registerDoSEQ()
# 
# # cat("Save or load data\n")
# save(AllPairs, file=paste0(writingDir,folder,"/AllPairs.RData"))
# # load(file=paste0(writingDir,folder,"/AllPairs.RData"))


load(file = paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(file = paste0(writingDir,folder,"/PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
PotentialInfectors=PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding

cat("Get lowest observed minimum distance\n")
MinimumDistances_Clean=foreach(i=1:length(AllMinimumDistances)) %do% CleaningFunction2(AllMinimumDistances[[i]])
MinimumDistances_MeansByNWindow=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

cat("Get all pairs of incident episodes and potential infectors by episode number\n")
Episode="Episode"
Pairs_Episodes2=foreach(i=1:length(PotentialInfectors), .combine = 'rbind') %do% {
  if(is.null(PotentialInfectors[[i]])){
    None=c(NA, NA)
  }else{
    Pairs_Episode=c(PotentialInfectors[[i]]$Episode, data[i,Episode])}
}

cat("Remove NAs and bind\n")
Pairs_Episodes=cbind(as.character(Pairs_Episodes[,1]), as.character(Pairs_Episodes[,2]))


################################
#### Select Baseline Window ####
  
Window=21
  
##############################
#### Description of Pairs ####

load(file = paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  
cat("Get distances between pairs\n")
PairsDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Window]]
  
cat("Combine pairs with distances\n")
PairsDistanceTable=data.frame(Pairs_Episodes[[Window]], PairsDistances, stringsAsFactors = F)
  
cat("Remove NAs\n")
PairsDistanceTable$PairsDistances=as.numeric(as.character(PairsDistanceTable$PairsDistances))
PairsDistanceTable=PairsDistanceTable[complete.cases(PairsDistanceTable$PairsDistances),]
  
cat("Add info to table\n")
PairsDistanceTable$SourceImported=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Imported[which(data$Episode == PairsDistanceTable$X1[[i]])]
PairsDistanceTable$SourceDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$X1[[i]])]
PairsDistanceTable$SourceCases=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$TotalCases[which(data$Episode == PairsDistanceTable$X1[[i]])]
PairsDistanceTable$TargetDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$X2[[i]])]
PairsDistanceTable$Mechanism=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Mechanism[which(data$Episode == PairsDistanceTable$X1[[i]])]
colnames(PairsDistanceTable)=c("Source","Target","ShortestPathDistance","SourceImported","SourceDepartment","SourceCases","TargetDepartment","Mechanism")
PairsDistanceTable=PairsDistanceTable[,c("Source","SourceImported","SourceDepartment","ShortestPathDistance","SourceCases","TargetDepartment","Target","Mechanism")]
  
cat("Save or load\n")
write.csv(PairsDistanceTable, file=paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))
  
####################################
#### Extended Analysis of Pairs ####

Extended=T

if(Extended){
  SourceTable=aggregate(SourceImported ~ Source, data=PairsDistanceTable, FUN=length)
  SourceTable=merge(SourceTable, data[,c(1,4)], by.x="Source", by.y="Episode", all.x=T)
  SourceTable=SourceTable[order(SourceTable$Imported),]
  rownames(SourceTable)=1:nrow(SourceTable)
  
  SourceFreqTable=table(SourceTable$SourceImported, SourceTable$Imported)
  cat("Save or load\n")
  write.csv(SourceFreqTable, file=paste0(writingDir,folder,"/SourceFreqTable.csv"))
  
  ######################################
  #### Source Total Number of Cases ####
  
  SourceFreqTable=as.data.frame(table(PairsDistanceTable$Source))
  SourceFreqTable$TotalCases=foreach(i=1:nrow(SourceFreqTable), .combine = 'c') %do% data$TotalCases[which(data$Episode == SourceFreqTable$Var1[[i]])]
  
  ##########################
  #### Summary of Pairs ####
  
  cat("Average Length Between Pairs\n")
  Mean=mean(PairsDistanceTable$ShortestPathDistance)
  cat("Range\n")
  Min=min(PairsDistanceTable$ShortestPathDistance)
  NonZeroMin=min(PairsDistanceTable$ShortestPathDistance[PairsDistanceTable$ShortestPathDistance > 0])
  Max=max(PairsDistanceTable$ShortestPathDistance)
  cat("CI\n")
  Upper_CI=Mean + 1.96*sd(PairsDistanceTable$ShortestPathDistance)/sqrt(length(PairsDistanceTable$ShortestPathDistance))
  Lower_CI=Mean - 1.96*sd(PairsDistanceTable$ShortestPathDistance)/sqrt(length(PairsDistanceTable$ShortestPathDistance))
  
  EpisodesExplained=t(ProportionsTable[Window,])
  SummaryTable=as.data.frame(rbind(nrow(data), nrow(unique(data_Imported)), length(unique(NonImportedEpisodes)), 
                                   length(PairsDistanceTable$ShortestPathDistance), Mean, Lower_CI, Upper_CI, 
                                   Min, NonZeroMin, Max, EpisodesExplained))
  
  
  rownames(SummaryTable)=c("Total Episodes", "Imported Episodes", "Non-Imported Episodes", 
                           "Number of Pairs","Mean Distance","Lower_CI","Upper_CI", "Min",
                           "NonZeroMin","Max","% Episodes Linked Out of Total",
                           "% Imported Episodes as Source","% Non-Imported Episodes as Targets")
  colnames(SummaryTable)=Year
  
  cat("Save or load\n")
  write.csv(SummaryTable, file=paste0(writingDir,folder,"/SummaryTable.csv"))
  
}else{
  cat("Load results\n")
  ProportionsTable=read.csv(file=paste0(writingDir,folder,"/Proportion Table of All Windows.csv"))
  PairsDistanceTable=read.csv(file=paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))
  SummaryTable=read.csv(file=paste0(writingDir,folder,"/SummaryTable.csv"))
}


#############################################
#### Table of 2015-2015 Summary of Pairs ####

runAllSummaries=T
if(runAllSummaries){
  
  Window = 21
  
  Table2012=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2012), " Results","/SummaryTable.csv"))
  Table2013=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2013), " Results","/SummaryTable.csv"))
  Table2014=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2014), " Results","/SummaryTable.csv"))
  Table2015=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2015), " Results","/SummaryTable.csv"))

  load(file=paste0(writingDir,"1000 Permutations/", as.character(2012), " Results","/AllDistances.RData"))
  Permutations2012=AllDistances
  Permutations2012=foreach(i=1:length(Permutations2012[[1]])) %do% lapply(Permutations2012, `[[`, i) #get first elements i of each list
  Permutations2012=foreach(i=1:length(Permutations2012)) %do% CleaningFunction(Permutations2012[[i]])
  Permutations2012=Permutations2012[[Window]]
  Permutations2012=unlist(Permutations2012)
  Permutations2012=Permutations2012[!is.na(Permutations2012)]
  
  MeanPermutation2012=mean(Permutations2012, na.rm = T)
  Upper_CI_P2012=MeanPermutation2012 + 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  Lower_CI_P2012=MeanPermutation2012 - 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  
  
  load(file=paste0(writingDir,"1000 Permutations/", as.character(2013), " Results","/AllDistances.RData"))
  Permutations2013=AllDistances
  Permutations2013=foreach(i=1:length(Permutations2013[[1]])) %do% lapply(Permutations2013, `[[`, i) #get first elements i of each list
  Permutations2013=foreach(i=1:length(Permutations2013)) %do% CleaningFunction(Permutations2013[[i]])
  Permutations2013=Permutations2013[[Window]]
  Permutations2013=unlist(Permutations2013)
  Permutations2013=Permutations2013[!is.na(Permutations2013)]
  
  MeanPermutation2013=mean(Permutations2013, na.rm = T)
  Upper_CI_P2013=MeanPermutation2013 + 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  Lower_CI_P2013=MeanPermutation2013 - 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  
  
  load(file=paste0(writingDir,"1000 Permutations/", as.character(2014), " Results","/AllDistances.RData"))
  Permutations2014=AllDistances
  Permutations2014=foreach(i=1:length(Permutations2014[[1]])) %do% lapply(Permutations2014, `[[`, i) #get first elements i of each list
  Permutations2014=foreach(i=1:length(Permutations2014)) %do% CleaningFunction(Permutations2014[[i]])
  Permutations2014=Permutations2014[[Window]]
  Permutations2014=unlist(Permutations2014)
  Permutations2014=Permutations2014[!is.na(Permutations2014)]
  
  MeanPermutation2014=mean(Permutations2014, na.rm = T)
  Upper_CI_P2014=MeanPermutation2014 + 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  Lower_CI_P2014=MeanPermutation2014 - 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  
  
  load(file=paste0(writingDir,"1000 Permutations/", as.character(2015), " Results","/AllDistances.RData"))
  Permutations2015=AllDistances
  Permutations2015=foreach(i=1:length(Permutations2015[[1]])) %do% lapply(Permutations2015, `[[`, i) #get first elements i of each list
  Permutations2015=foreach(i=1:length(Permutations2015)) %do% CleaningFunction(Permutations2015[[i]])
  Permutations2015=Permutations2015[[Window]]
  Permutations2015=unlist(Permutations2015)
  Permutations2015=Permutations2015[!is.na(Permutations2015)]
  
  MeanPermutation2015=mean(Permutations2015, na.rm = T)
  Upper_CI_P2015=MeanPermutation2015 + 1.96*sd(Permutations2015, na.rm = T)/sqrt(length(Permutations2015))
  Lower_CI_P2015=MeanPermutation2015 - 1.96*sd(Permutations2015, na.rm = T)/sqrt(length(Permutations2015))
  
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
  write.csv(AllSummaries, file=paste0(writingDir,"1000 Permutations/AllSummaries.csv"))
}else{
  AllSummaries=read.csv(file=paste0(writingDir,"1000 Permutations/AllSummaries 16.01.2018.csv"))
}



######################################
#### Number Episodes per Infector ####

cat("Load\n")
Table2012=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2012), " Results","/PairsDistanceInfoTable.csv"))
Table2013=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2013), " Results","/PairsDistanceInfoTable.csv"))
Table2014=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2014), " Results","/PairsDistanceInfoTable.csv"))
Table2015=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2015), " Results","/PairsDistanceInfoTable.csv"))

Freq2012=as.data.frame(table(table(Table2012$Source)), stringsAsFactors = F)
Freq2013=as.data.frame(table(table(Table2013$Source)), stringsAsFactors = F)
Freq2014=as.data.frame(table(table(Table2014$Source)), stringsAsFactors = F)
Freq2015=as.data.frame(table(table(Table2015$Source)), stringsAsFactors = F)

AllFrequencies1=merge(Freq2012, Freq2013, by="Var1", all=T)
colnames(AllFrequencies1)=c("Var1", "Year2012", "Year2013")
AllFrequencies2=merge(AllFrequencies1, Freq2014, by="Var1", all=T)
colnames(AllFrequencies2)=c("Var1", "Year2012", "Year2013", "Year2014")
AllFrequencies3=merge(AllFrequencies2, Freq2015, by="Var1", all=T)
colnames(AllFrequencies3)=c("Freq", "Year2012", "Year2013", "Year2014", "Year2015")
AllFrequencies3[is.na(AllFrequencies3)] = 0
AllFrequencies4=rbind(c(8,0,0,0,0), AllFrequencies3)
AllFrequencies4=AllFrequencies4[order(AllFrequencies4$Freq),]
rownames(AllFrequencies4) = 1:9

# melt the data frame for plotting
AllFrequencies5 <- melt(AllFrequencies4, id.vars='Freq')

# Unstacked
ggplot(AllFrequencies5, aes(Freq, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme(legend.title=element_blank()) +
  guides(fill=guide_legend(title="Year")) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal()

# Stacked
ggplot(AllFrequencies5, aes(Freq, value, fill = variable)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme(legend.title=element_blank()) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal()

####################
### Only 2 >

AllFrequencies6=AllFrequencies5[which(AllFrequencies5$Freq > 2),]

# Unstacked
ggplot(AllFrequencies6, aes(Freq, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") 


# Stacked
ggplot(AllFrequencies6, aes(Freq, value, fill = variable)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  guides(fill=guide_legend(title="Year")) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal()

##############
### void

# Unstacked
ggplot(AllFrequencies5, aes(Freq, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") 


# Stacked
ggplot(AllFrequencies5, aes(Freq, value, fill = variable)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") 
