################################################################
##### Describing Pairs of Episodes and Potential Infectors #####
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

cat(paste("Load", Year, "Candidates\n"))
load(paste0(writingDir,folder,"/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat(paste("Load", Year, "Minimum Distances\n"))
load(paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat(paste("Load", Year, "Potential Infector\n"))
load(file = paste0(writingDir,folder,"/PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
PotentialInfectors=PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding

#######################
#### GET ALL PAIRS ####

run=T

if(run){
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
}else{
  load(file=paste0(writingDir,folder,"/AllPairs.RData"))
}

##############################
#### DESCRIPTIVE ANALYSIS ####

runDescriptiveAnalysis=T

if(runDescriptiveAnalysis){
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
    TotalLinked=length(unique(Pairs_Episodes[[i]][complete.cases(Pairs_Episodes[[i]])]))/nrow(data)
    
    cat("Total imported episodes linked out of total imported episodes\n")
    data_Imported=data[which(data$Imported == "O"),]
    TotalLinkedImported=length(unique(Pairs_Episodes_Imported[[i]][complete.cases(Pairs_Episodes_Imported[[i]])]))/nrow(data_Imported)
    
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
  # NonNAPairsDistances=PairsDistances[complete.cases(PairsDistances)]
  
  # cat("Get Table of Pairs and Distances\n")
  # PairsDistanceTable=as.data.frame(cbind(Pairs_Episodes[[Window]], NonNAPairsDistances)) #issue
  # cat("Number of different sources\n")
  # length(levels(PairsDistanceTable$V1))
  # cat("Number of different targets\n")
  # length(levels(PairsDistanceTable$V2))
  
  cat("Combine pairs with distances\n")
  PairsDistanceTable=data.frame(Pairs_Episodes[[Window]], PairsDistances, stringsAsFactors = F)
  
  cat("Remove NAs\n")
  # PairsDistanceTable$V1=as.character(PairsDistanceTable$X1)
  # PairsDistanceTable$V2=as.character(PairsDistanceTable$X2)
  # PairsDistanceTable$NonNAPairsDistances=as.numeric(as.character(PairsDistanceTable$NonNAPairsDistances))
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
  
  Table2012=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/SummaryTable.csv"))
  Table2013=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/SummaryTable.csv"))
  Table2014=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/SummaryTable.csv"))
  Table2015=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/SummaryTable.csv"))
  # Table2016=read.csv(file=paste0(writingDir,"2016 Final Observed and Permutation Results/SummaryTable.csv"))
  
  load(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  Permutations2012=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  Permutations2012=foreach(i=1:length(Permutations2012[[1]])) %do% lapply(Permutations2012, `[[`, i) #get first elements i of each list
  Permutations2012=foreach(i=1:length(Permutations2012)) %do% CleaningFunction(Permutations2012[[i]])
  Permutations2012=Permutations2012[[Window]]
  Permutations2012=unlist(Permutations2012)
  Permutations2012=Permutations2012[!is.na(Permutations2012)]
  
  MeanPermutation2012=mean(Permutations2012, na.rm = T)
  Upper_CI_P2012=MeanPermutation2012 + 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  Lower_CI_P2012=MeanPermutation2012 - 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  
  
  load(file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  Permutations2013=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  Permutations2013=foreach(i=1:length(Permutations2013[[1]])) %do% lapply(Permutations2013, `[[`, i) #get first elements i of each list
  Permutations2013=foreach(i=1:length(Permutations2013)) %do% CleaningFunction(Permutations2013[[i]])
  Permutations2013=Permutations2013[[Window]]
  Permutations2013=unlist(Permutations2013)
  Permutations2013=Permutations2013[!is.na(Permutations2013)]
  
  MeanPermutation2013=mean(Permutations2013, na.rm = T)
  Upper_CI_P2013=MeanPermutation2013 + 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  Lower_CI_P2013=MeanPermutation2013 - 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  
  
  load(file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  Permutations2014=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  Permutations2014=foreach(i=1:length(Permutations2014[[1]])) %do% lapply(Permutations2014, `[[`, i) #get first elements i of each list
  Permutations2014=foreach(i=1:length(Permutations2014)) %do% CleaningFunction(Permutations2014[[i]])
  Permutations2014=Permutations2014[[Window]]
  Permutations2014=unlist(Permutations2014)
  Permutations2014=Permutations2014[!is.na(Permutations2014)]
  
  MeanPermutation2014=mean(Permutations2014, na.rm = T)
  Upper_CI_P2014=MeanPermutation2014 + 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  Lower_CI_P2014=MeanPermutation2014 - 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  
  
  load(file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
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
  write.csv(AllSummaries, file=paste0(writingDir,"Feb 14 Results/AllSummaries.csv"))
}else{
  AllSummaries=read.csv(file=paste0(writingDir,"Feb 14 Results/AllSummaries.csv"))
}



######################################
#### Number Episodes per Infector ####

cat("Load\n")
Table2012=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/PairsDistanceInfoTable.csv"))
Table2013=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/PairsDistanceInfoTable.csv"))
Table2014=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/PairsDistanceInfoTable.csv"))
Table2015=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/PairsDistanceInfoTable.csv"))

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
  guides(fill=guide_legend(title="Year")) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal()

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


#####################
##### Trend Test ####

# 2 or more
SS2=c(sum(Freq2012[2:nrow(Freq2012),2]), 
      sum(Freq2013[2:nrow(Freq2013),2]),
      sum(Freq2014[2:nrow(Freq2014),2]),
      sum(Freq2015[2:nrow(Freq2015),2]))
EP=c(sum(Freq2012[,2]), 
      sum(Freq2013[,2]),
      sum(Freq2014[,2]),
      sum(Freq2015[,2]))

#Test
prop.trend.test(SS2, EP)
# H0: There is no linear trend in the proportion of cases across age groups
# Ha: There is a linear trend in the proportion of cases across age groups

# 3 or more
SS3=c(sum(Freq2012[3:nrow(Freq2012),2]), 
      sum(Freq2013[3:nrow(Freq2013),2]),
      sum(Freq2014[3:nrow(Freq2014),2]),
      sum(Freq2015[3:nrow(Freq2015),2]))

#Test
prop.trend.test(SS3, EP)
# H0: There is no linear trend in the proportion of cases across age groups
# Ha: There is a linear trend in the proportion of cases across age groups

# 4 or more
SS4=c(sum(Freq2012[4:nrow(Freq2012),2]), 
      sum(Freq2013[4:nrow(Freq2013),2]),
      sum(Freq2014[4:nrow(Freq2014),2]),
      sum(Freq2015[4:nrow(Freq2015),2]))

#Test
prop.trend.test(SS4, EP)
# H0: There is no linear trend in the proportion of cases across age groups
# Ha: There is a linear trend in the proportion of cases across age groups



##############################################
##### Number of Cases and Secondary Cases ####

cat("Load\n")
Table2012=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/PairsDistanceInfoTable.csv"))
Table2013=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/PairsDistanceInfoTable.csv"))
Table2014=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/PairsDistanceInfoTable.csv"))
Table2015=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/PairsDistanceInfoTable.csv"))

Freq2012=as.data.frame((table(Table2012$Source)), stringsAsFactors = F)
Freq2013=as.data.frame((table(Table2013$Source)), stringsAsFactors = F)
Freq2014=as.data.frame((table(Table2014$Source)), stringsAsFactors = F)
Freq2015=as.data.frame((table(Table2015$Source)), stringsAsFactors = F)

TableFreq2012=merge(Table2012, Freq2012, by.x="Source", by.y="Var1", all.x=T)
TableFreq2013=merge(Table2013, Freq2013, by.x="Source", by.y="Var1", all.x=T)
TableFreq2014=merge(Table2014, Freq2014, by.x="Source", by.y="Var1", all.x=T)
TableFreq2015=merge(Table2015, Freq2015, by.x="Source", by.y="Var1", all.x=T)

# 2012
TableFreq2012$CaseRank=rank(TableFreq2012$SourceCases, ties.method = "average")
TableFreq2012$FreqRank=rank(TableFreq2012$Freq, ties.method = "average")
cor.test(TableFreq2012$CaseRank, TableFreq2012$FreqRank, method = "spearman")
# 2013
TableFreq2013$CaseRank=rank(TableFreq2013$SourceCases, ties.method = "average")
TableFreq2013$FreqRank=rank(TableFreq2013$Freq, ties.method = "average")
cor.test(TableFreq2013$CaseRank, TableFreq2013$FreqRank, method = "spearman")
# 2014
TableFreq2014$CaseRank=rank(TableFreq2014$SourceCases, ties.method = "average")
TableFreq2014$FreqRank=rank(TableFreq2014$Freq, ties.method = "average")
cor.test(TableFreq2014$CaseRank, TableFreq2014$FreqRank, method = "spearman")
# 2015
TableFreq2015$CaseRank=rank(TableFreq2015$SourceCases, ties.method = "average")
TableFreq2015$FreqRank=rank(TableFreq2015$Freq, ties.method = "average")
cor.test(TableFreq2015$CaseRank, TableFreq2015$FreqRank, method = "spearman")
