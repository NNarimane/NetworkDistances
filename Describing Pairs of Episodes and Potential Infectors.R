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

cat("Choose year to analyze\n")
Year=2013

folder=paste0("Dec 8 Results/", as.character(Year), " Results")

#########################
#### LOAD CANDIDATES ####

cat(paste("Load", Year, "Candidates\n"))
load(paste0(writingDir,folder,"/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat(paste("Load", Year, "Minimum Distances\n"))
load(paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#######################
#### GET ALL PAIRS ####

run=FALSE

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

runDescriptiveAnalysis=FALSE
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
  
}else{
  cat("Load results\n")
  ProportionsTable=read.csv(file=paste0(writingDir,folder,"/Proportion Table of All Windows.csv"))
  PairsDistanceTable=read.csv(file=paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))
  SummaryTable=read.csv(file=paste0(writingDir,folder,"/SummaryTable.csv"))
}


#############################################
#### Table of 2015-2015 Summary of Pairs ####

runAllSummaries=FALSE
if(runAllSummaries){
  
  Window = 21
  
  Table2012=read.csv(file=paste0(writingDir,"Dec 8 Results/", as.character(2012), " Results","/SummaryTable.csv"))
  Table2013=read.csv(file=paste0(writingDir,"Dec 8 Results/", as.character(2013), " Results","/SummaryTable.csv"))
  Table2014=read.csv(file=paste0(writingDir,"Dec 8 Results/", as.character(2014), " Results","/SummaryTable.csv"))
  Table2015=read.csv(file=paste0(writingDir,"Dec 8 Results/", as.character(2015), " Results","/SummaryTable.csv"))
  # Table2016=read.csv(file=paste0(writingDir,"2016 Final Observed and Permutation Results/SummaryTable.csv"))
  
  load(file=paste0(writingDir,"Dec 8 Results/", as.character(2012), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  Permutations2012=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  Permutations2012=foreach(i=1:length(Permutations2012[[1]])) %do% lapply(Permutations2012, `[[`, i) #get first elements i of each list
  Permutations2012=foreach(i=1:length(Permutations2012)) %do% CleaningFunction(Permutations2012[[i]])
  Permutations2012=Permutations2012[[Window]]
  Permutations2012=unlist(Permutations2012)
  Permutations2012=Permutations2012[!is.na(Permutations2012)]
  
  MeanPermutation2012=mean(Permutations2012, na.rm = T)
  Upper_CI_P2012=MeanPermutation2012 + 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  Lower_CI_P2012=MeanPermutation2012 - 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  
  
  load(file=paste0(writingDir,"Dec 8 Results/", as.character(2013), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  Permutations2013=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  Permutations2013=foreach(i=1:length(Permutations2013[[1]])) %do% lapply(Permutations2013, `[[`, i) #get first elements i of each list
  Permutations2013=foreach(i=1:length(Permutations2013)) %do% CleaningFunction(Permutations2013[[i]])
  Permutations2013=Permutations2013[[Window]]
  Permutations2013=unlist(Permutations2013)
  Permutations2013=Permutations2013[!is.na(Permutations2013)]
  
  MeanPermutation2013=mean(Permutations2013, na.rm = T)
  Upper_CI_P2013=MeanPermutation2013 + 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  Lower_CI_P2013=MeanPermutation2013 - 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  
  
  load(file=paste0(writingDir,"Dec 8 Results/", as.character(2014), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  Permutations2014=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  Permutations2014=foreach(i=1:length(Permutations2014[[1]])) %do% lapply(Permutations2014, `[[`, i) #get first elements i of each list
  Permutations2014=foreach(i=1:length(Permutations2014)) %do% CleaningFunction(Permutations2014[[i]])
  Permutations2014=Permutations2014[[Window]]
  Permutations2014=unlist(Permutations2014)
  Permutations2014=Permutations2014[!is.na(Permutations2014)]
  
  MeanPermutation2014=mean(Permutations2014, na.rm = T)
  Upper_CI_P2014=MeanPermutation2014 + 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  Lower_CI_P2014=MeanPermutation2014 - 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  
  
  load(file=paste0(writingDir,"Dec 8 Results/", as.character(2015), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
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
  write.csv(AllSummaries, file=paste0(writingDir,"AllSummaries 09.12.2017.csv"))
}else{
  AllSummaries=read.csv(file=paste0(writingDir,"AllSummaries 09.12.2017.csv"))
}


##########################################
#### Analysis of Departments of Pairs ####

# Spatial Summary function
getSpatialSummary=function(Year){
  Year=Year
  folder=paste0("Dec 8 Results/", as.character(Year), " Results")
  PairsDistanceTable=read.csv(file=paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))
  
  # Number of different departments link
  UniqueSourceDepts=length(unique(PairsDistanceTable$SourceDepartment))
  UniqueTargetDepts=length(unique(PairsDistanceTable$TargetDepartment))
  UniqueSourceDepts_Imported=length(unique(PairsDistanceTable[which(PairsDistanceTable$SourceImported == "O"),]$SourceDepartment))
  UniqueTargetDepts_Imported=length(unique(PairsDistanceTable[which(PairsDistanceTable$SourceImported == "O"),]$TargetDepartment))
  
  # Old Regions
  library(readxl)
  Dept_Region_GeoCodes = read_excel("C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/Dept Region GeoCodes.xlsx")
  colnames(Dept_Region_GeoCodes)[3]="OldRegions"
  # New regions
  NewRegions=data.frame(OldRegions=c("Bourgogne", "Franche-Comté",
                                     "Aquitaine","Limousin","Poitou-Charente",
                                     "Alsace","Champagne","Lorraine",
                                     "Languedoc","Midi-Pyrénées",
                                     "Haute-Normandie","Normandie","Basse-Normandie",
                                     "Nord","Pas-de-Calais","Picardie",
                                     "Auvergne","Rhône-Alpes",
                                     "Provence-Alpes-Côte d'Azur",
                                     "Centre",
                                     "Corse",
                                     "Bretagne",
                                     "Ile-de-France",
                                     "Pays-de-la-Loire"), 
                        NewRegions=c("Bourgogne-Franche-Comté","Bourgogne-Franche-Comté",
                                     "Nouvelle-Aquitaine","Nouvelle-Aquitaine","Nouvelle-Aquitaine",
                                     "Grand Est","Grand Est","Grand Est",
                                     "Occitanie","Occitanie",
                                     "Normandie","Normandie","Normandie",
                                     "Hauts-de-France","Hauts-de-France","Hauts-de-France",
                                     "Auvergne-Rhône-Alpes","Auvergne-Rhône-Alpes",
                                     "Provence-Alpes-Côte d'Azur",
                                     "Centre-Val-de-Loire",
                                     "Corse",
                                     "Bretagne",
                                     "Ile-de-France",
                                     "Pays-de-la-Loire"))
  # Merge
  Dept_Region_GeoCodes=merge(Dept_Region_GeoCodes, NewRegions, by="OldRegions", all.x=T)
  
  # Add regions
  PairsDistanceTable_Regions=merge(PairsDistanceTable, Dept_Region_GeoCodes, by.x="SourceDepartment", by.y="Department", all.x=T)
  PairsDistanceTable_Regions=merge(PairsDistanceTable_Regions, Dept_Region_GeoCodes, by.x="TargetDepartment", by.y="Department", all.x=T)
  colnames(PairsDistanceTable_Regions)=c("Target.Department","Source.Department","X","Source","Source.Imported","ShortestPathDistance",
                                         "Source.Cases","Target","Mechanism","Source.OldRegion","Source.DeptName","Source.Latitude","Source.Longitude",
                                         "Source.NewRegion","Target.OldRegion","Target.DeptName","Target.Latitude","Target.Longitude","Target.NewRegions")
  PairsDistanceTable_Regions=PairsDistanceTable_Regions[,c("X","Source","Source.Department","Source.DeptName","Source.Imported","Source.Cases","Source.OldRegion","Source.Latitude","Source.Longitude",
                                                           "Source.NewRegion","ShortestPathDistance","Mechanism",
                                                           "Target","Target.Department","Target.DeptName","Target.OldRegion","Target.Latitude","Target.Longitude","Target.NewRegions")]
  PairsDistanceTable_Regions=PairsDistanceTable_Regions[order(PairsDistanceTable_Regions$X),]
  
  # Proportion of same dept source and target
  PercentSameDeptPairs=prop.table(table(PairsDistanceTable_Regions$Source.Department == PairsDistanceTable_Regions$Target.Department))[2]
  # Proportion of same region source and target
  PercentSameOldRegionPairs=prop.table(table(PairsDistanceTable_Regions$Source.OldRegion == PairsDistanceTable_Regions$Target.OldRegion))[2]
  PercentSameNewRegionPairs=prop.table(table(PairsDistanceTable_Regions$Source.NewRegion == PairsDistanceTable_Regions$Target.NewRegion))[2]
  
  # Distance between pairs
  library("geosphere")
  PairsDistanceTable_Regions$GeoDistanceBetweenPairs=foreach(i=1:nrow(PairsDistanceTable_Regions), .combine = "c") %do% distm(PairsDistanceTable_Regions[i,c("Source.Longitude","Source.Latitude")], PairsDistanceTable_Regions[i,c("Target.Longitude","Target.Latitude")],  fun=distHaversine)/1000
  # Mean geo distance among non-shared department pairs
  MeanDistNonSameDeptPairs=mean(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs)
  MeanDistNonSameDeptPairs
  
  MeanDistNonSameOldRegionPairs=mean(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.OldRegion != PairsDistanceTable_Regions$Target.OldRegion),]$GeoDistanceBetweenPairs)
  MeanDistNonSameOldRegionPairs
  MeanDistNonSameNewRegionPairs=mean(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.NewRegion != PairsDistanceTable_Regions$Target.NewRegion),]$GeoDistanceBetweenPairs)
  MeanDistNonSameNewRegionPairs
  
  DistNonSameDeptPairs=PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs
  NumberNonSameDeptPairs=length(DistNonSameDeptPairs)
  hist(DistNonSameDeptPairs, breaks=seq(from=0, to=900, by=100), freq = F, ylim = c(0,0.005))
  DistNonSameDeptPairs=data.frame(DistNonSameDeptPairs, Cut=cut(DistNonSameDeptPairs, seq(from=0, to=900, by=100)))
  DistributionDistNonSameDeptPairs=data.frame(prop.table(table(DistNonSameDeptPairs$Cut)))
  rownames(DistributionDistNonSameDeptPairs)=DistributionDistNonSameDeptPairs$Var1
  DistributionDistNonSameDeptPairs$Var1=NULL
  colnames(DistributionDistNonSameDeptPairs)=as.character(Year)
  
  # Get department pairs analysis summary table
  SpatialSummary=t(data.frame(UniqueSourceDepts,UniqueTargetDepts,
                              UniqueSourceDepts_Imported,UniqueTargetDepts_Imported,
                              PercentSameDeptPairs, PercentSameOldRegionPairs,
                              PercentSameNewRegionPairs, MeanDistNonSameDeptPairs,
                              MeanDistNonSameOldRegionPairs, MeanDistNonSameNewRegionPairs,
                              NumberNonSameDeptPairs))
  colnames(SpatialSummary)=as.character((Year))
  SpatialSummary=rbind(SpatialSummary, DistributionDistNonSameDeptPairs)
  
  #Save
  write.csv(SpatialSummary, file=paste0(writingDir,folder,"/SpatialSummary.csv"))
  
  return(SpatialSummary)
}

# Get all summaries
AllSpatialSummaries=foreach(i=c(2012,2013,2014,2015), .combine = "cbind") %do% getSpatialSummary(i)
# Save
write.table(AllSpatialSummaries, file=paste0(writingDir, "AllSpatialSummaries.csv"))
