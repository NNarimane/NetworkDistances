#####################################################################
#####DOES TRANSFER NETWORK HELP EXPLAIN CPE EPISODES (FUNCTIONS)#####
#####################################################################

################################################
#### CPE DATA WITH MECHANISM AND CLASS INFO ####
getCPEData=function(){
  
  ###########################################
  #### STEP 1a: GET DATA ON CPE EPISODES ####
  
  cat("Upload all data\n")
  data=read.csv(file=paste("Data/ALL_EPISODES (clean).csv", sep=""), header=T, sep=";", stringsAsFactors=FALSE)
  
  cat("Change dates to R dates\n")
  data$Department=as.character(data$Department)
  data$DateEpisode <- as.Date(data$DateEpisode, format = "%d/%m/%Y")
  data$DateOneCase <- as.Date(data$DateOneCase, format = "%d/%m/%Y")
  data$DateMoreOneCase <- as.Date(data$DateMoreOneCase, format = "%d/%m/%Y")
  data$DateImported <- as.Date(data$DateImported, format = "%d/%m/%Y")
  data$DateMoreFiveCases <- as.Date(data$DateMoreFiveCases, format = "%d/%m/%Y")
  
  # cat("Select data by dates\n")
  # cat("Choose start date\n")
  # startDate="2015-01-01"
  #
  # cat("Choose end date\n")
  # endDate="2015-06-30"
  #
  # cat("Get subset of data\n")
  # data=data[which(data$DateEpisode > as.Date(startDate) & data$DateEpisode < as.Date(endDate)),]
  
  ###########################################
  #### STEP 1b: MECHANISMS OF RESISTANCE ####
  
  cat("Split Multiple Mechanisms\n")
  Mechanisms=str_split_fixed(data$Mechanism, " ", 3)
  Mechanisms[Mechanisms == ""] = NA
  colnames(Mechanisms)=c("FirstMechanism", "SecondMechanism", "ThirdMechanism")
  Mechanisms=as.data.frame(Mechanisms, stringsAsFactors=FALSE)
  
  cat("Class Type\n")
  ClassA=c("SME", "IMI", "NMC", "GES", "KPC", "SHV", "SFC")
  ClassB=c("VIM", "IMP", "NDM")
  ClassD="OXA"
  
  cat("Add First Class Types\n")
  Mechanisms$FirstClass=NA
  Mechanisms$FirstClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassA"
  Mechanisms$FirstClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassB"
  Mechanisms$FirstClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassD"
  
  cat("Add Second Class Types\n")
  Mechanisms$SecondClass=NA
  Mechanisms$SecondClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassA"
  Mechanisms$SecondClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassB"
  Mechanisms$SecondClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassD"
  
  cat("Add Third Class Types\n")
  Mechanisms$ThirdClass=NA
  Mechanisms$ThirdClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassA"
  Mechanisms$ThirdClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassB"
  Mechanisms$ThirdClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassD"
  
  cat("Add Multiple Mechanisms to Data\n")
  data=cbind(data, Mechanisms)
  
  return(data)
}

##########################################################
#### GET CANDIDATE TRANSMITTERS BY MECHANISM OR CLASS ####
getCandidateTransmitters_byWeek_byMechanism=function(Week){
  
  ##############################################
  #### STEP 2: Find All Candidate Transmitters ####
  cat("#### STEP 2: Find All Candidate Transmitters ####\n")
  
  #A potential infector
  # - occured N days/Weeks before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
  # - shares the same mechanism OR shares same class (test mechanism)
  # - in the same bacteria?? (will not test yet)
  
  cat(paste("Set Number of Preceeding Weeks Equal to", Week, "\n"))
  Week=Week
  
  cat("Criterion 1: Episode Occured N Weeks Before\n")
  CandidateTransmitters_byWeeks=lapply(1:nrow(data), function(i){data[data[,2]<data[i,2] & data[,2]> (data[i,2]-Week*7),]})
  
  cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
  ImportationStatus="Imported"
  CandidateTransmitters_byWeeks_ImportationCorrection=foreach(i=1:length(CandidateTransmitters_byWeeks)) %do% {
    ConditionMet=data[i,ImportationStatus] == "N" 
    if(ConditionMet){
      CandidateTransmitters_byWeeks[[i]]=CandidateTransmitters_byWeeks[[i]]
    }
  }
  CandidateTransmitters_byWeeks_ImportationCorrection[sapply(CandidateTransmitters_byWeeks_ImportationCorrection, is.null)] = NULL
  
  cat("Criterion 3: Same CPE mechanism of resistance\n")
  FirstResistanceMechanism="FirstMechanism"
  CandidateTransmitters_byFirstMechanism=foreach(i=1:length(CandidateTransmitters_byWeeks_ImportationCorrection)) %do% {
    CandidateTransmitters=CandidateTransmitters_byWeeks_ImportationCorrection[[i]]
    CandidateTransmitters[CandidateTransmitters[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
  }
  SecondResistanceMechanism="SecondMechanism"
  CandidateTransmitters_bySecondMechanism=foreach(i=1:length(CandidateTransmitters_byFirstMechanism)) %do% {
    CandidateTransmitters=CandidateTransmitters_byFirstMechanism[[i]]
    CandidateTransmitters[CandidateTransmitters[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
  }
  ThirdResistanceMechanism="ThirdMechanism"
  CandidateTransmitters_byThirdMechanism=foreach(i=1:length(CandidateTransmitters_bySecondMechanism)) %do% {
    CandidateTransmitters=CandidateTransmitters_bySecondMechanism[[i]]
    CandidateTransmitters[CandidateTransmitters[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
  }
  
  return(CandidateTransmitters_byThirdMechanism)
}
getCandidateTransmitters_byWeek_byClass=function(Week){
  
  ##############################################
  #### STEP 2: Find All Potential Infectors ####
  cat("#### STEP 2: Find All Candidate Transmitters ####\n")
  
  #A potential infector
  # - occured N days/Weeks before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
  # - shares the same mechanism OR shares same class (test mechanism)
  # - in the same bacteria?? (will not test yet)
  
  cat(paste("Set Number of Preceeding Days Equal to", Week, "\n"))
  Week=Week
  
  cat("Criterion 1: Episode Occured N Weeks Before\n")
  CandidateTransmitters_byWeeks=lapply(1:nrow(data), function(i){data[data[,2]<data[i,2] & data[,2]> (data[i,2]-Week*7),]})
  
  cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
  ImportationStatus="Imported"
  CandidateTransmitters_byWeeks_ImportationCorrection=foreach(i=1:length(CandidateTransmitters_byWeeks)) %do% {
    ConditionMet=data[i,ImportationStatus] == "N" 
    if(ConditionMet){
      CandidateTransmitters_byWeeks[[i]]=CandidateTransmitters_byWeeks[[i]]
    }
  }
  CandidateTransmitters_byWeeks_ImportationCorrection[sapply(CandidateTransmitters_byWeeks_ImportationCorrection, is.null)] = NULL
  
  
  cat("Criterion 3: Same CPE Class of resistance\n")
  FirstResistanceClass="FirstClass"
  CandidateTransmitters_byFirstClass=foreach(i=1:length(CandidateTransmitters_byWeeks_ImportationCorrection)) %do% {
    CandidateTransmitters=CandidateTransmitters_byWeeks_ImportationCorrection[[i]]
    CandidateTransmitters[CandidateTransmitters[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
  }
  SecondResistanceClass="SecondClass"
  CandidateTransmitters_bySecondClass=foreach(i=1:length(CandidateTransmitters_byFirstClass)) %do% {
    CandidateTransmitters=CandidateTransmitters_byFirstClass[[i]]
    CandidateTransmitters[CandidateTransmitters[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
  }
  ThirdResistanceClass="ThirdClass"
  CandidateTransmitters_byThirdClass=foreach(i=1:length(CandidateTransmitters_bySecondClass)) %do% {
    CandidateTransmitters=CandidateTransmitters_bySecondClass[[i]]
    CandidateTransmitters[CandidateTransmitters[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
  }
  return(CandidateTransmitters_byThirdClass)
}

########################################################
#### GET MINIMUM DISTANCE OF CANDIDATE TRANSMITTERS ####
getMinimumDistances_CandidateTransmitters_byWeek=function(i, CandidateTransmitters, weights, algorithm){
  ##############################################################################################
  #### STEP 3: Calculate Minimum (Network) Distance Between Episode and Potential Infectors ####
  cat("#### STEP 3: Calculate Minimum (Network) Distance Between Episode and Potential Infectors ####\n")
  
  cat("List of Departments of Episode and Candidate Transmitters\n")
  Department="Department"
  CandidateTransmitters_Departments=foreach(i=1:length(CandidateTransmitters)) %do% {
    CandidateTransmitters_Departments=CandidateTransmitters[[i]]
    CandidateTransmitters_Departments[Department]
  }
  
  cat("Distance Matrix Between Departments\n")
  Distances_Matrix=as.data.frame(distances(directed.graph_Dept, weights = weights, algorithm = algorithm))
  
  cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
  CandidateTransmitters_Departments_MinDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
    CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
    if(nrow(CandidateTransmitters_Departments_Subset) > 0){
      Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
        Distances=Distances_Matrix[data[i,Department],CandidateTransmitters_Departments_Subset[j,Department]]
      } 
    }else{
      Distances=NA
    }
    
  }
  
  cat("Get Minimum Distance Between Episode and 1 Potential Infector\n")
  MinDistance=lapply(CandidateTransmitters_Departments_MinDistances, function(x) min(x))
  
  cat(paste("Unlist, Remove Inf, Minimum Distances", i, "\n"))
  MinimumDistances=unlist(MinDistance)
  
  return(MinimumDistances)
}

############################################################
#### GET CANDIATE TRANSMITTERS FROM RANDOM PERMUTATIONS ####
getCandidateTransmitters_Permutations_byWeek=function(Week){
  cat("Permutate Data: Mechanisms & Department\n")
  Permutation=cbind(data[,c(1:4,6:13)], data[sample(1:nrow(data)),c(5,14:21)])
  
  cat(paste("Set Number of Preceeding Weeks Equal to", Week, "\n"))
  Week=Week
  
  cat("Criterion 1: Candidate Transmitter Occured at least N Weeks Before Episode\n")
  Candidate_Transmitter_Permutations_byWeek=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-Week*7 & data$DateEpisode < data[i,2]),]
  
  cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
  ImportationStatus="Imported"
  Candidate_Transmitter_Permutations_byWeek_ImportationCorrection=foreach(i=1:length(Candidate_Transmitter_Permutations_byWeek)) %do% {
    ConditionMet=data[i,ImportationStatus] == "N" 
    if(ConditionMet){
      Candidate_Transmitter_Permutations_byWeek[[i]]=Candidate_Transmitter_Permutations_byWeek[[i]]
    }
  }
  Candidate_Transmitter_Permutations_byWeek_ImportationCorrection[sapply(Candidate_Transmitter_Permutations_byWeek_ImportationCorrection, is.null)] = NULL
  
  return(Candidate_Transmitter_Permutations_byWeek_ImportationCorrection)
}

###########################
#### CLEANING FUNCTION ####
CleaningFunction=function(x){
  x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
  x <- lapply(x, function(x) {x[c(1:length(x))]})
  return(x)
} 

##################################################################################
#### GET AVERAGE MINIMUM DISTANCE OF CANDIDATE TRANSMITTERS FROM PERMUTATIONS ####
getAverageMinDistances_CandidateTransmitters_Permutations_byWeek=function(MinimumDistances_PotentialInfector_Permutation){
  cat(paste("Average", Week, "runs\n"))
  CandidateTransmitters_Permutations_byWeek=foreach(i=1:length(MinimumDistances_PotentialInfector_Permutation[[1]])) %do% lapply(MinimumDistances_PotentialInfector_Permutation, `[[`, i) #get first elements i of each list
  
  cat("Run Cleaning Function for Random Simulations and Original Results\n")
  CandidateTransmitters_Permutations_byWeek_Clean=foreach(i=1:length(CandidateTransmitters_Permutations_byWeek)) %do% CleaningFunction(CandidateTransmitters_Permutations_byWeek[[i]])
  
  cat("Convert sub-list of",Nruns," values into dataframe to facilitate per case averages\n")
  CandidateTransmitters_Permutations_byWeek_Dataframes=foreach(i=1:length(CandidateTransmitters_Permutations_byWeek_Clean)) %do% data.frame(CandidateTransmitters_Permutations_byWeek_Clean[[i]], row.names = NULL) 
  
  cat("Row Means of",Nruns," simulations for every case\n")
  CandidateTransmitters_Permutations_byWeek_RowMeans=foreach(i=1:length(CandidateTransmitters_Permutations_byWeek_Dataframes)) %do% rowMeans(CandidateTransmitters_Permutations_byWeek_Dataframes[[i]], na.rm=TRUE)
  
  return(CandidateTransmitters_Permutations_byWeek_RowMeans)
}

##################################################
#### WILCOXON PAIRED RANKED SUM TEST FUNCTION ####
getWilcoxonPairedRankTestPValues=function(i, MinimumDistances, MinimumDistances_Permutations){
  # cat("Clean out Inf values from MinimumDistances\n")
  # MinimumDistances_Clean=CleaningFunction(MinimumDistances)
  cat(paste("Combine Non-Random and Random Minimum Distances for Week", i, "\n"))
  CombinedMinimumDistances=cbind(MinimumDistances[[i]], MinimumDistances_Permutations[[i]])
  cat("Test only for lengths greater than 3\n")
  length=nrow(CombinedMinimumDistances)
  if(length > 3L){
    cat(paste("Run Test for Week", i, "\n"))
    CombinedMinimumDistances_WilcoxonTest=wilcox.test(CombinedMinimumDistances[,1], CombinedMinimumDistances[,2], paired = T)
    #H0: distributions of calculated potential infector min distances is the same as the distribution of min distances of random potetial infectors
  }else{
    CombinedMinimumDistances_WilcoxonTest$p.value<-NA
  }
  return(CombinedMinimumDistances_WilcoxonTest$p.value)
}

####################################################
#### PROPORTIONS OF VALUES UNDER 5TH PERCENTILE ####
get5thQuantiles=function(Week, MinimumDistances, AllRandomMinimumDistances){
  cat("Convert random permutations into dataframes\n")
  RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
  RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
  cat("Get proportion cases that have values smaller than the 5th percentile of simulations for each week N\n")
  ProportionsTable=foreach(n=1:Week, .combine = "cbind") %do% {
    day=foreach(i=1:nrow(data)) %do% (MinimumDistances[[n]][i] <= quantile(RandomSimulationsByDays_Dataframes[[n]][i,], 0.05, na.rm=T)[[1]])
    day=unlist(day)
    resultsTable=prop.table(table(day))
  }
  return(ProportionsTable)
}
getMeanMinimumDistances=function(MinimumDistances, AllRandomMinimumDistances){
  Week=Week
  
  cat("Convert random simulations into dataframes\n")
  RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
  RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
  cat(paste("Row Means of", Week, "permutations for every case\n"))
  RandomSimulationsByDays_RowMeans=foreach(i=1:length(RandomSimulationsByDays_Dataframes)) %do% rowMeans(RandomSimulationsByDays_Dataframes[[i]], na.rm=TRUE)
  
  cat(paste("Min distance means of", Week, "permutations for every Week N\n"))
  RandomSimulationsByDays_MeansByNDays=foreach(i=1:length(RandomSimulationsByDays_RowMeans)) %do% mean(unlist(RandomSimulationsByDays_RowMeans[[i]]), na.rm = T)
  
  cat("Mean distance for every Week N for potential infectors\n")
  MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
  MinimumDistances_MeansByNDays=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
  
  cat("Get table\n")
  MinDistancesTableByMeanPerNDays=as.data.frame(cbind(unlist(MinimumDistances_MeansByNDays), unlist(RandomSimulationsByDays_MeansByNDays)))
  colnames(MinDistancesTableByMeanPerNDays)=c("MinimumDistances_MeansByNDays", "RandomSimulationsByDays_MeansByNDays")
  
  return(MinDistancesTableByMeanPerNDays)
}






#################################################################################################################################################################
################################## OLD ##########################################################################################################################
#################################################################################################################################################################

# ################################################
# #### CPE DATA WITH MECHANISM AND CLASS INFO ####
# getCPEData=function(){
#   
#   ###########################################
#   #### STEP 1a: GET DATA ON CPE EPISODES ####
#   
#   cat("Upload all data\n")
#   data=read.csv(file=paste("Data/ALL_EPISODES (clean).csv", sep=""), header=T, sep=";", stringsAsFactors=FALSE)
#   
#   cat("Change dates to R dates\n")
#   data$Department=as.character(data$Department)
#   data$DateEpisode <- as.Date(data$DateEpisode, format = "%d/%m/%Y")
#   data$DateOneCase <- as.Date(data$DateOneCase, format = "%d/%m/%Y")
#   data$DateMoreOneCase <- as.Date(data$DateMoreOneCase, format = "%d/%m/%Y")
#   data$DateImported <- as.Date(data$DateImported, format = "%d/%m/%Y")
#   data$DateMoreFiveCases <- as.Date(data$DateMoreFiveCases, format = "%d/%m/%Y")
#   
#   # cat("Select data by dates\n")
#   # cat("Choose start date\n")
#   # startDate="2015-01-01"
#   #
#   # cat("Choose end date\n")
#   # endDate="2015-06-30"
#   #
#   # cat("Get subset of data\n")
#   # data=data[which(data$DateEpisode > as.Date(startDate) & data$DateEpisode < as.Date(endDate)),]
#   
#   ###########################################
#   #### STEP 1b: MECHANISMS OF RESISTANCE ####
#   
#   cat("Split Multiple Mechanisms\n")
#   Mechanisms=str_split_fixed(data$Mechanism, " ", 3)
#   Mechanisms[Mechanisms == ""] = NA
#   colnames(Mechanisms)=c("FirstMechanism", "SecondMechanism", "ThirdMechanism")
#   Mechanisms=as.data.frame(Mechanisms, stringsAsFactors=FALSE)
#   
#   cat("Class Type\n")
#   ClassA=c("SME", "IMI", "NMC", "GES", "KPC", "SHV", "SFC")
#   ClassB=c("VIM", "IMP", "NDM")
#   ClassD="OXA"
#   
#   cat("Add First Class Types\n")
#   Mechanisms$FirstClass=NA
#   Mechanisms$FirstClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassA"
#   Mechanisms$FirstClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassB"
#   Mechanisms$FirstClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassD"
#   
#   cat("Add Second Class Types\n")
#   Mechanisms$SecondClass=NA
#   Mechanisms$SecondClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassA"
#   Mechanisms$SecondClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassB"
#   Mechanisms$SecondClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassD"
#   
#   cat("Add Third Class Types\n")
#   Mechanisms$ThirdClass=NA
#   Mechanisms$ThirdClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassA"
#   Mechanisms$ThirdClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassB"
#   Mechanisms$ThirdClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassD"
#   
#   cat("Add Multiple Mechanisms to Data\n")
#   data=cbind(data, Mechanisms)
#   
#   return(data)
# }
# 
# #######################################
# #### POTENTIAL INFECTORS FUNCTIONS ####
# getPotentialInfectors_byMechanism=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-NDays & data$DateEpisode < data[i,2]),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE mechanism of resistance\n")
#   FirstResistanceMechanism="FirstMechanism"
#   PotentialInfectors_byFirstMechanism=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
#   }
#   SecondResistanceMechanism="SecondMechanism"
#   PotentialInfectors_bySecondMechanism=foreach(i=1:length(PotentialInfectors_byFirstMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
#   }
#   ThirdResistanceMechanism="ThirdMechanism"
#   PotentialInfectors_byThirdMechanism=foreach(i=1:length(PotentialInfectors_bySecondMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
#   }
#   return(PotentialInfectors_byThirdMechanism)
# }
# getPotentialInfectors_byClass=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-NDays & data$DateEpisode < data[i,2]),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE Class of resistance\n")
#   FirstResistanceClass="FirstClass"
#   PotentialInfectors_byFirstClass=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
#   }
#   SecondResistanceClass="SecondClass"
#   PotentialInfectors_bySecondClass=foreach(i=1:length(PotentialInfectors_byFirstClass)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstClass[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
#   }
#   ThirdResistanceClass="ThirdClass"
#   PotentialInfectors_byThirdClass=foreach(i=1:length(PotentialInfectors_bySecondClass)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondClass[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
#   }
#   return(PotentialInfectors_byThirdClass)
# }
# 
# ########################################
# #### GET NETWORK DISTANCES FUNCTION ####
# getMinimumDistances=function(i, PotentialInfectors, weights, algorithm){
#   ##############################################################################################
#   #### STEP 3: Calculate Minimum (Network) Distance Between Episode and Potential Infectors ####
#   cat("#### STEP 3: Calculate Minimum (Network) Distance Between Episode and Potential Infectors ####\n")
#   
#   cat("List of Departments of Episode and Potential Infectors\n")
#   Department="Department"
#   PotentialInfectors_Departments=foreach(i=1:length(PotentialInfectors)) %do% {
#     PotentialInfector_Departments=PotentialInfectors[[i]]
#     PotentialInfector_Departments[Department]
#   }
#   
#   cat("Distance Matrix Between Departments\n")
#   Distances_Matrix=as.data.frame(distances(directed.graph_Dept, weights = weights, algorithm = algorithm))
#   cat("Mean Distance Between Departments\n")
#   mean_distance(directed.graph_Dept)
#   
#   cat("Distance Between Department of Episode and Potential Infectors Departments\n")
#   PotentialInfectors_Departments_MinDistances=foreach(i=1:length(PotentialInfectors_Departments)) %do% {
#     PotentialInfectors_Departments_Subset=PotentialInfectors_Departments[[i]]
#     Distances=foreach(j=1:length(PotentialInfectors_Departments_Subset)) %do% {
#       Distances=Distances_Matrix[data[i,Department],PotentialInfectors_Departments_Subset[j,Department]]
#     }
#     MinDistance=lapply(Distances, min)
#   }
#   
#   cat(paste("Unlist, Remove Inf, Minimum Distances", i, "\n"))
#   MinimumDistances=unlist(PotentialInfectors_Departments_MinDistances)
#   #MinimumDistances=MinimumDistances[which(is.finite(MinimumDistances))]
#   
#   return(MinimumDistances)
# }
# 
# #################################################
# #### GET RANDOM POTENTIAL INFECTORS FUNCTION ####
# getRandomPotentialInfectors=function(NDays, PotentialInfectors){
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-NDays & data$DateEpisode < data[i,2]),]
#   
#   cat("Criterion 4: Same Number of Random Infectors as Original Potential Infector\n")
#   randomRows = function(df,n){
#     return(df[sample(nrow(df),n),])
#   }
#   PotentialInfectors_Size=lapply(PotentialInfectors, nrow)
#   Random_PotentialInfectors=foreach(i=1:length(PotentialInfectors_byDays)) %do% randomRows(PotentialInfectors_byDays[[i]], PotentialInfectors_Size[[i]])
#   return(Random_PotentialInfectors)
# }
# getRandomMinimumDistances=function(PotentialInfectors){
#   cat(paste("Get Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   PotentialInfectors_RI=foreach(i=1:MaxDays) %do% getRandomPotentialInfectors(i, PotentialInfectors[[i]])
#   cat(paste("Get Minimum Distances for Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   MinimumDistances_RI=foreach(i=1:length(PotentialInfectors_RI)) %do% getMinimumDistances(i, PotentialInfectors_RI[[i]], weights = weights, algorithm = algorithm)
#   return(MinimumDistances_RI)
# }
# 
# #############################################
# #### AVERAGE RANDOM SIMULATIONS FUNCTION ####
# 
# #### CLEANING FUNCTION ####
# CleaningFunction=function(x){
#   x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
#   x <- lapply(x, function(x) {x[c(1:2686)]})
#   return(x)
# } 
# CleaningFunction2=function(x){
#   x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
#   return(x)
# } 
# getAverageRandomSimulationsMinDistances=function(AllRandomMinimumDistances){
#   cat(paste("Average", Nruns, "runs\n"))
#   RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
#   
#   cat("Run Cleaning Function for Random Simulations and Original Results\n")
#   RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
#   
#   cat("Convert sub-list of 100 values into dataframe to facilitate per case averages\n")
#   RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
#   
#   cat("Row Means of 100 simulations for every case\n")
#   RandomSimulationsByDays_RowMeans=foreach(i=1:length(RandomSimulationsByDays_Dataframes)) %do% rowMeans(RandomSimulationsByDays_Dataframes[[i]], na.rm=TRUE)
#   
#   return(RandomSimulationsByDays_RowMeans)
# }
# 
# ##################################################
# #### WILCOXON PAIRED RANKED SUM TEST FUNCTION ####
# getWilcoxonPairedRankTestPValues=function(i, MinimumDistances, AllRandomMinimumDistances){
#   cat("Clean out Inf values from MinimumDistances\n")
#   MinimumDistances_Clean=CleaningFunction(MinimumDistances)
#   cat(paste("Combine Non-Random and Random Minimum Distances for", i, "Days\n"))
#   CombinedMinimumDistances=cbind(MinimumDistances_Clean[[i]], AllRandomMinimumDistances[[i]])
#   cat("Test only for lengths greater than 3\n")
#   length=nrow(CombinedMinimumDistances)
#   if(length > 3L){
#     cat(paste("Run Test for", i, "Days\n"))
#     CombinedMinimumDistances_WilcoxonTest=wilcox.test(CombinedMinimumDistances[,1], CombinedMinimumDistances[,2], paired = T)
#     #H0: distributions of calculated potential infector min distances is the same as the distribution of min distances of random potetial infectors
#   }else{
#     CombinedMinimumDistances_WilcoxonTest$p.value<-NA
#   }
#   return(CombinedMinimumDistances_WilcoxonTest$p.value)
# }
# 
# ####################################################
# #### PROPORTIONS OF VALUES UNDER 5TH PERCENTILE ####
# get5thQuantiles=function(Week, MinimumDistances, AllRandomMinimumDistances){
#   cat("Convert random permutations into dataframes\n")
#   RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
#   RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
#   RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
#   cat("Get proportion cases that have values smaller than the 5th percentile of simulations for each week N\n")
#   ProportionsTable=foreach(n=1:Week, .combine = "cbind") %do% {
#     day=foreach(i=1:nrow(data)) %do% (MinimumDistances[[n]][i] <= quantile(RandomSimulationsByDays_Dataframes[[n]][i,], 0.05, na.rm=T)[[1]])
#     day=unlist(day)
#     resultsTable=prop.table(table(day))
#   }
#   return(ProportionsTable)
# }
# getMeanMinimumDistances=function(MinimumDistances, AllRandomMinimumDistances){
#   Week=Week
#   
#   cat("Convert random simulations into dataframes\n")
#   RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
#   RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
#   RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
#   cat(paste("Row Means of", Week, "permutations for every case\n"))
#   RandomSimulationsByDays_RowMeans=foreach(i=1:length(RandomSimulationsByDays_Dataframes)) %do% rowMeans(RandomSimulationsByDays_Dataframes[[i]], na.rm=TRUE)
#   
#   cat(paste("Min distance means of", Week, "permutations for every Week N\n"))
#   RandomSimulationsByDays_MeansByNDays=foreach(i=1:length(RandomSimulationsByDays_RowMeans)) %do% mean(unlist(RandomSimulationsByDays_RowMeans[[i]]), na.rm = T)
#   
#   cat("Mean distance for every Week N for potential infectors\n")
#   MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
#   MinimumDistances_MeansByNDays=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
#   
#   cat("Get table\n")
#   MinDistancesTableByMeanPerNDays=as.data.frame(cbind(unlist(MinimumDistances_MeansByNDays), unlist(RandomSimulationsByDays_MeansByNDays)))
#   colnames(MinDistancesTableByMeanPerNDays)=c("MinimumDistances_MeansByNDays", "RandomSimulationsByDays_MeansByNDays")
#   
#   return(MinDistancesTableByMeanPerNDays)
# }
# 
# ###################################################
# #### POTENTIAL INFECTORS FUNCTIONS (DAY RANGE) ####
# getPotentialInfectors_DayRange_byMechanism=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE mechanism of resistance\n")
#   FirstResistanceMechanism="FirstMechanism"
#   PotentialInfectors_byFirstMechanism=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
#   }
#   SecondResistanceMechanism="SecondMechanism"
#   PotentialInfectors_bySecondMechanism=foreach(i=1:length(PotentialInfectors_byFirstMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
#   }
#   ThirdResistanceMechanism="ThirdMechanism"
#   PotentialInfectors_byThirdMechanism=foreach(i=1:length(PotentialInfectors_bySecondMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
#   }
#   return(PotentialInfectors_byThirdMechanism)
# }
# getPotentialInfectors_DayRange_byClass=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE Class of resistance\n")
#   FirstResistanceClass="FirstClass"
#   PotentialInfectors_byFirstClass=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
#   }
#   SecondResistanceClass="SecondClass"
#   PotentialInfectors_bySecondClass=foreach(i=1:length(PotentialInfectors_byFirstClass)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstClass[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
#   }
#   ThirdResistanceClass="ThirdClass"
#   PotentialInfectors_byThirdClass=foreach(i=1:length(PotentialInfectors_bySecondClass)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondClass[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
#   }
#   return(PotentialInfectors_byThirdClass)
# }
# getRandomPotentialInfectors_DayRange=function(NDays, PotentialInfectors){
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 4: Same Number of Random Infectors as Original Potential Infector\n")
#   randomRows = function(df,n){
#     return(df[sample(nrow(df),n),])
#   }
#   PotentialInfectors_Size=lapply(PotentialInfectors, nrow)
#   Random_PotentialInfectors=foreach(i=1:length(PotentialInfectors_byDays)) %do% randomRows(PotentialInfectors_byDays[[i]], PotentialInfectors_Size[[i]])
#   return(Random_PotentialInfectors)
# }
# getRandomMinimumDistances_DayRange=function(PotentialInfectors){
#   cat(paste("Get Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   PotentialInfectors_RI=foreach(i=1:MaxDays) %do% getRandomPotentialInfectors_DayRange(i, PotentialInfectors[[i]])
#   cat(paste("Get Minimum Distances for Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   MinimumDistances_RI=foreach(i=1:length(PotentialInfectors_RI)) %do% getMinimumDistances(i, PotentialInfectors_RI[[i]], weights = weights, algorithm = algorithm)
#   return(MinimumDistances_RI)
# }
# 
# ###################################################
# #### POTENTIAL INFECTORS FUNCTIONS (DAY RANGE) ####
# getPotentialInfectors_DayRange_byMechanism=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE mechanism of resistance\n")
#   FirstResistanceMechanism="FirstMechanism"
#   PotentialInfectors_byFirstMechanism=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
#   }
#   SecondResistanceMechanism="SecondMechanism"
#   PotentialInfectors_bySecondMechanism=foreach(i=1:length(PotentialInfectors_byFirstMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
#   }
#   ThirdResistanceMechanism="ThirdMechanism"
#   PotentialInfectors_byThirdMechanism=foreach(i=1:length(PotentialInfectors_bySecondMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
#   }
#   return(PotentialInfectors_byThirdMechanism)
# }
# getPotentialInfectors_DayRange_byClass=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE Class of resistance\n")
#   FirstResistanceClass="FirstClass"
#   PotentialInfectors_byFirstClass=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
#   }
#   SecondResistanceClass="SecondClass"
#   PotentialInfectors_bySecondClass=foreach(i=1:length(PotentialInfectors_byFirstClass)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstClass[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
#   }
#   ThirdResistanceClass="ThirdClass"
#   PotentialInfectors_byThirdClass=foreach(i=1:length(PotentialInfectors_bySecondClass)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondClass[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
#   }
#   return(PotentialInfectors_byThirdClass)
# }
# getRandomPotentialInfectors_DayRange=function(NDays, PotentialInfectors){
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 4: Same Number of Random Infectors as Original Potential Infector\n")
#   randomRows = function(df,n){
#     return(df[sample(nrow(df),n),])
#   }
#   PotentialInfectors_Size=lapply(PotentialInfectors, nrow)
#   Random_PotentialInfectors=foreach(i=1:length(PotentialInfectors_byDays)) %do% randomRows(PotentialInfectors_byDays[[i]], PotentialInfectors_Size[[i]])
#   return(Random_PotentialInfectors)
# }
# getRandomMinimumDistances_DayRange=function(PotentialInfectors){
#   cat(paste("Get Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   PotentialInfectors_RI=foreach(i=1:MaxDays) %do% getRandomPotentialInfectors_DayRange(i, PotentialInfectors[[i]])
#   cat(paste("Get Minimum Distances for Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   MinimumDistances_RI=foreach(i=1:length(PotentialInfectors_RI)) %do% getMinimumDistances(i, PotentialInfectors_RI[[i]], weights = weights, algorithm = algorithm)
#   return(MinimumDistances_RI)
# }