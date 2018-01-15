######################################################
##### Histograms of observed and permutated data #####
######################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

#######################
#### HIST FUNCTION ####
#######################

getHistograms=function(main, Day, MinimumDistances, AllPermutatationMinimumDistances, breaks){
  #Observed Data
  MinimumDistances=MinimumDistances[[Day]]
  
  #Permutations
  cat("Get first elements i of each list\n")
  CandidateTransmitters_Permutations_byDay=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) 
  cat("Run Cleaning Function for Random Simulations and Original Results\n")
  CandidateTransmitters_Permutations_byDay_Clean=foreach(i=1:length(CandidateTransmitters_Permutations_byDay)) %do% CleaningFunction(CandidateTransmitters_Permutations_byDay[[i]])
  cat("Convert sub-list of values into dataframe to facilitate per case averages\n")
  CandidateTransmitters_Permutations_byDay_Dataframes=foreach(i=1:length(CandidateTransmitters_Permutations_byDay_Clean)) %do% data.frame(CandidateTransmitters_Permutations_byDay_Clean[[i]], row.names = NULL) 
  cat("Get first elements i of each list\n")
  PermutationsMinimumDistances=DaySelection=CandidateTransmitters_Permutations_byDay_Dataframes[[Day]]
  # DaySelection=CandidateTransmitters_Permutations_byDay_Dataframes[[Day]]
  # PermutationsMinimumDistances=rowMeans(DaySelection)
  
  # df <- data.frame(MinimumDistances=MinimumDistances, PermutationsMinimumDistances=PermutationsMinimumDistances)
  # ggplot(melt(df), aes(value, fill = variable)) + geom_histogram(position = "dodge")
  
  MinimumDistancesSubset=MinimumDistances[MinimumDistances <= Cutoff]
  PermutationsMinimumDistancesSubset=unlist(PermutationsMinimumDistances)
  PermutationsMinimumDistancesSubset=PermutationsMinimumDistancesSubset[PermutationsMinimumDistancesSubset <= Cutoff]
  
  # l <- list(unlist(PermutationsMinimumDistances), MinimumDistances)
  l <- list(PermutationsMinimumDistancesSubset, MinimumDistancesSubset)
  
  # breaks <- pretty(unlist(l))
  # breaks=as.numeric(c(-1, 0, 1, 2, 3,4,5,6,7,8,9,10))
  # breaks=as.numeric(c(-0.1, 0, 0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
  # breaks=as.numeric(c(-0.01, 0, 0.01, 0.02, 0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,
  #                     0.2,0.3,0.4,0.5,0.6))
  breaks=as.numeric(seq(from=-1, to=Cutoff, by=1))
  
  # levs = levels(cut(unlist(l), breaks=breaks))
  levs = as.character(breaks + 1)
  # levs = as.character(breaks + 0.01)
  levs=levs[1:length(breaks)-1]
  
  if(Legend){
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=F, xlab="Network Distances", 
                       ylab="Density", main=main, cex.axis=1, 
                       ylim=ylim, cex.main=5, cex.lab=3,
                       legend.text=c("Permutations", "Observed"),
                       col=c("aquamarine4", "aquamarine3"))
  }else{
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=F, xlab="Network Distances", 
                       ylab="Density", main=main, cex.axis=1, 
                       ylim=ylim, cex.main=5, cex.lab=3,
                       col=c("aquamarine4", "aquamarine3"))
  }
  

  return(FinalHist)
}

#############
#### DAY ####
#############

Day=21

####################
#### PARAMETERS ####
####################

#Layout
# par(mfrow=c(2,2))
layout(matrix(c(1,2,
                1,2,
                1,2,
                1,2,
                1,2,
                3,4,
                3,4,
                3,4,
                3,4,
                3,4,
                5,5), 
              nrow=11, byrow = TRUE))
layout.show(n=5)

par(mar=c(4,4,4,4))

ylim=c(0,0.4)
# ylim=c(0,5)

Cutoff=24

##############
#### HIST ####
##############

#2012
load(paste0(writingDir,"Jan 15 Results/", as.character(2012), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"Jan 15 Results/", as.character(2012), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2012=getHistograms("2012", Day, MinimumDistances, AllPermutatationMinimumDistances, breaks)
Hist2012

#2013
load(paste0(writingDir,"Jan 10 Results/", as.character(2013), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"Jan 10 Results/", as.character(2013), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2013=getHistograms("2013", Day, MinimumDistances, AllPermutatationMinimumDistances)
Hist2013

#2014
load(paste0(writingDir,"Jan 10 Results/", as.character(2014), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"Jan 10 Results/", as.character(2014), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2014=getHistograms("2014", Day, MinimumDistances, AllPermutatationMinimumDistances)
Hist2014

#2015
load(paste0(writingDir,"Jan 10 Results/", as.character(2015), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"Jan 10 Results/", as.character(2015), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2015=getHistograms("2015", Day, MinimumDistances, AllPermutatationMinimumDistances)
Hist2015

par(mar=c(1,1,1,1))
plot.new()
legend(x="center",legend=c("Permutations", "Observed"),
       horiz = TRUE, fill=c("aquamarine4", "aquamarine3"))

