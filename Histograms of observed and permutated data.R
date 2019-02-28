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
  
  if(Density){
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=F, xlab="Network Distance", 
                       ylab="Density", main=main, cex.axis=1, 
                       ylim=ylim, cex.main=5, cex.lab=3,
                       col=c("aquamarine4", "aquamarine3"))
  }else{
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=T, xlab="Network Distance", 
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
                1,2,
                1,2,
                3,4,
                3,4,
                3,4,
                3,4,
                3,4,
                3,4,
                3,4,
                5,5), 
              nrow=15, byrow = TRUE))
# layout.show(n=5)

par(mar=c(4,4,4,0))

ylim=c(0,0.4)
# ylim=c(0,5)

Cutoff=20

##############
#### HIST ####
##############

#2012-2015
load(paste0(writingDir,"2012-2015 All Results/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"2012-2015 All Results/AllDistances500.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances500
Density=T
Hist2012to2015=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances, breaks)
title("2012-2015", adj=0, cex.main=1.8, font.main = 1)
DeptTable=matrix(c(Hist2012to2015[[2]][2,1], c(1-Hist2012to2015[[2]][2,1]),
                   Hist2012to2015[[2]][1,1], c(1-Hist2012to2015[[2]][1,1])),
                 ncol=2, byrow=F)
rownames(DeptTable)=c("SameDept", "DiffDept")
colnames(DeptTable)=c("Observed", "Permutations")
DeptTable=as.table(DeptTable)
Test=DeptTable*100
#Tests
Result=chisq.test(Test)
Result


dev.copy(jpeg, "Histograms FINAL V2.jpg", height=8, width=12, units="in", res=300)
dev.copy(pdf, "Histograms FINAL V2.pdf", height=8, width=12)

#2012
load(paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/AllDistances.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances

Density=T
Hist2012=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances, breaks)
title("2012", adj=0, cex.main=1.8, font.main = 1)

#2013
load(paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/AllDistances.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances

Density=T
Hist2013=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances)
title("2013", adj=0, cex.main=1.8, font.main = 1)


#2014
load(paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/AllDistances.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances

Density=T
Hist2014=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances)
title("2014", adj=0, cex.main=1.8, font.main = 1)

#2015
load(paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/AllDistances.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances

Density=T
Hist2015=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances)
title("2015", adj=0, cex.main=1.8, font.main = 1)

# Legend
par(mar=c(1,1,1,1))
plot.new()
legend(x="center",legend=c("Mean of 500 Permutations", "Observed Data"),
       horiz = TRUE, fill=c("aquamarine4", "aquamarine3"))

dev.off()

######################################################
#### Chi2 Test: same department vs. non-same dept ####

getChi2TestSameVSDiffDept_Density=function(Year){
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/AllDistances.RData"))
  MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  AllPermutatationMinimumDistances=AllDistances
  
  Density=T
  Hist=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances, breaks)
  
  DeptTable=matrix(c(Hist[[2]][2,1], c(1-Hist[[2]][2,1]),
                     Hist[[2]][1,1], c(1-Hist[[2]][1,1])),
                   ncol=2, byrow=F)
  rownames(DeptTable)=c("SameDept", "DiffDept")
  colnames(DeptTable)=c("Observed", "Permutations")
  DeptTable=as.table(DeptTable)
  DeptTable
  #Tests
  Result=chisq.test(DeptTable[1,], DeptTable[2,])
  return(Result)
}
getChi2TestSameVSDiffDept_Counts=function(Year){
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/AllDistances.RData"))
  MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  AllPermutatationMinimumDistances=AllDistances
  Density=F
  Hist=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances, breaks)
  DeptTable=matrix(c(Hist[[2]][2,1], sum(Hist[[2]][2,2:length(Hist[[2]][2,])]),
                     Hist[[2]][1,1], sum(Hist[[2]][1,2:length(Hist[[2]][1,])])),
                   ncol=2, byrow=F)
  rownames(DeptTable)=c("SameDept", "DiffDept")
  colnames(DeptTable)=c("Observed", "Permutations")
  DeptTable=as.table(DeptTable)
  DeptTable
  #Tests
  Result=chisq.test(DeptTable[1,], DeptTable[2,])
  return(Result)
}
getChi2TestAllDept_Density=function(Year){
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/AllDistances.RData"))
  MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  AllPermutatationMinimumDistances=AllDistances
  Density=T
  Hist=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances, breaks)
  DeptTable=matrix(c(Hist[[2]][2,],
                     Hist[[2]][1,]),
                   ncol=2, byrow=F)
  DeptTable=as.table(DeptTable)
  DeptTable
  #Tests
  Result=wilcox.test(DeptTable[,1], DeptTable[,2], paired=T)
  return(Result)
}

getChi2TestSameVSDiffDept_Density(2012)
getChi2TestSameVSDiffDept_Density(2013)
getChi2TestSameVSDiffDept_Density(2014)
getChi2TestSameVSDiffDept_Density(2015)

getChi2TestSameVSDiffDept_Counts(2012)
getChi2TestSameVSDiffDept_Counts(2013)
getChi2TestSameVSDiffDept_Counts(2014)
getChi2TestSameVSDiffDept_Counts(2015)

getChi2TestAllDept_Density(2012)
getChi2TestAllDept_Density(2013)
getChi2TestAllDept_Density(2014)
getChi2TestAllDept_Density(2015)


#########################
#### Prop trend test ####

getSameDept=function(Year){
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  load(paste0(writingDir,"500 Permutations/", as.character(Year), " Results","/AllDistances.RData"))
  MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
  AllPermutatationMinimumDistances=AllDistances
  Density=F
  Hist=getHistograms("", Day, MinimumDistances, AllPermutatationMinimumDistances, breaks)
  NumberObservedSameDept=Hist[[2]][2,1]
  return(NumberObservedSameDept)
}


ObservedSameDept=c(getSameDept(2012),
                       getSameDept(2013),
                       getSameDept(2014),
                       getSameDept(2015))
ObservedSameDept

Observed=c(97,176,307,496)

prop.trend.test(ObservedSameDept, Observed)
