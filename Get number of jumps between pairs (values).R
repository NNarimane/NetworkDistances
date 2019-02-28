#############################################
##### Get number of jumps between pairs #####
################## WEIGHTS ##################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

#############################
#### DATA AND PARAMETERS ####

source("NetworkDistances/DataParameters.R")

##################################
#### GET UNWEIGHTED DISTANCES ####

getUnweightedDistances=function(Year){
  
  # Get potential infectors
  load(file = paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
  
  # Distance matrix
  weights = NA
  algorithm = "automatic"
  Distances_Matrix_Unweighted=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))
  
  # Get all distances from day 1-30
  Unweighted_Distances=foreach(d=1:30) %do% {
    
    PotentialInfectors=PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[d]]
    Pairs_Episodes_Unweighted_Distances=foreach(i=1:length(PotentialInfectors)) %do% {
      if(is.null(PotentialInfectors[[i]])){
        None=NA
      }else{
        Distance=Distances_Matrix_Unweighted[PotentialInfectors[[i]]$Department, data[i,"Department"]]
      }
    }
    Pairs_Episodes_Unweighted_Distances
  }
  
  save(Unweighted_Distances, file = paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/Unweighted_Distances.RData"))
  
  return(Unweighted_Distances)
}
getMeansCIPerDay=function(UnweightedDistanceValues){foreach(i=1:30, .combine="rbind") %do% {
  Mean=mean(unlist(UnweightedDistanceValues[[i]]), na.rm=T)
  Upper_CI=Mean + 1.96*sd(unlist(UnweightedDistanceValues[[i]]), na.rm=T)/sqrt(length(unlist(UnweightedDistanceValues[[i]])))
  Lower_CI=Mean - 1.96*sd(unlist(UnweightedDistanceValues[[i]]), na.rm=T)/sqrt(length(unlist(UnweightedDistanceValues[[i]])))
  FinalResult=paste0(round(Mean, 2), " 95% CI [", round(Lower_CI,2), "-", round(Upper_CI,2), "]")
  return(FinalResult)
}} 

# UnweightedDistanceValues2012=getUnweightedDistances(2012)
# save(UnweightedDistanceValues2012, file=paste0(writingDir, "Feb 14 Results/", as.character(2012), " Results", "/UnweightedDistanceValues2012.RData"))
load(file=paste0(writingDir, "Feb 14 Results/", as.character(2012), " Results", "/UnweightedDistanceValues2012.RData"))
# ObservedUnweightedMeans2012=getMeansCIPerDay(UnweightedDistanceValues2012)
# write.table(ObservedUnweightedMeans2012, file=paste0(writingDir, "Feb 14 Results/", as.character(2012), " Results", "/ObservedUnweightedMeans2012.csv"), sep=",")

# UnweightedDistanceValues2013=getUnweightedDistances(2013)
# save(UnweightedDistanceValues2013, file=paste0(writingDir, "Feb 14 Results/", as.character(2013), " Results", "/UnweightedDistanceValues2013.RData"))
load(file=paste0(writingDir, "Feb 14 Results/", as.character(2013), " Results", "/UnweightedDistanceValues2013.RData"))
# ObservedUnweightedMeans2013=getMeansCIPerDay(UnweightedDistanceValues2013)
# write.table(ObservedUnweightedMeans2013, file=paste0(writingDir, "Feb 14 Results/", as.character(2013), " Results", "/ObservedUnweightedMeans2013.csv"), sep=",")

# UnweightedDistanceValues2014=getUnweightedDistances(2014)
# save(UnweightedDistanceValues2014, file=paste0(writingDir, "Feb 14 Results/", as.character(2014), " Results", "/UnweightedDistanceValues2014.RData"))
load(file=paste0(writingDir, "Feb 14 Results/", as.character(2014), " Results", "/UnweightedDistanceValues2014.RData"))
# ObservedUnweightedMeans2014=getMeansCIPerDay(UnweightedDistanceValues2014)
# write.table(ObservedUnweightedMeans2014, file=paste0(writingDir, "Feb 14 Results/", as.character(2014), " Results", "/ObservedUnweightedMeans2014.csv"), sep=",")

# UnweightedDistanceValues2015=getUnweightedDistances(2015)
# save(UnweightedDistanceValues2015, file=paste0(writingDir, "Feb 14 Results/", as.character(2015), " Results", "/UnweightedDistanceValues2015.RData"))
load(file=paste0(writingDir, "Feb 14 Results/", as.character(2015), " Results", "/UnweightedDistanceValues2015.RData"))
# ObservedUnweightedMeans2015=getMeansCIPerDay(UnweightedDistanceValues2015)
# write.table(ObservedUnweightedMeans2015, file=paste0(writingDir, "Feb 14 Results/", as.character(2015), " Results", "/ObservedUnweightedMeans2015.csv"), sep=",")

##################################
#### GET UNWEIGHTED DISTANCES ####
########## PERMUTATIONS ##########

# Get potential infectors
load(file = paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/AllInfectors.RData"))

# Distance matrix
weights = NA
algorithm = "automatic"
Distances_Matrix_Unweighted=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))

#Parallel
cat("Numer of cores to use\n")
cores=5

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

All_Unweighted_Distances=foreach(p=1:100, .packages=c('foreach')) %dopar% {
Infectors=AllInfectors[[p]]
# Get all distances from day 1-30
Unweighted_Distances=foreach(d=1:30) %do% {
      PotentialInfectors=Infectors[[d]]
      Pairs_Episodes_Unweighted_Distances=foreach(i=1:length(PotentialInfectors)) %do% {
        if(is.null(PotentialInfectors[[i]])){
          None=NA
        }else{
          Distance=Distances_Matrix_Unweighted[PotentialInfectors[[i]]$Department, data[i,"Department"]]
        }
      }
      Pairs_Episodes_Unweighted_Distances
    }
}
cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()


# 2012
# Permutations_UnweightedDistanceValues2012=All_Unweighted_Distances
# save(Permutations_UnweightedDistanceValues2012, file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Permutations_UnweightedDistanceValues2012.RData"))
load(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Permutations_UnweightedDistanceValues2012.RData"))
# All_Distances_2012=foreach(i=1:length(Permutations_UnweightedDistanceValues2012[[1]])) %do% unlist(lapply(Permutations_UnweightedDistanceValues2012, `[[`, i)) 
# PermutationsUnweightedMeans2012=getMeansCIPerDay(All_Distances_2012)
# write.table(PermutationsUnweightedMeans2012, file=paste0(writingDir, "Feb 14 Results/", as.character(2012), " Results", "/PermutationsUnweightedMeans2012.csv"), sep=",")

# 2013
# Permutations_UnweightedDistanceValues2013=All_Unweighted_Distances
# save(Permutations_UnweightedDistanceValues2013, file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/Permutations_UnweightedDistanceValues2013.RData"))
load(file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/Permutations_UnweightedDistanceValues2013.RData"))
# All_Distances_2013=foreach(i=1:length(Permutations_UnweightedDistanceValues2013[[1]])) %do% unlist(lapply(Permutations_UnweightedDistanceValues2013, `[[`, i)) 
# PermutationsUnweightedMeans2013=getMeansCIPerDay(All_Distances_2013)
# write.table(PermutationsUnweightedMeans2013, file=paste0(writingDir, "Feb 14 Results/", as.character(2013), " Results", "/PermutationsUnweightedMeans2013.csv"), sep=",")

# 2014
# Permutations_UnweightedDistanceValues2014=All_Unweighted_Distances
# save(Permutations_UnweightedDistanceValues2014, file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/Permutations_UnweightedDistanceValues2014.RData"))
load(file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/Permutations_UnweightedDistanceValues2014.RData"))
# All_Distances_2014=foreach(i=1:length(Permutations_UnweightedDistanceValues2014[[1]])) %do% unlist(lapply(Permutations_UnweightedDistanceValues2014, `[[`, i)) 
# PermutationsUnweightedMeans2014=getMeansCIPerDay(All_Distances_2014)
# write.table(PermutationsUnweightedMeans2014, file=paste0(writingDir, "Feb 14 Results/", as.character(2014), " Results", "/PermutationsUnweightedMeans2014.csv"), sep=",")

# 2015
# Permutations_UnweightedDistanceValues2015=All_Unweighted_Distances
# save(Permutations_UnweightedDistanceValues2015, file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/Permutations_UnweightedDistanceValues2015.RData"))
load(file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/Permutations_UnweightedDistanceValues2015.RData"))
# All_Distances_2015=foreach(i=1:length(Permutations_UnweightedDistanceValues2015[[1]])) %do% unlist(lapply(Permutations_UnweightedDistanceValues2015, `[[`, i)) 
# PermutationsUnweightedMeans2015=getMeansCIPerDay(All_Distances_2015)
# write.table(PermutationsUnweightedMeans2015, file=paste0(writingDir, "Feb 14 Results/", as.character(2015), " Results", "/PermutationsUnweightedMeans2015.csv"), sep=",")




####################################
######### Wilcoxon Test ############
####################################

# Year
UnweightedDistanceValues=UnweightedDistanceValues2015
Permutations_UnweightedDistanceValues=Permutations_UnweightedDistanceValues2015

# Fix observed weights
Observed=foreach(d=1:30) %do% {
  foreach(n=1:nrow(data), .combine="c") %do% {
    value=UnweightedDistanceValues[[d]][[n]]
    if(length(value) == 0){
      value=NA
    }else{
      value=value
    }
  }
}
save(Observed, file=paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/Observed.RData"))

#Parallel
cat("Numer of cores to use\n")
cores=5

cat("Make clusters for parallel\n")
cl=makeCluster(cores)
registerDoSNOW(cl)
getDoParWorkers()

# Run
Permutations=foreach(p=1:100, .packages = "foreach") %dopar% {
  foreach(d=1:30) %do% {
    foreach(n=1:nrow(data), .combine="c") %do% {
      value=Permutations_UnweightedDistanceValues[[p]][[d]][[n]]
      if(length(value) == 0){
        value=NA
      }else{
        value=value
      }
    }
  }
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()

save(Permutations, file=paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/Permutations.RData"))


# Average
Nruns=100
Average_Permutations=getAverageMinDistances_CandidateTransmitters_Permutations(Permutations)

cat("Wilcoxon Rank Sum Test\n")
CombinedMinimumDistances_WilcoxonPairedRankTestPValues=foreach(i=1:length(Observed)) %do% getWilcoxonPairedRankTestPValues(i, Observed, Average_Permutations)

cat("Create Table of Statistically Significant and Non-Sig Results\n")
WilcoxonPairedRankTestPValues=unlist(CombinedMinimumDistances_WilcoxonPairedRankTestPValues)
Results=as.data.frame(WilcoxonPairedRankTestPValues)
Results$LowerBoundDay=1:Day
Results$UpperBound=7+1:Day
# Results$Day=1:30
Results$StatSigDiff=Results$WilcoxonPairedRankTestPValues < 0.05

cat("Save Results\n")
write.table(Results, file=paste0(writingDir,folder,"/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations (UNWEIGHTED).csv"), sep=",")


#####################
#### Prop Tables ####

PropTable2012=foreach(i=1:30, .combine="rbind") %do% prop.table(table(unlist(UnweightedDistanceValues2012[[i]])))
PropTable2013=foreach(i=1:30, .combine="rbind") %do% prop.table(table(unlist(UnweightedDistanceValues2013[[i]])))
PropTable2014=foreach(i=1:30, .combine="rbind") %do% prop.table(table(unlist(UnweightedDistanceValues2014[[i]])))
PropTable2014[which(rowSums(PropTable2014) > 1),4] = NA
PropTable2015=foreach(i=1:30, .combine="rbind") %do% prop.table(table(unlist(UnweightedDistanceValues2015[[i]])))

####################
#### Histograms ####

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

  MinimumDistancesSubset=MinimumDistances[MinimumDistances <= Cutoff]
  PermutationsMinimumDistancesSubset=unlist(PermutationsMinimumDistances)
  PermutationsMinimumDistancesSubset=PermutationsMinimumDistancesSubset[PermutationsMinimumDistancesSubset <= Cutoff]
  
  l <- list(PermutationsMinimumDistancesSubset, MinimumDistancesSubset)
  breaks=as.numeric(seq(from=-1, to=Cutoff, by=1))
  levs = as.character(breaks + 1)
  levs=levs[1:length(breaks)-1]
  
  if(Density){
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=F, xlab="NSP Distances", 
                       ylab="Density", main=main, cex.axis=1, 
                       ylim=ylim, cex.main=5, cex.lab=3,
                       col=c("aquamarine4", "aquamarine3"))
  }else{
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=T, xlab="NSP Distances", 
                       ylab="Density", main=main, cex.axis=1, 
                       ylim=ylim, cex.main=5, cex.lab=3,
                       col=c("aquamarine4", "aquamarine3"))
  }
  
  
  return(FinalHist)
}

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
layout.show(n=5)
par(mar=c(4,4,4,4))
ylim=c(0,1)
Cutoff=3

# 2012
load(paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Permutations.RData"))
Density=T
Hist2012=getHistograms("", Day, Observed, Permutations, breaks)
title("2012", adj=0, cex.main=1.8, font.main = 1)

# 2013
load(paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/Permutations.RData"))
Density=T
Hist2013=getHistograms("", Day, Observed, Permutations, breaks)
title("2013", adj=0, cex.main=1.8, font.main = 1)

# 2014
load(paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/Permutations.RData"))
Density=T
Hist2014=getHistograms("", Day, Observed, Permutations, breaks)
title("2014", adj=0, cex.main=1.8, font.main = 1)

# 2015
load(paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/Permutations.RData"))
Density=T
Hist2015=getHistograms("", Day, Observed, Permutations, breaks)
title("2015", adj=0, cex.main=1.8, font.main = 1)


par(mar=c(1,1,1,1))
plot.new()
legend(x="center",legend=c("Permutations", "Observed"),
       horiz = TRUE, fill=c("aquamarine4", "aquamarine3"))


#################
##### Plots #####

getYearlyPlots=function(MinimumDistances, AllPermutatationMinimumDistances, title){
  if(Transformed){
    #Observed Data
    MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
    MinimumDistances_Means=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
    MinimumDistances_MeansByNWeeks=lapply(MinimumDistances_Means, function(x) {1/(10^x)})
    
    #Permutations
    RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
    RandomSimulations_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 
    RandomSimulationsByWeeks_Means=lapply(RandomSimulations_Means, function(x) {unlist(lapply(x, function(y) {1/(10^x)}))})
  }else{
    #Observed Data
    MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
    MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
    
    #Permutations
    RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
    RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 
  }
  
  #ggplot
  df1=as.data.frame(t(as.data.frame(RandomSimulationsByWeeks_Means)))
  rownames(df1)=c(1:100)
  df2=cbind(paste0("Run", 1:100), df1)
  colnames(df2)=c("Run", 1:30)
  mdf <- melt(df2, id.vars="Run", value.name="AvgMinDistance", variable.name="Window")
  
  ObservedDistances=as.data.frame(cbind(rep("Observed", 30), 1:30, (unlist(MinimumDistances_MeansByNWeeks))))
  colnames(ObservedDistances)=colnames(mdf)
  ObservedDistances$AvgMinDistance=as.numeric(as.character(ObservedDistances$AvgMinDistance))
  
  FinalPlot=ggplot(data=mdf, aes(x=Window, y=AvgMinDistance, group = Run)) +
    geom_line(aes(colour = "Permutations"), alpha=0.5) +
    stat_summary(aes(y = AvgMinDistance, group=1, fill="95% of Permutations"),
                 fun.ymin = function(z) { quantile(z,0.025) },
                 fun.ymax = function(z) { quantile(z,0.975) },
                 fun.y = median,
                 geom = "ribbon", group=1,
                 alpha=0.2) +
    scale_fill_manual(name="", values="aquamarine3") +
    stat_summary(aes(y = AvgMinDistance, group=1, colour="Permutations Mean"), fun.y=mean, geom="line",group=1) +
    geom_line(data=ObservedDistances, aes(x=Window, y=AvgMinDistance, colour="Observed")) +
    scale_colour_manual(name="", values=c("brown3", "grey", "aquamarine4")) +
    labs(title = title,
         x = expression('Window'[n]),
         y = "Mean NSP Distances") +
    ylim(low=low, high=high) +
    theme_bw() +
    theme(legend.position="bottom") +
    guides(fill=guide_legend(order=0), color=guide_legend(order=1))
  
  
  return(FinalPlot)
}

Transformed=F

low=0.6
high=1.05


#2012
load(paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Permutations.RData"))
Plot2012=getYearlyPlots(Observed, Permutations, "2012")
Plot2012

#2013
load(paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/Permutations.RData"))
Plot2013=getYearlyPlots(Observed, Permutations, "2013")
Plot2013

#2014
load(paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/Permutations.RData"))
Plot2014=getYearlyPlots(Observed, Permutations, "2014")
Plot2014

#2015
load(paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/Permutations.RData"))
Plot2015=getYearlyPlots(Observed, Permutations, "2015")
Plot2015



grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(Plot2012, Plot2013, Plot2014, Plot2015)


#########################
#### Get Mean Tables ####
#########################

load(paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/Observed.RData"))
load(paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/Permutations.RData"))
Results=read.csv(file=paste0(writingDir,folder,"/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations (UNWEIGHTED).csv"), sep=",")

cat("Get Mean Table\n")
MeanMinimumDistancesTable=getMeanMinimumDistances(Observed, Permutations)

cat("Get Range Table\n")
RangeTable=getRange(Day, Observed, Permutations)

cat("Get Proportion Tables\n")
ProportionTables=get5thQuantiles(Day, Observed, Permutations)

cat("Merge Tables\n")
FinalTable=cbind(Results, MeanMinimumDistancesTable, RangeTable, ProportionTables[2,])

cat("Save\n")
write.table(FinalTable, file = paste0(writingDir,folder,"/FinalTable Unweighted.csv"), sep=",")


####################################
#### Get 2012-2015 Final Tables ####
####################################

Table2012=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/FinalTable Unweighted.csv"))
Table2013=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2013), " Results","/FinalTable Unweighted.csv"))
Table2014=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2014), " Results","/FinalTable Unweighted.csv"))
Table2015=read.csv(file=paste0(writingDir,"Feb 14 Results/", as.character(2015), " Results","/FinalTable Unweighted.csv"))

CombinedFinalTable=cbind(Table2012[,c(2,3,1,5,6)], Table2013[,c(1,5,6)], Table2014[,c(1,5,6)], Table2015[,c(1,5,6)])
write.table(CombinedFinalTable, file=paste0(writingDir, "Feb 14 Results/CombinedFinalTable Unweighted.csv"), sep=",")

