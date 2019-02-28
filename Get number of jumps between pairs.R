#############################################
##### Get number of jumps between pairs #####
################ PROPORTIONS ################

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
  Unweighted_Distances_PropTables=foreach(d=1:30, .combine="rbind") %do% {
    
    PotentialInfectors=PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[d]]
    Pairs_Episodes_Unweighted_Distances=foreach(i=1:length(PotentialInfectors), .combine = 'rbind') %do% {
      if(is.null(PotentialInfectors[[i]])){
        None=NA
      }else{
        Distance=Distances_Matrix_Unweighted[PotentialInfectors[[i]]$Department, data[i,"Department"]]
      }
    }
    
    Table=prop.table(table(Pairs_Episodes_Unweighted_Distances))
    
  }
  
  write.table(Unweighted_Distances_PropTables, file = paste0(writingDir,"Feb 14 Results/", as.character(Year), " Results","/UnweightedDistanceTable.csv"), sep=",")
  
  return(Unweighted_Distances_PropTables)
}

UnweightedDistanceTable2012=getUnweightedDistances(2012)
save(UnweightedDistanceTable2012, file=paste0(writingDir, "Feb 14 Results/", as.character(2012), " Results", "/UnweightedDistanceTable2012.RData"))
# load(file=paste0(writingDir, "Feb 14 Results/", as.character(2012), " Results", "/UnweightedDistanceTable2012.RData"))

UnweightedDistanceTable2013=getUnweightedDistances(2013)
save(UnweightedDistanceTable2013, file=paste0(writingDir, "Feb 14 Results/", as.character(2013), " Results", "/UnweightedDistanceTable2013.RData"))
# load(file=paste0(writingDir, "Feb 14 Results/", as.character(2013), " Results", "/UnweightedDistanceTable2013.RData"))

UnweightedDistanceTable2014=getUnweightedDistances(2014)
save(UnweightedDistanceTable2014, file=paste0(writingDir, "Feb 14 Results/", as.character(2014), " Results", "/UnweightedDistanceTable2014.RData"))
# load(file=paste0(writingDir, "Feb 14 Results/", as.character(2014), " Results", "/UnweightedDistanceTable2014.RData"))

UnweightedDistanceTable2015=getUnweightedDistances(2015)
save(UnweightedDistanceTable2015, file=paste0(writingDir, "Feb 14 Results/", as.character(2015), " Results", "/UnweightedDistanceTable2015.RData"))
# load(file=paste0(writingDir, "Feb 14 Results/", as.character(2015), " Results", "/UnweightedDistanceTable2015.RData"))

##################################
#### GET UNWEIGHTED DISTANCES ####
########## PERMUTATIONS ##########

Year = 2013

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

All_Unweighted_Distances_PropTables=foreach(p=1:100, .packages=c('foreach')) %dopar% {
  Infectors=AllInfectors[[p]]
  # Get all distances from day 1-30
  Unweighted_Distances_PropTables=foreach(d=1:30, .combine="rbind") %do% {
    Infectors_PerDay=Infectors[[d]]
    Pairs_Episodes_Unweighted_Distances=foreach(i=1:length(Infectors_PerDay), .combine = 'rbind') %do% {
      if(is.null(Infectors_PerDay[[i]])){
        None=NA
      }else{
        Distance=Distances_Matrix_Unweighted[Infectors_PerDay[[i]]$Department, data[i,"Department"]]
      }
    }
    Table=prop.table(table(Pairs_Episodes_Unweighted_Distances))
  }
}

cat("Stop parallel\n")
stopCluster(cl)
print("Cluster stopped")
registerDoSEQ()


# 2012
Permutations_UnweightedDistanceTable2012=All_Unweighted_Distances_PropTables
save(Permutations_UnweightedDistanceTable2012, file=paste0(writingDir, folder, "/Permutations_UnweightedDistanceTable2012.RData"))
# load(file=paste0(writingDir,"Feb 14 Results/", as.character(2012), " Results","/Permutations_UnweightedDistanceTable2012.RData"))

All_Zero_2012=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2012[[i]][,1]}
All_One_2012=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2012[[i]][,2]}
All_Two_2012=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2012[[i]][,3]}
# Permutations_UnweightedDistanceTable2012_ZeroOneTwoThree=Permutations_UnweightedDistanceTable2012[which(lapply(Permutations_UnweightedDistanceTable2012, ncol) == 4)]
# All_Three_2012_OnlyThrees=foreach(i=1:length(Permutations_UnweightedDistanceTable2012_ZeroOneTwoThree), .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2012_ZeroOneTwoThree[[i]][,4]}
# All_Three_2012=cbind(All_Three_2012_OnlyThrees, matrix(0, 30, 100-ncol(All_Three_2012)))

All_Zero_Means_2012=rowMeans(All_Zero_2012)
All_One_Means_2012=rowMeans(All_One_2012)
All_Two_Means_2012=rowMeans(All_Two_2012)
# All_Three_Means_2012=rowMeans(All_Three_2012)

# 2013
Permutations_UnweightedDistanceTable2013=All_Unweighted_Distances_PropTables
save(Permutations_UnweightedDistanceTable2013, file=paste0(writingDir, folder, "/Permutations_UnweightedDistanceTable2013.RData"))
# load(file=paste0(writingDir, "Feb 14 Results/", as.character(2012), " Results", "/Permutations_UnweightedDistanceTable2013.RData"))

All_Zero_2013=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2013[[i]][,1]}
All_One_2013=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2013[[i]][,2]}
All_Two_2013=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2013[[i]][,3]}
Permutations_UnweightedDistanceTable2013_ZeroOneTwoThree=Permutations_UnweightedDistanceTable2013[which(lapply(Permutations_UnweightedDistanceTable2013, ncol) == 4)]
All_Three_2013=foreach(i=1:length(Permutations_UnweightedDistanceTable2013_ZeroOneTwoThree), .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2013_ZeroOneTwoThree[[i]][,4]}

All_Zero_Means_2013=rowMeans(All_Zero_2013)
All_One_Means_2013=rowMeans(All_One_2013)
All_Two_Means_2013=rowMeans(All_Two_2013)
All_Three_Means_2013=rowMeans(All_Three_2013)

# 2014
# Permutations_UnweightedDistanceTable2014=getUnweightedDistances_Permutations(2014)
# save(Permutations_UnweightedDistanceTable2014, file=paste0(writingDir, folder, "/Permutations_UnweightedDistanceTable2014.RData"))
load(file=paste0(writingDir, folder, "/Permutations_UnweightedDistanceTable2014.RData"))

All_Zero_2014=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2014[[i]][,1]}
All_One_2014=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2014[[i]][,2]}
All_Two_2014=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2014[[i]][,3]}
Permutations_UnweightedDistanceTable2014_ZeroOneTwoThree=Permutations_UnweightedDistanceTable2014[which(lapply(Permutations_UnweightedDistanceTable2014, ncol) == 4)]
All_Three_2014=foreach(i=1:length(Permutations_UnweightedDistanceTable2014_ZeroOneTwoThree), .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2014_ZeroOneTwoThree[[i]][,4]}

All_Zero_Means_2014=rowMeans(All_Zero_2014)
All_One_Means_2014=rowMeans(All_One_2014)
All_Two_Means_2014=rowMeans(All_Two_2014)
All_Three_Means_2014=rowMeans(All_Three_2014)

# 2015
# Permutations_UnweightedDistanceTable2015=getUnweightedDistances_Permutations(2015)
# save(Permutations_UnweightedDistanceTable2015, file=paste0(writingDir, folder, "/Permutations_UnweightedDistanceTable2015.RData"))
load(file=paste0(writingDir, folder, "/Permutations_UnweightedDistanceTable2015.RData"))

All_Zero_2015=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2015[[i]][,1]}
All_One_2015=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2015[[i]][,2]}
All_Two_2015=foreach(i=1:100, .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2015[[i]][,3]}
Permutations_UnweightedDistanceTable2015_ZeroOneTwoThree=Permutations_UnweightedDistanceTable2015[which(lapply(Permutations_UnweightedDistanceTable2015, ncol) == 4)]
All_Three_2015=foreach(i=1:length(Permutations_UnweightedDistanceTable2015_ZeroOneTwoThree), .combine="cbind") %do% {Table=Permutations_UnweightedDistanceTable2015_ZeroOneTwoThree[[i]][,4]}

All_Zero_Means_2015=rowMeans(All_Zero_2015)
All_One_Means_2015=rowMeans(All_One_2015)
All_Two_Means_2015=rowMeans(All_Two_2015)
All_Three_Means_2015=rowMeans(All_Three_2015)


#######################################
#### PLOTS OF UNWEIGHTED DISTANCES ####

getUnweightedPlots2012=function(UnweightedDistanceTable, Year){
  AllJumps=data.frame(Day=1:30, 
                      Observed_SameDept=UnweightedDistanceTable[,1], 
                      Observed_OneJump=UnweightedDistanceTable[,2],
                      Observed_TwoJumps=UnweightedDistanceTable[,3],
                      Permutations_SameDept=All_Zero_Means_2012, 
                      Permutations_OneJump=All_One_Means_2012,
                      Permutations_TwoJumps=All_Two_Means_2012)
  
  AllJumps_melted=melt(AllJumps, id.vars="Day")
  
  Plot=ggplot(data=AllJumps_melted, aes(x=Day, y=value, group=variable, colour=variable, linetype=variable)) + geom_line(size=1.2) +
    geom_vline(aes(xintercept=21), alpha=0.5, size=1, colour="orange") +
    labs(title = as.character(Year),
         x = expression('Window'[n]),
         y = "Percent (%)") +
    scale_linetype_manual(values=c(1,1,1,2,2,2), labels=c("Observed = 0",
                                                          "Observed = 1",
                                                          "Observed = 2",
                                                          "Permutations = 0",
                                                          "Permutations = 1",
                                                          "Permutations = 2"),
                          guide = guide_legend(title = "Jumps between episodes")) +
    scale_colour_manual(values=c("aquamarine3", "aquamarine4",
                                 "steelblue3", "aquamarine3",
                                 "aquamarine4", "steelblue3"), labels=c("Observed = 0",
                                                                        "Observed = 1",
                                                                        "Observed = 2",
                                                                        "Permutations = 0",
                                                                        "Permutations = 1",
                                                                        "Permutations = 2"),
                        guide = guide_legend(title = "Jumps between episodes")) +
    scale_x_continuous(breaks=seq(from=1, to=30, by=1)) +
    theme_bw()
  return(Plot)
}

Plot2012=getUnweightedPlots2012(UnweightedDistanceTable2012, 2012)
Plot2012

getUnweightedPlots2013=function(UnweightedDistanceTable, Year){
  AllJumps=data.frame(Day=1:30, 
                      Observed_SameDept=UnweightedDistanceTable[,1], 
                      Observed_OneJump=UnweightedDistanceTable[,2],
                      Observed_TwoJumps=UnweightedDistanceTable[,3],
                      Permutations_SameDept=All_Zero_Means_2013, 
                      Permutations_OneJump=All_One_Means_2013,
                      Permutations_TwoJumps=All_Two_Means_2013,
                      Permutations_ThreeJumps=All_Three_Means_2013)
  
  AllJumps_melted=melt(AllJumps, id.vars="Day")
  
  Plot=ggplot(data=AllJumps_melted, aes(x=Day, y=value, group=variable, colour=variable, linetype=variable)) + geom_line(size=1.2) +
    geom_vline(aes(xintercept=21), alpha=0.5, size=1, colour="orange") +
    labs(title = as.character(Year),
         x = expression('Window'[n]),
         y = "Percent (%)") +
    scale_linetype_manual(values=c(1,1,1,2,2,2,2), labels=c("Observed = 0",
                                                          "Observed = 1",
                                                          "Observed = 2",
                                                          "Permutations = 0",
                                                          "Permutations = 1",
                                                          "Permutations = 2",
                                                          "Permutations = 3"),
                          guide = guide_legend(title = "Jumps between episodes")) +
    scale_colour_manual(values=c("aquamarine3", "aquamarine4",
                                 "steelblue3", "aquamarine3",
                                 "aquamarine4", "steelblue3",
                                 "steelblue4"), labels=c("Observed = 0",
                                                                        "Observed = 1",
                                                                        "Observed = 2",
                                                                        "Permutations = 0",
                                                                        "Permutations = 1",
                                                                        "Permutations = 2",
                                                         "Permutations = 3"),
                        guide = guide_legend(title = "Jumps between episodes")) +
    scale_x_continuous(breaks=seq(from=1, to=30, by=1)) +
    theme_bw()
  return(Plot)
}
Plot2013=getUnweightedPlots2013(UnweightedDistanceTable2013, 2013)
Plot2013
