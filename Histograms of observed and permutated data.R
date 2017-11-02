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

getHistograms=function(main, Day, MinimumDistances, AllPermutatationMinimumDistances){
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
  
  MinimumDistancesSubset=MinimumDistances[MinimumDistances <= 10]
  PermutationsMinimumDistancesSubset=unlist(PermutationsMinimumDistances)
  PermutationsMinimumDistancesSubset=PermutationsMinimumDistancesSubset[PermutationsMinimumDistancesSubset <= 10]
  
  # l <- list(unlist(PermutationsMinimumDistances), MinimumDistances)
  l <- list(PermutationsMinimumDistancesSubset, MinimumDistancesSubset)
  
  # breaks <- pretty(unlist(l))
  breaks=as.numeric(c(-1, 0, 1, 2, 3,4,5,6,7,8,9,10))
  # levs = levels(cut(unlist(l), breaks=breaks))
  levs = as.character(breaks + 1)
  levs=levs[1:length(breaks)-1]
  
  if(Legend){
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=F, xlab="Network Distances", 
                       ylab="Density", main=main, cex.axis=1, 
                       ylim=c(0,0.5), cex.main=5, cex.lab=3,
                       legend.text=c("Permutations", "Observed"),
                       col=c("gray30", "lightgrey"))
  }else{
    FinalHist=multhist(l, breaks=breaks, names.arg = levs, 
                       freq=F, xlab="Network Distances", 
                       ylab="Density", main=main, cex.axis=1, 
                       ylim=c(0,0.5), cex.main=5, cex.lab=3,
                       col=c("gray30", "lightgrey"))
  }
  

  return(FinalHist)
}

#############
#### DAY ####
#############

Day=21

##############
#### HIST ####
##############

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


#2012
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2012 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2012 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2012=getHistograms("2012", Day, MinimumDistances, AllPermutatationMinimumDistances)
Hist2012

#2013
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2013 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2013 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2013=getHistograms("2013", Day, MinimumDistances, AllPermutatationMinimumDistances)
Hist2013

#2014
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2014=getHistograms("2014", Day, MinimumDistances, AllPermutatationMinimumDistances)
Hist2014

#2015
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

Legend=F
Hist2015=getHistograms("2015", Day, MinimumDistances, AllPermutatationMinimumDistances)
Hist2015

par(mar=c(1,1,1,1))
plot.new()
legend(x="center",legend=c("Permutations", "Observed"),
       horiz = TRUE, fill=c("gray30", "lightgrey"))

##################
#### COMBINED ####
##################

# multiplot(Hist2012, Hist2014, Hist2013, Hist2015, cols=2)

#All Plots with Legend
# grid_arrange_shared_legend <- function(...) {
#   plots <- list(...)
#   g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
#   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
#   lheight <- sum(legend$height)
#   grid.arrange(
#     do.call(arrangeGrob, lapply(plots, function(x)
#       x + theme(legend.position="none"))),
#     legend,
#     ncol = 1,
#     heights = unit.c(unit(1, "npc") - lheight, lheight))
# }

# grid_arrange_shared_legend(Plot2012, Plot2014, Plot2013, Plot2015)