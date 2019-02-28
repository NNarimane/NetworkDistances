###################################################
#### Plots of Observed Data and Permutations ######
###################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

########################
#### PLOTS FUNCTION ####
########################

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
  rownames(df1)=c(1:length(AllPermutatationMinimumDistances))
  df2=cbind(paste0("Run", 1:length(AllPermutatationMinimumDistances)), df1)
  colnames(df2)=c("Run", 1:30)
  mdf <- melt(df2, id.vars="Run", value.name="AvgMinDistance", variable.name="Window")
  
  ObservedDistances=as.data.frame(cbind(rep("Observed", 30), 1:30, (unlist(MinimumDistances_MeansByNWeeks))))
  colnames(ObservedDistances)=colnames(mdf)
  ObservedDistances$AvgMinDistance=as.numeric(as.character(ObservedDistances$AvgMinDistance))
  
  FinalPlot=ggplot(data=mdf, aes(x=Window, y=AvgMinDistance, group = Run)) +
      geom_line(aes(colour = "Permutations"), Permutations=0.5) +
      stat_summary(aes(y = AvgMinDistance, group=1, fill="95% of Permutations"),
                   fun.ymin = function(z) { quantile(z,0.025) },
                   fun.ymax = function(z) { quantile(z,0.975) },
                   fun.y = median,
                   geom = "ribbon", group=1,
                   Permutations=0.2) +
      scale_fill_manual(name="", values="aquamarine3") +
      scale_x_discrete(labels=c("1","","","","5",
                                "","","","","10",
                                "","","","","15",
                                "","","","","20",
                                "","","","","25",
                                "","","","","30")) +
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

####################
#### PARAMETERS ####
####################

Permutations=F

if(Permutations){
  low=0.2
high=0.45
}else{
  low=4
  high=8.7
}

Transformed=F

###############
#### PLOTS ####
###############

#2012
load(paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/AllDistances.RData"))
# load(paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances
# AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=T
Plot2012=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2012")
Plot2012

#2013
load(paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/AllDistances.RData"))
# load(paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances
# AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=T
Plot2013=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2013")
Plot2013

#2014
load(paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/AllDistances.RData"))
# load(paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances
# AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=T
Plot2014=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2014")
Plot2014

#2015
load(paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/AllDistances.RData"))
# load(paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllDistances
# AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=T
Plot2015=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2015")
Plot2015

#All Plots with Legend
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


dev.copy(png,"Figure 3.png", height=6, width=10, units="in", res=300)
dev.copy(jpeg,"Figure 3.jpg", height=8, width=12, units="in", res=300)
dev.copy(pdf,"Figure 3.pdf", height=8, width=12)

grid_arrange_shared_legend(Plot2012, Plot2013, Plot2014, Plot2015)


dev.off()
