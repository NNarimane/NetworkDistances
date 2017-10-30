###################################################
#### Plots of Observed Data and Permutations ######
###################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###############
#### PLOTS ####
###############

getYearlyPlots=function(MinimumDistances, AllPermutatationMinimumDistances, title){
  #Observed Data
  MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
  MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
  
  #Permutations
  RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByWeeks_Means=lapply(AllPermutatationMinimumDistances, function(x) {unlist(lapply(x, function(y) {mean(unlist(y), na.rm=T)}))}) 
  
  #Plot
  # plot(unlist(MinimumDistances_MeansByNWeeks), type='l', col="red", ylim=ylim, 
  #      ylab="Avg. Shortest Dist. All Ep.", 
  #      xlab="1st Day of 1-Week Sliding Window",
  #      main="2012 CPE Episodes (n=240) & 50 Permutations")
  # for(i in 1:length(RandomSimulationsByWeeks)){
  #   lines(unlist(RandomSimulationsByWeeks_Means[[i]]), col=rgb(0, 0, 1, 0.4))
  # }
  # grid(nx = 15, ny = 5, col = "gray")
  
  #ggplot
  df1=as.data.frame(t(as.data.frame(RandomSimulationsByWeeks_Means)))
  rownames(df1)=c(1:50)
  df2=cbind(paste0("Run", 1:50), df1)
  colnames(df2)=c("Run", 1:30)
  mdf <- melt(df2, id.vars="Run", value.name="AvgMinDistance", variable.name="Window")
  
  # ggplot(data=mdf, aes(x=Window, y=AvgMinDistance, group = Run)) +
  #   geom_line(colour = "gray") +
  #   stat_summary(aes(y = AvgMinDistance, group=1), 
  #                fun.data = mean_cl_normal, 
  #                geom = "ribbon", group=1, 
  #                fun.args = list(conf.int = 0.95), 
  #                fill="lightblue", 
  #                alpha=0.8) +
  #   stat_summary(aes(y = AvgMinDistance, group=1), fun.y=mean, colour="black", geom="line",group=1)

  # df3 <- summarySE(mdf, measurevar="AvgMinDistance", groupvars="Window", conf.interval = 0.95)

  # ggplot(data=df3, aes(x=Window, y=AvgMinDistance)) +
  #   geom_errorbar(aes(ymin=AvgMinDistance-ci, ymax=AvgMinDistance+ci), width=.1) +
  #   geom_path()
  
  ObservedDistances=as.data.frame(cbind(rep("Observed", 30), 1:30, (unlist(MinimumDistances_MeansByNWeeks))))
  colnames(ObservedDistances)=colnames(mdf)
  ObservedDistances$AvgMinDistance=as.numeric(as.character(ObservedDistances$AvgMinDistance))
  
  # ##Confidence Interval
  # ##Lower Bound##
  # lbci<-colMeans(df1) - 1.96*apply(df1, 2, sd)/sqrt(nrow(df1))
  # ##Upper Bound##
  # ubci<-colMeans(df1) + 1.96*apply(df1, 2, sd)/sqrt(nrow(df1))
  # #CI
  # ci=as.data.frame(cbind(lbci, ubci))
  # #Window
  # ci$Window=as.factor(1:30)
  # #AvgDistance
  # ci$AvgMinDistance=colMeans(df1)
  
  FinalPlot=ggplot(data=mdf, aes(x=Window, y=AvgMinDistance, group = Run)) +
      geom_line(aes(colour = "Permutations"), alpha=0.5) +
      # stat_summary(aes(y = AvgMinDistance, group=1, fill="Permutations 95% CI"),
      #              fun.data = mean_cl_normal,
      #              geom = "ribbon", group=1,
      #              fun.args = list(conf.int = 0.95),
      #              alpha=0.3) +
      stat_summary(aes(y = AvgMinDistance, group=1, fill="95% of Permutations"),
                   fun.ymin = function(z) { quantile(z,0.025) },
                   fun.ymax = function(z) { quantile(z,0.975) },
                   fun.y = median,
                   geom = "ribbon", group=1,
                   alpha=0.3) +
    
      scale_fill_manual(name="", values="dodgerblue1") +
      stat_summary(aes(y = AvgMinDistance, group=1, colour="Permutations Mean"), fun.y=mean, geom="line",group=1) +
      geom_line(data=ObservedDistances, aes(x=Window, y=AvgMinDistance, colour="Observed")) +
      scale_colour_manual(name="", values=c("red", "grey", "black")) +
      labs(title = title,
           x = "1st Day of 1-Week Sliding Window",
           y = "Avg. Min. Shortest Distance") +
      ylim(low=low, high=high) +
      theme_bw() +
      theme(legend.position="bottom") +
      guides(fill=guide_legend(order=0), color=guide_legend(order=1))

  
  return(FinalPlot)
}

#layout
# par(mfrow=c(2,2))
# par(mfrow=c(1,1))
# ylim=c(2,5)
low=2.2
high=4.5

#2012
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2012 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2012 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=T
Plot2012=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2012 CPE Episodes (n=240)")
Plot2012

#2013
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2013 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2013 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=T
Plot2013=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2013 CPE Episodes (n=401)")
Plot2013

#2014
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2014 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=T
Plot2014=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2014 CPE Episodes (n=668)")
Plot2014

#2015
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=F
Plot2015=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2015 CPE Episodes (n=948)")
Plot2015

#2015b
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week) 30 to 60 Days/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week) 30 to 60 Days/AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
MinimumDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding
AllPermutatationMinimumDistances=AllRuns_MinimumDistances_CandidateTransmitters_Permutations_byDay_byMechanism_SharedDept_Reshuffled_Sliding

noLegend=F
Plot2015b=getYearlyPlots(MinimumDistances, AllPermutatationMinimumDistances, "2015 CPE Episodes (n=948)")
Plot2015b

#All Plots
multiplot(Plot2012, Plot2014, Plot2013, Plot2015, cols=2)

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

grid_arrange_shared_legend(Plot2012, Plot2014, Plot2013, Plot2015)
