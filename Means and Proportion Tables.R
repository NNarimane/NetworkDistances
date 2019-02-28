#############################################################################################
#### Using Distribution of N simulations: Compare where potential infector values fall ######
################ Calculate proportions, and values of shortest path #########################
#############################################################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

#############################
#### DATA AND PARAMETERS ####

source("NetworkDistances/DataParameters.R")

####################################
#### GET MIN SHORTEST DISTANCES ####

cat("Get Observed Min Distances\n")
load(paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat("Get Permutations Min Distances\n")
# load(paste0(writingDir,folder,"/AllDistances500.RData"))
# AllDistances=AllDistances500
load(paste0(writingDir,folder,"/AllDistances.RData"))
AllDistances=AllDistances[1:500]

#########################
#### Get Mean Tables ####
#########################

cat("Get Mean Table\n")
# MeanMinimumDistancesTable=getMeanMinimumDistances(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllDistances)
MeanMinimumDistancesTable=getMeanMinimumDistancesWithCI_corrected(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllDistances)

cat("Get Range Table\n")
RangeTable=getRange(Day, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllDistances)

cat("Get Proportion Tables\n")
# ProportionTables=get5thQuantiles(Day, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, AllDistances)

Results=read.csv( file=paste0(writingDir,folder,"/Wilcoxon Rank Sum Test Results for Day 1 to ", Day," for Sliding Week ", Nruns," Permutations.csv"), sep=",")
cat("Merge Tables\n")
FinalTable=cbind(Results, MeanMinimumDistancesTable, RangeTable
                 # , ProportionTables[2,]
                 )

cat("Save\n")
write.table(FinalTable, file = paste0(writingDir,folder,"/FinalTable.csv"), sep=",")


####################################
#### Get 2012-2015 Final Tables ####
####################################

Table2012=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/FinalTable.csv"))
Table2013=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/FinalTable.csv"))
Table2014=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/FinalTable.csv"))
Table2015=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/FinalTable.csv"))

CombinedFinalTable=cbind(Table2012[,c(2,3,1,5,8)], Table2013[,c(1,5,8)], Table2014[,c(1,5,8)], Table2015[,c(1,5,8)])
write.table(CombinedFinalTable, file=paste0(writingDir, "500 Permutations/CombinedFinalTable.csv"), sep=",")

###################################
#### Get 2012-2015 Mean Tables ####
###################################


runAllPropTables=T
if(runAllPropTables){
  
  Table2012=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2012), " Results","/Proportion Table of All Windows.csv"))
  Table2013=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2013), " Results","/Proportion Table of All Windows.csv"))
  Table2014=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2014), " Results","/Proportion Table of All Windows.csv"))
  Table2015=read.csv(file=paste0(writingDir,"1000 Permutations/", as.character(2015), " Results","/Proportion Table of All Windows.csv"))
  
  AllPropTables=data.frame(Day=1:30, Year2012=Table2012[,4], Year2013=Table2013[,4], Year2014=Table2014[,4], Year2015=Table2015[,4])
  # colnames(AllPropTables)=c("Day","2012","2013","2014","2015")
  
  write.csv(AllPropTables, file=paste0(writingDir,"1000 Permutations/AllPropTables.csv"))
}else{
  AllPropTables=read.csv(file=paste0(writingDir,"1000 Permutations/AllPropTables.csv"))
}


##### Plots #####

ggplot(AllPropTables, aes(Day)) + 
  geom_line(aes(y = Year2012, colour = "2012"), size = 1.2) + 
  geom_line(aes(y = Year2013, colour = "2013"), size = 1.2) +
  geom_line(aes(y = Year2014, colour = "2014"), size = 1.2) +
  geom_line(aes(y = Year2015, colour = "2015"), size = 1.2) +
  geom_vline(aes(xintercept=21, colour = "Baseline"), alpha=0.5, size=1.2) +
  scale_y_continuous(labels = scales::percent) +
  ylab("% Non-imported episodes with potential infector") +
  xlab("1st Day of 1-Week Sliding Window") +
  scale_colour_manual(name="", values=c("aquamarine4", "aquamarine3", "steelblue3", "steelblue4", "red")) +
  guides(color=guide_legend(order=1)) +
  theme_bw()

