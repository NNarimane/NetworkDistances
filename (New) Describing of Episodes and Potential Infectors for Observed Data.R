################################################################
##### Describing Pairs of Episodes and Potential Infectors #####
################################################################

source(paste0(NETpath,"Network_CPE_CommonHeader.R"))

###################################
#### GET FUNCTIONS SOURCE CODE ####

source(paste0(NETpath,"Network_CPE_Functions.R"), local = FALSE, verbose = getOption("verbose"))

#############################
#### DATA AND PARAMETERS ####

source(paste0(NETpath,"Network_CPE_DataParameters.R"))


#########################
#### LOAD CANDIDATES ####

# cat(paste("Load", Year, "Candidates\n"))
# load(paste0(writingDir,folder,"/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat(paste("Load", Year, "Minimum Distances\n"))
load(paste0(writingDir,folder,"/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat(paste("Load", Year, "Potential Infector\n"))
load(file = paste0(writingDir,folder,"/PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
PotentialInfectors=PotentialInfectors_byDay_byMechanism_SharedDept_Reshuffled_Sliding
# 
# # Fix PI in 2012-2015 Episodes
# for(x in 1:length(PotentialInfectors)){
#   for(y in 1:length(PotentialInfectors[[x]])){
#     if(is.null(PotentialInfectors[[x]][[y]]) || isTRUE(nrow(PotentialInfectors[[x]][[y]]) == 0)){
#       PotentialInfectors[[x]][[y]]=NA
#     }else{
#       PotentialInfectors[[x]][[y]]$Episode=as.numeric(rownames(PotentialInfectors[[x]][[y]]))
#     }
#   }
# }
# save(PotentialInfectors, file=paste0(writingDir,folder,"/PotentialInfectorsFixed.RData"))
# load(file=paste0(writingDir,folder,"/PotentialInfectorsFixed.RData"))

##############################
#### GET PAIRS + Distance ####

AllPairsDistanceTables=foreach(i=1:length(PotentialInfectors)) %do% {
  PIDbyDay=PotentialInfectors[[i]]
  PIDbyDaybyEpisode=foreach(j=1:nrow(data), .combine="rbind") %do% {
    if(is.null(PIDbyDay[[j]]) || isTRUE(nrow(PIDbyDay[[j]]) == 0) || is.na(PIDbyDay[[j]])){
      None=c(j,NA)
    }else{
      ID=c(j,PIDbyDay[[j]]$Episode)
    }
  }
  PIDbyDaybyEpisode=unname(PIDbyDaybyEpisode)
  PIDbyDaybyEpisodeDistance=cbind(PIDbyDaybyEpisode, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[i]])
  colnames(PIDbyDaybyEpisodeDistance)=c("IncidentEpisode", "PotentialInfector", "NSPDistance")
  WithIncidentEpisodeInfo=cbind(PIDbyDaybyEpisodeDistance, data[,c("Imported","Department","TotalCases","Mechanism")])
  colnames(WithIncidentEpisodeInfo)=c("IncidentEpisode","PotentialInfector",
                                      "NSPDistance","EiImported","EiDepartment","EiCases","EiMechanism")
  WithPIinfo=merge(WithIncidentEpisodeInfo, data[,c(1,3,4,5,14)], by.x="PotentialInfector", by.y="Episode", all.x=T)
  colnames(WithPIinfo)=c("PotentialInfector","IncidentEpisode","NSPDistance","EiImported",
                         "EiDepartment","EiCases","EiMechanism","PiTotalCases",
                         "PiImported","PiMechanism","PiDepartment")
  WithPIinfo
}


###############################
#### Proportions Explained ####

run=T

if(run){
  ProportionsTable=foreach(i=1:length(AllPairsDistanceTables), .combine = "rbind") %do% {
  cat("Total episodes linked out of total episodes\n")
  TotalEpisodesLinkedtoPI=length((AllPairsDistanceTables[[i]][,"PotentialInfector"][complete.cases(AllPairsDistanceTables[[i]][,"PotentialInfector"])]))/nrow(data)
  TotalUniquePI=length(unique(AllPairsDistanceTables[[i]][,"PotentialInfector"][complete.cases(AllPairsDistanceTables[[i]][,"PotentialInfector"])]))/nrow(data)
  
  cat("Total imported episodes linked out of total imported episodes\n")
  data_Imported=data[which(data$Imported == "O"),]
  AllPairsDistanceTables_Imported=AllPairsDistanceTables[[i]][which(AllPairsDistanceTables[[i]]$PiImported == "O"),]
  TotalEpisodesLinkedImportedtoPI=nrow((AllPairsDistanceTables_Imported))/nrow(data_Imported)
  TotalEpisodesLinkedImportedtoUniquePI=length(unique(AllPairsDistanceTables_Imported$PotentialInfector))/nrow(data_Imported)
  
  cat("Total non-imported episodes linked out of total non-imported episodes\n")
  data_Nonimported=data[which(data$Imported == "N"),]
  TotalNonImportedEpisodesLinked=length((AllPairsDistanceTables[[i]][,"PotentialInfector"][complete.cases(AllPairsDistanceTables[[i]][,"PotentialInfector"])]))/nrow(data_Nonimported)
  
  cat("Cbind\n")
  Table=c(TotalEpisodesLinkedtoPI, TotalUniquePI, TotalEpisodesLinkedImportedtoPI, TotalEpisodesLinkedImportedtoUniquePI,
          TotalNonImportedEpisodesLinked)
}
ProportionsTable=as.data.frame(ProportionsTable)
colnames(ProportionsTable)=c("% Ei Linked to PI",
                             "% Potential Infectors",
                             "% Ei Linked to Imported PI",
                             "% Ei Linked to Non-Imported PI",
                             "% Ei Linked out of Total Possible")
write.csv(ProportionsTable, file=paste0(writingDir,folder,"/Proportion Table of All Windows.csv"))

################################
#### Select Baseline Window ####

Window=21

##############################
#### Description of Pairs ####

PairsDistanceTable=AllPairsDistanceTables[[Window]]
PairsDistanceTable=PairsDistanceTable[complete.cases(PairsDistanceTable$PotentialInfector),]
cat("Save or load\n")
write.csv(PairsDistanceTable, file=paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))


##########################
#### Summary of Pairs ####

cat("Average Length Between Pairs\n")
Mean=mean(PairsDistanceTable$NSPDistance)
cat("Range\n")
Min=min(PairsDistanceTable$NSPDistance)
NonZeroMin=min(PairsDistanceTable$NSPDistance[PairsDistanceTable$NSPDistance > 0])
Max=max(PairsDistanceTable$NSPDistance)
cat("CI\n")
Upper_CI=Mean + 1.96*sd(PairsDistanceTable$NSPDistance)/sqrt(length(PairsDistanceTable$NSPDistance))
Lower_CI=Mean - 1.96*sd(PairsDistanceTable$NSPDistance)/sqrt(length(PairsDistanceTable$NSPDistance))

# Permutations
cat("Average Length Between Pairs\n")
# load(file = paste0(writingDir,folder,"/AllDistances500.RData"))
# Permutations=AllDistances500
load(file = paste0(writingDir,folder,"/AllDistances.RData"))
Permutations=AllDistances[1:500]
Permutations=foreach(i=1:length(Permutations[[1]])) %do% lapply(Permutations, `[[`, i) #get first elements i of each list
Permutations=foreach(i=1:length(Permutations)) %do% CleaningFunction(Permutations[[i]])
Permutations=Permutations[[21]]
Permutations=unlist(Permutations)
Permutations=Permutations[!is.na(Permutations)]
MeanPermutation=mean(Permutations, na.rm = T)
Upper_CI_P=MeanPermutation + 1.96*sd(Permutations, na.rm = T)/sqrt(length(Permutations))
Lower_CI_P=MeanPermutation - 1.96*sd(Permutations, na.rm = T)/sqrt(length(Permutations))

Mean=mean(PairsDistanceTable$NSPDistance)
cat("Range\n")
Min=min(PairsDistanceTable$NSPDistance)
NonZeroMin=min(PairsDistanceTable$NSPDistance[PairsDistanceTable$NSPDistance > 0])
Max=max(PairsDistanceTable$NSPDistance)
cat("CI\n")
Upper_CI=Mean + 1.96*sd(PairsDistanceTable$NSPDistance)/sqrt(length(PairsDistanceTable$NSPDistance))
Lower_CI=Mean - 1.96*sd(PairsDistanceTable$NSPDistance)/sqrt(length(PairsDistanceTable$NSPDistance))

EpisodesExplained=t(ProportionsTable[Window,])
SummaryTable=as.data.frame(rbind(nrow(data), nrow(unique(data_Imported)), nrow(unique(data_Nonimported)), 
                                 length(PairsDistanceTable$NSPDistance), Mean, Lower_CI, Upper_CI, 
                                 MeanPermutation, Lower_CI_P, Upper_CI_P, 
                                 EpisodesExplained))

rownames(SummaryTable)=c("Total Episodes", "Imported Episodes", "Non-Imported Episodes", 
                         "Number of Pairs","Mean Distance","Lower_CI","Upper_CI"
                         ,"Permutations Mean Distance","Permutations Lower_CI",
                         "Permutations Upper_CI",
                         "% Ei Linked to PI",
                         "% Potential Infectors",
                         "% Ei Linked to Imported PI",
                         "% Ei Linked to Non-Imported PI",
                         "% Ei Linked out of Total Possible")
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

runAllSummaries=T
if(runAllSummaries){
  
  Window = 21
  
  Table2012=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/SummaryTable.csv"))
  Table2013=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/SummaryTable.csv"))
  Table2014=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/SummaryTable.csv"))
  Table2015=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/SummaryTable.csv"))

  # load(file=paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/AllDistances.RData"))
  # Permutations2012=AllDistances
  # Permutations2012=foreach(i=1:length(Permutations2012[[1]])) %do% lapply(Permutations2012, `[[`, i) #get first elements i of each list
  # Permutations2012=foreach(i=1:length(Permutations2012)) %do% CleaningFunction(Permutations2012[[i]])
  # Permutations2012=Permutations2012[[Window]]
  # Permutations2012=unlist(Permutations2012)
  # Permutations2012=Permutations2012[!is.na(Permutations2012)]
  # 
  # MeanPermutation2012=mean(Permutations2012, na.rm = T)
  # Upper_CI_P2012=MeanPermutation2012 + 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  # Lower_CI_P2012=MeanPermutation2012 - 1.96*sd(Permutations2012, na.rm = T)/sqrt(length(Permutations2012))
  # 
  # load(file=paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/AllDistances.RData"))
  # Permutations2013=AllDistances
  # Permutations2013=foreach(i=1:length(Permutations2013[[1]])) %do% lapply(Permutations2013, `[[`, i) #get first elements i of each list
  # Permutations2013=foreach(i=1:length(Permutations2013)) %do% CleaningFunction(Permutations2013[[i]])
  # Permutations2013=Permutations2013[[Window]]
  # Permutations2013=unlist(Permutations2013)
  # Permutations2013=Permutations2013[!is.na(Permutations2013)]
  # 
  # MeanPermutation2013=mean(Permutations2013, na.rm = T)
  # Upper_CI_P2013=MeanPermutation2013 + 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  # Lower_CI_P2013=MeanPermutation2013 - 1.96*sd(Permutations2013, na.rm = T)/sqrt(length(Permutations2013))
  # 
  # 
  # load(file=paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/AllDistances.RData"))
  # Permutations2014=AllDistances
  # Permutations2014=foreach(i=1:length(Permutations2014[[1]])) %do% lapply(Permutations2014, `[[`, i) #get first elements i of each list
  # Permutations2014=foreach(i=1:length(Permutations2014)) %do% CleaningFunction(Permutations2014[[i]])
  # Permutations2014=Permutations2014[[Window]]
  # Permutations2014=unlist(Permutations2014)
  # Permutations2014=Permutations2014[!is.na(Permutations2014)]
  # 
  # MeanPermutation2014=mean(Permutations2014, na.rm = T)
  # Upper_CI_P2014=MeanPermutation2014 + 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  # Lower_CI_P2014=MeanPermutation2014 - 1.96*sd(Permutations2014, na.rm = T)/sqrt(length(Permutations2014))
  # 
  # load(file=paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/AllDistances.RData"))
  # Permutations2015=AllDistances
  # Permutations2015=foreach(i=1:length(Permutations2015[[1]])) %do% lapply(Permutations2015, `[[`, i) #get first elements i of each list
  # Permutations2015=foreach(i=1:length(Permutations2015)) %do% CleaningFunction(Permutations2015[[i]])
  # Permutations2015=Permutations2015[[Window]]
  # Permutations2015=unlist(Permutations2015)
  # Permutations2015=Permutations2015[!is.na(Permutations2015)]
  # 
  # MeanPermutation2015=mean(Permutations2015, na.rm = T)
  # Upper_CI_P2015=MeanPermutation2015 + 1.96*sd(Permutations2015, na.rm = T)/sqrt(length(Permutations2015))
  # Lower_CI_P2015=MeanPermutation2015 - 1.96*sd(Permutations2015, na.rm = T)/sqrt(length(Permutations2015))
  # 
  # AllPermutationMeans=cbind(MeanPermutation2012, MeanPermutation2013, MeanPermutation2014, MeanPermutation2015)
  # AllPermutationMeans=cbind("Permutation Means", AllPermutationMeans)
  # colnames(AllPermutationMeans)=c("Year","2012","2013","2014","2015")
  # AllUpper_CI=cbind(Upper_CI_P2012, Upper_CI_P2013, Upper_CI_P2014, Upper_CI_P2015)
  # AllUpper_CI=cbind("Permutations Upper_CI", AllUpper_CI)
  # colnames(AllUpper_CI)=c("Year","2012","2013","2014","2015")
  # AllLower_CI=cbind(Lower_CI_P2012, Lower_CI_P2013, Lower_CI_P2014, Lower_CI_P2015)
  # AllLower_CI=cbind("Permutations Lower_CI", AllLower_CI)
  # colnames(AllLower_CI)=c("Year","2012","2013","2014","2015")
  # 
  # AllPermutationsInfo=rbind(AllPermutationMeans, AllUpper_CI, AllLower_CI)
  
  AllSummaries=cbind(Table2012, Table2013[,2], Table2014[,2], Table2015[,2])
  colnames(AllSummaries)=c("Year","2012","2013","2014","2015")
  
  # AllSummaries=rbind(AllSummaries[c(1:5),], AllPermutationsInfo, AllSummaries[c(6:13),])
  write.csv(AllSummaries, file=paste0(writingDir,"500 Permutations/AllSummaries.csv"))
}else{
  AllSummaries=read.csv(file=paste0(writingDir,"500 Permutations/AllSummaries.csv"))
}


######################################
#### Number Episodes per Infector ####

PairsDistanceTable=read.csv(file=paste0(writingDir, "2012-2015 All Results/PairsDistanceInfoTable.csv"))
Freq2012to2015=as.data.frame(table(table(PairsDistanceTable$PotentialInfector)), stringsAsFactors = F)
Freq2012to2015$Var1=as.numeric(Freq2012to2015$Var1)
Freq2012to2015$Freq=as.numeric(Freq2012to2015$Freq)
Freq2012to2015Final= as.vector(rep(Freq2012to2015$Var1, Freq2012to2015$Freq))

FinalHist=hist(Freq2012to2015Final, breaks=c(1:9), xlab="Multiple spreading event size", 
                   ylab="Frequency", main="2012-2015", cex.axis=1, cex.main=5, cex.lab=3,
                   col=c("aquamarine4"))

######################################
#### Number Episodes per Infector ####

cat("Load\n")
Table2012=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2012), " Results","/PairsDistanceInfoTable.csv"))
Table2013=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2013), " Results","/PairsDistanceInfoTable.csv"))
Table2014=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2014), " Results","/PairsDistanceInfoTable.csv"))
Table2015=read.csv(file=paste0(writingDir,"500 Permutations/", as.character(2015), " Results","/PairsDistanceInfoTable.csv"))

Freq2012=as.data.frame(table(table(Table2012$PotentialInfector)), stringsAsFactors = F)
Freq2013=as.data.frame(table(table(Table2013$PotentialInfector)), stringsAsFactors = F)
Freq2014=as.data.frame(table(table(Table2014$PotentialInfector)), stringsAsFactors = F)
Freq2015=as.data.frame(table(table(Table2015$PotentialInfector)), stringsAsFactors = F)

AllFrequencies1=merge(Freq2012, Freq2013, by="Var1", all=T)
colnames(AllFrequencies1)=c("Var1", "Year2012", "Year2013")
AllFrequencies2=merge(AllFrequencies1, Freq2014, by="Var1", all=T)
colnames(AllFrequencies2)=c("Var1", "Year2012", "Year2013", "Year2014")
AllFrequencies3=merge(AllFrequencies2, Freq2015, by="Var1", all=T)
colnames(AllFrequencies3)=c("Freq", "Year2012", "Year2013", "Year2014", "Year2015")
AllFrequencies3[is.na(AllFrequencies3)] = 0
AllFrequencies4=rbind(c(8,0,0,0,0), AllFrequencies3)
AllFrequencies4=AllFrequencies4[order(AllFrequencies4$Freq),]
rownames(AllFrequencies4) = 1:9

# melt the data frame for plotting
AllFrequencies5 <- melt(AllFrequencies4, id.vars='Freq')

# Unstacked
ggplot(AllFrequencies5, aes(Freq, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme(legend.title=element_blank()) +
  guides(fill=guide_legend(title="Year")) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal()

# Stacked
ggplot(AllFrequencies5, aes(Freq, value, fill = variable)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme(legend.title=element_blank()) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal()

####################
### Only 2 >

AllFrequencies6=AllFrequencies5[which(AllFrequencies5$Freq > 2),]

# Unstacked
ggplot(AllFrequencies6, aes(Freq, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") 


# Stacked
ggplot(AllFrequencies6, aes(Freq, value, fill = variable)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  guides(fill=guide_legend(title="Year")) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal()

##############
### void

# Unstacked
ggplot(AllFrequencies5, aes(Freq, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  xlab("Number of Non-Imported Episodes from Single Potential Infector") +
  ylab("Number of Potential Infectors") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") 


# Stacked
ggplot(AllFrequencies5, aes(Freq, value, fill = variable)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4"), 
                    labels=c("2012","2013","2014","2015")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") 

#####################
##### Trend Test ####

# 2 or more
SS2=c(sum(Freq2012[2:nrow(Freq2012),2]), 
      sum(Freq2013[2:nrow(Freq2013),2]),
      sum(Freq2014[2:nrow(Freq2014),2]),
      sum(Freq2015[2:nrow(Freq2015),2]))
EP=c(sum(Freq2012[,2]), 
     sum(Freq2013[,2]),
     sum(Freq2014[,2]),
     sum(Freq2015[,2]))

#Test
prop.trend.test(SS2, EP)
# H0: There is no linear trend in the proportion of cases across age groups
# Ha: There is a linear trend in the proportion of cases across age groups

# 3 or more
SS3=c(sum(Freq2012[3:nrow(Freq2012),2]), 
      sum(Freq2013[3:nrow(Freq2013),2]),
      sum(Freq2014[3:nrow(Freq2014),2]),
      sum(Freq2015[3:nrow(Freq2015),2]))

#Test
prop.trend.test(SS3, EP)
# H0: There is no linear trend in the proportion of cases across age groups
# Ha: There is a linear trend in the proportion of cases across age groups

# 4 or more
SS4=c(sum(Freq2012[4:nrow(Freq2012),2]), 
      sum(Freq2013[4:nrow(Freq2013),2]),
      sum(Freq2014[4:nrow(Freq2014),2]),
      sum(Freq2015[4:nrow(Freq2015),2]))

#Test
prop.trend.test(SS4, EP)
# H0: There is no linear trend in the proportion of cases across age groups
# Ha: There is a linear trend in the proportion of cases across age groups



##############################################
##### Number of Cases and Secondary Cases ####

library(data.table)
Table2012=as.data.table(Table2012)
Table2013=as.data.table(Table2013)
Table2014=as.data.table(Table2014)
Table2015=as.data.table(Table2015)

TableFreq2012=Table2012[,MultipleSpreading := .N, by = PotentialInfector]
TableFreq2013=Table2013[,MultipleSpreading := .N, by = PotentialInfector]
TableFreq2014=Table2014[,MultipleSpreading := .N, by = PotentialInfector]
TableFreq2015=Table2015[,MultipleSpreading := .N, by = PotentialInfector]

# 2012
TableFreq2012$CaseRank=rank(TableFreq2012$PiTotalCases, ties.method = "average")
TableFreq2012$FreqRank=rank(TableFreq2012$MultipleSpreading, ties.method = "average")
cor.test(TableFreq2012$CaseRank, TableFreq2012$FreqRank, method = "kendall")
# 2013
TableFreq2013$CaseRank=rank(TableFreq2013$PiTotalCases, ties.method = "average")
TableFreq2013$FreqRank=rank(TableFreq2013$MultipleSpreading, ties.method = "average")
cor.test(TableFreq2013$CaseRank, TableFreq2013$FreqRank, method = "kendall")
# 2014
TableFreq2014$CaseRank=rank(TableFreq2014$PiTotalCases, ties.method = "average")
TableFreq2014$FreqRank=rank(TableFreq2014$MultipleSpreading, ties.method = "average")
cor.test(TableFreq2014$CaseRank, TableFreq2014$FreqRank, method = "kendall")
# 2015
TableFreq2015$CaseRank=rank(TableFreq2015$PiTotalCases, ties.method = "average")
TableFreq2015$FreqRank=rank(TableFreq2015$MultipleSpreading, ties.method = "average")
cor.test(TableFreq2015$CaseRank, TableFreq2015$FreqRank, method = "kendall")
# 2012-2015
Table2012to2015=read.csv(file=paste0(writingDir,"2012-2015 All Results/PairsDistanceInfoTable.csv"))
Table2012to2015=as.data.table(Table2012to2015)
TableFreq2012to2015=Table2012to2015[,MultipleSpreading := .N, by = PotentialInfector]
TableFreq2012to2015$CaseRank=rank(TableFreq2012to2015$PiTotalCases, ties.method = "average")
TableFreq2012to2015$FreqRank=rank(TableFreq2012to2015$MultipleSpreading, ties.method = "average")
cor.test(TableFreq2012to2015$CaseRank, TableFreq2012to2015$FreqRank, method = "kendall")

