#####################################################################################
##### Reconstructing Transmission Tree Between Episodes and Potential Infectors #####
#####################################################################################

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Original or Transformed Weights\n")
Transformed=T

cat("Upload Department Contact Network\n")
if(Transformed){
  cat("Upload Department Network with Transformed Weights\n")
  load("Data/Department Network (Transformed).RData")
}else{
  cat("Upload Department Contact Network without Transformations\n")
  load("../Hospital_Network/HospitalNetwork/Data/Department Network.RData")
}

###################
#### LOAD DATA ####

cat("Choose start date\n")
startDate="2015-01-01"

cat("Choose end date\n")
endDate="2015-12-30"

cat("Get CPE Data with Mechanism and Class Info\n")
data=getCPEData()

#########################
#### LOAD CANDIDATES ####

load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#######################
#### SELECT WINDOW ####

cat("Get lowest observed minimum distance and window")
MinimumDistances_Clean=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding)) %do% CleaningFunction2(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[i]])
MinimumDistances_MeansByNWindow=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)

min(unlist(MinimumDistances_MeansByNWindow))
Window=which.min(MinimumDistances_MeansByNWindow)

cat("From plots, window between Day X and Day Y showed biggest difference")
Day=Window

cat(paste("Subset Candidates from Day =", Day))
CandidateTransmitters=CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Day]]
# MinDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Day]]

##################################
#### GET POTENTIAL CANDIDATES ####

cat("Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters\n")

cat("List of Departments of Episode and Candidate Transmitters\n")
Department="Department"
CandidateTransmitters_Departments=lapply(1:length(CandidateTransmitters), function(i) {
  CandidateTransmitters_Department=as.data.frame(as.character(CandidateTransmitters[[i]]$Department), stringsAsFactors = FALSE)
  colnames(CandidateTransmitters_Department)=Department
  return(CandidateTransmitters_Department)
})

cat("Set Parameters\n")
weights = E(directed.graph_Dept)$weight
algorithm = "dijkstra"
cat("Distance Matrix Between Departments\n")
Distances_Matrix=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))

cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
CandidateTransmitters_Departments_MinDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
  CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
    if(nrow(CandidateTransmitters_Departments_Subset) > 0 ){
      Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
        Distances=Distances_Matrix[CandidateTransmitters_Departments_Subset[j,Department],data[i,Department]]
      } 
    }else{
      Distances=NA
    }
}

cat("Locate Potential Infector\n")
MinDistanceLocation=lapply(CandidateTransmitters_Departments_MinDistances, function(x) which.min(x))

cat("Get Info of Potential Infector\n")
PotentialCandidates=foreach(i=1:length(CandidateTransmitters)) %do% {
  if(!is.na(CandidateTransmitters_Departments_MinDistances[[i]])){
    CandidateTransmitters_Subset=CandidateTransmitters[[i]]
    MinDistanceLocation_Subset=MinDistanceLocation[[i]]
    PotentialCandidate=CandidateTransmitters_Subset[MinDistanceLocation_Subset,]
  }
}

###################
#### GET PAIRS ####

#ALL
Episode="Episode"
Pairs_Episodes=foreach(i=1:length(PotentialCandidates), .combine = 'rbind') %do% {
  if(is.null(PotentialCandidates[[i]])){
    None=c(NA, NA)
  }else{
    Pairs_Episode=c(PotentialCandidates[[i]]$Episode, data[i,Episode])}
}

Pairs_Episodes=Pairs_Episodes[complete.cases(Pairs_Episodes),]
Pairs_Episodes=Pairs_Episodes[order(Pairs_Episodes[,1]),]
Pairs_Episodes=cbind(as.character(Pairs_Episodes[,1]), as.character(Pairs_Episodes[,2]))

#Imported as Source
Pairs_Episodes_Imported=foreach(i=1:length(PotentialCandidates), .combine = 'rbind') %do% {
  if(is.null(PotentialCandidates[[i]])){
    None=c(NA, NA)
  }else{
    require(plyr)
    PotentialCandidates_Imported=llply(PotentialCandidates, function(x) subset(x, x$Imported == "O"))
    data_Imported=data[which(data$Imported == "O"),]
    Pairs_Episode=c(PotentialCandidates_Imported[[i]]$Episode, data_Imported[i,Episode])}
}

Pairs_Episodes_Imported=Pairs_Episodes_Imported[complete.cases(Pairs_Episodes_Imported),]
Pairs_Episodes_Imported=Pairs_Episodes_Imported[order(Pairs_Episodes_Imported[,1]),]
Pairs_Episodes_Imported=cbind(as.character(Pairs_Episodes_Imported[,1]), as.character(Pairs_Episodes_Imported[,2]))

##############################
#### Description of Pairs ####

#Get Distances
PairsDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Window]]
NonNAPairsDistances=PairsDistances[complete.cases(PairsDistances)]

#Get Table of Pairs and Distances
PairsDistanceTable=as.data.frame(cbind(Pairs_Episodes, NonNAPairsDistances))
#Number of different sources
length(levels(PairsDistanceTable$V1))
#Number of different targets
length(levels(PairsDistanceTable$V2))

#Add info to table
PairsDistanceTable$V1=as.character(PairsDistanceTable$V1)
PairsDistanceTable$V2=as.character(PairsDistanceTable$V2)
PairsDistanceTable$NonNAPairsDistances=as.numeric(as.character(PairsDistanceTable$NonNAPairsDistances))
PairsDistanceTable$SourceImported=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Imported[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$SourceDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$TargetDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V2[[i]])]
PairsDistanceTable$Mechanism=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Mechanism[which(data$Episode == PairsDistanceTable$V1[[i]])]
colnames(PairsDistanceTable)=c("Source","Target","ShortestPathDistance","SourceImported","SourceDepartment","TargetDepartment","Mechanism")
PairsDistanceTable=PairsDistanceTable[,c("Source","SourceImported","SourceDepartment","ShortestPathDistance","TargetDepartment","Target","Mechanism")]

#Save
write.csv(PairsDistanceTable, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/PairsDistanceInfoTable.csv"))

##########################
#### Summary of Pairs ####

#Average Length Between Pairs
Mean=mean(NonNAPairsDistances)
#Range
Min=min(NonNAPairsDistances)
NonZeroMin=min(NonNAPairsDistances[NonNAPairsDistances > 0])
Max=max(NonNAPairsDistances)
#CI
Upper_CI=Mean + 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))
Lower_CI=Mean - 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))

#Total episodes linked out of total episodes
TotalLinked=nrow(Pairs_Episodes)/nrow(data)
#How many episodes are explained
prop.table(table(unlist(lapply(PotentialCandidates, is.null))))

#Total imported episodes linked out of total imported episodes
TotalLinkedImported=nrow(Pairs_Episodes_Imported)/nrow(data_Imported)

#Total non-imported episodes linked out of total non-imported episodes
NonImportedEpisodes=data$Episode[which(data$Imported == "N")]
NonImportedEpisodesWithSource=NonImportedEpisodes[NonImportedEpisodes %in% Pairs_Episodes[,2]]
TotalLinkedNonImported=length(NonImportedEpisodesWithSource)/length(NonImportedEpisodes)

#SummaryTable
SummaryTable=as.data.frame(rbind(nrow(data), nrow(data_Imported), length(NonImportedEpisodes), length(NonNAPairsDistances), Mean, Min, NonZeroMin, Max, Upper_CI, Lower_CI, TotalLinked,
                   TotalLinkedImported, TotalLinkedNonImported))
rownames(SummaryTable)=c("All 2015 Episodes","All 2015 Imported Episodes","All 2015 Non-Imported Episodes","Number of Pairs","Mean Distance","Min","NonZeroMin","Max","Upper_CI","Lower_CI","% Episodes Linked Out of Total",
                         "% Imported Episodes as Source","% Non-Imported Episodes as Targets")
colnames(SummaryTable)=c("2015 Pairs at Day 21-28")

#Save
write.csv(SummaryTable, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/SummaryTable.csv"))


###################
#### GET GRAPH ####

TreeGraph=graph_from_edgelist(Pairs_Episodes, directed = T)
# plot(TreeGraph, layout=layout_as_tree(TreeGraph, mode="out"), vertex.size=2, vertex.label.cex=0.5)
#
# layout=layout.reingold.tilford(TreeGraph, circular=T)
# layout=layout.fruchterman.reingold(TreeGraph)
# plot(TreeGraph, layout=layout, vertex.size=7, vertex.label.cex=0.5)


#Add attributes
names=V(TreeGraph)$name
names_int=as.integer(names)

DataSubset=data[data$Episode %in% names_int, c("DateEpisode","Episode", "Mechanism", "Department")]
DataSubset=DataSubset[match(names_int, DataSubset$Episode),]

V(TreeGraph)$Mechanism=DataSubset$Mechanism
V(TreeGraph)$Department=DataSubset$Department

write.graph(TreeGraph, file=paste0(writingDir,"2015 CPE Shortest Distance Pairs Tree (with Attributes) Day 21-28.gml"), format="gml")

#Decompose network into components
DecomposedNetwork <- decompose.graph(TreeGraph)

####################
#### visNETWORK ####

getNetwork=function(DecomposedNetwork, subgraph){
  Network <- toVisNetworkData(DecomposedNetwork[[subgraph]])
  
  MechanismFactor=as.factor(nodes$Mechanism)
  MechanismColors=c(palette(rainbow(8)))[MechanismFactor]
  
  nodes <- Network$nodes
  nodes$group=nodes$Mechanism 
  edges <- Network$edges
  
  newMechanism=gsub(" ", "", nodes$Mechanism, fixed = TRUE)
  nodes$group=newMechanism
  
  nodes$label=nodes$Department
  nodes$value=20
  nodes$shadow=TRUE
  
  edges$smooth=F
  
  Plot=visNetwork(nodes, edges, width = "100%", font.size =10, shape='circle') %>%
    visHierarchicalLayout(sortMethod="directed") %>%
    visEdges(arrows = 'to') 
  
  return(Plot)
}

Plot=getNetwork(DecomposedNetwork, 5)
Plot

##########################
#### Temporal Network ####

library("ndtv")

#Get episode dates
DataSubset$Days=DataSubset$DateEpisode-DataSubset$DateEpisode[1]

DynamicEdgeList=Pairs_Episodes

#add department for each edge and onset terminus days


