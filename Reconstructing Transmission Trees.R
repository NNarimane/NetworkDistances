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

cat("Choose year to analyze\n")
Year="2015"

cat("Choose start date\n")
startDate=paste0(Year, "-01-01")

cat("Choose end date\n")
endDate=paste0(Year, "-12-30")

cat("Get CPE Data with Mechanism and Class Info\n")
data=getCPEData()

#########################
#### LOAD CANDIDATES ####

cat(paste("Load", Year, "Candidates\n"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

cat(paste("Load", Year, "Minimum Distances\n"))
load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#######################
#### GET ALL PAIRS ####

AllPairs=foreach(i=1:length(MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding)) %do% {
  getPairsByWindow(i, MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding, CandidateTransmitters_byDay_byMechanism_SharedDept_Reshuffled_Sliding)
}

##############################
#### Description of Pairs ####

cat("Get distances between pairs\n")
PairsDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Window]]
NonNAPairsDistances=PairsDistances[complete.cases(PairsDistances)]

cat("Get Table of Pairs and Distances\n")
PairsDistanceTable=as.data.frame(cbind(Pairs_Episodes, NonNAPairsDistances))
cat("Number of different sources\n")
length(levels(PairsDistanceTable$V1))
cat("Number of different targets\n")
length(levels(PairsDistanceTable$V2))

cat("Add info to table\n")
PairsDistanceTable$V1=as.character(PairsDistanceTable$V1)
PairsDistanceTable$V2=as.character(PairsDistanceTable$V2)
PairsDistanceTable$NonNAPairsDistances=as.numeric(as.character(PairsDistanceTable$NonNAPairsDistances))
PairsDistanceTable$SourceImported=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Imported[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$SourceDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V1[[i]])]
PairsDistanceTable$TargetDepartment=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Department[which(data$Episode == PairsDistanceTable$V2[[i]])]
PairsDistanceTable$Mechanism=foreach(i=1:nrow(PairsDistanceTable), .combine = 'c') %do% data$Mechanism[which(data$Episode == PairsDistanceTable$V1[[i]])]
colnames(PairsDistanceTable)=c("Source","Target","ShortestPathDistance","SourceImported","SourceDepartment","TargetDepartment","Mechanism")
PairsDistanceTable=PairsDistanceTable[,c("Source","SourceImported","SourceDepartment","ShortestPathDistance","TargetDepartment","Target","Mechanism")]

cat("Save or load\n")
# write.csv(PairsDistanceTable, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/PairsDistanceInfoTable.csv"))
# load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/PairsDistanceInfoTable.csv"))

##########################
#### Summary of Pairs ####


cat("Average Length Between Pairs\n")
Mean=mean(NonNAPairsDistances)
cat("Range\n")
Min=min(NonNAPairsDistances)
NonZeroMin=min(NonNAPairsDistances[NonNAPairsDistances > 0])
Max=max(NonNAPairsDistances)
cat("CI\n")
Upper_CI=Mean + 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))
Lower_CI=Mean - 1.96*sd(NonNAPairsDistances)/sqrt(length(NonNAPairsDistances))

cat("Total episodes linked out of total episodes\n")
TotalLinked=nrow(Pairs_Episodes)/nrow(data)
cat("How many episodes are explained\n")
prop.table(table(unlist(lapply(PotentialInfectors, is.null))))

cat("Total imported episodes linked out of total imported episodes\n")
TotalLinkedImported=nrow(Pairs_Episodes_Imported)/nrow(data_Imported)

cat("Total non-imported episodes linked out of total non-imported episodes\n")
NonImportedEpisodes=data$Episode[which(data$Imported == "N")]
NonImportedEpisodesWithSource=NonImportedEpisodes[NonImportedEpisodes %in% Pairs_Episodes[,2]]
TotalLinkedNonImported=length(NonImportedEpisodesWithSource)/length(NonImportedEpisodes)

cat("Summary Table\n")
SummaryTable=as.data.frame(rbind(nrow(data), nrow(data_Imported), length(NonImportedEpisodes), length(NonNAPairsDistances), Mean, Min, NonZeroMin, Max, Upper_CI, Lower_CI, TotalLinked,
                   TotalLinkedImported, TotalLinkedNonImported))
rownames(SummaryTable)=c(paste("All", Year, "Episodes"), paste("All", Year, "Imported Episodes"),"All 2015 Non-Imported Episodes","Number of Pairs","Mean Distance","Min","NonZeroMin","Max","Upper_CI","Lower_CI","% Episodes Linked Out of Total",
                         "% Imported Episodes as Source","% Non-Imported Episodes as Targets")
colnames(SummaryTable)=paste(Year, "Pairs at Day 21-28")

cat("Save or load\n")
# write.csv(SummaryTable, file=paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/SummaryTable.csv"))
# load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/SummaryTable.csv"))


###################
#### GET GRAPH ####

cat("Get Graph of Pairs\n")
TreeGraph=graph_from_edgelist(Pairs_Episodes, directed = T)

cat("Get order of names\n")
names=V(TreeGraph)$name
names_int=as.integer(names)

cat("Get info for each node\n")
DataSubset=data[data$Episode %in% names_int, c("DateEpisode","Episode", "Mechanism", "Department", "TotalCases", "Imported")]
cat("Number of episodes per department per year\n")
DeptTable=as.data.frame(table(data$Department))
DataSubset=merge(DataSubset, DeptTable, by.x="Department", by.y="Var1", all.x=T)
DataSubset=DataSubset[match(names_int, DataSubset$Episode),]

cat("Add attributes\n")
V(TreeGraph)$Mechanism=DataSubset$Mechanism
V(TreeGraph)$Department=DataSubset$Department
V(TreeGraph)$Cases=DataSubset$TotalCases
V(TreeGraph)$Imported=DataSubset$Imported
V(TreeGraph)$YearlyNumberEpisodes=DataSubset$Freq

cat("Save\n")
# write.graph(TreeGraph, file=paste0(writingDir, Year, " CPE Shortest Distance Pairs Tree (with Attributes3) Day 21-28.gml"), format="gml")

cat("Decompose network into components\n")
DecomposedNetwork <- decompose.graph(TreeGraph)
cat("Save\n")
# save(DecomposedNetwork, file=paste0(writingDir, Year, " CPE Shortest Distance Pairs Decomposed Trees (with Attributes) Day 21-28.RData"))

cat("Find position of largest component\n")
MaxLocation=which.max(sapply(DecomposedNetwork, vcount))
LargestComponent=DecomposedNetwork[[MaxLocation]]

####################
#### visNETWORK ####

cat("VizNetwork Function\n")
getNetwork=function(DecomposedNetwork, subgraph){
  Network <- toVisNetworkData(DecomposedNetwork[[subgraph]])
  
  nodes <- Network$nodes
  nodes$group=nodes$Imported 
  edges <- Network$edges
  
  ImportedFactor=as.factor(nodes$group)
  Colors=c(palette(rainbow(2)))[ImportedFactor]
  
  # newMechanism=gsub(" ", "", nodes$Mechanism, fixed = TRUE)
  # nodes$group=newMechanism
  
  nodes$label=nodes$Department
  # nodes$value=nodes$Cases
  nodes$value=nodes$YearlyNumberEpisodes/10
  nodes$shadow=TRUE
  
  edges$smooth=F
  
  Plot=visNetwork(nodes, edges, width = "100%", font.size=100, shape='circle') %>%
    visHierarchicalLayout(sortMethod="directed", direction = "LR") %>%
    visEdges(arrows = 'to') %>% visLegend(main="Imported", width = 0.1)
  
  return(Plot)
}

cat("Plot VizNetwork\n")
Plot=getNetwork(DecomposedNetwork, MaxLocation)
Plot

##########################
#### Temporal Network ####

cat("Get episode dates\n")
DataSubset$Days=DataSubset$DateEpisode-DataSubset$DateEpisode[1]

cat("Temporal Network for All Episodes or Largest Components\n")
Tree=T
if(Tree){
  DynamicEdgeList=as.data.frame(Pairs_Episodes)
}else{
  DynamicEdgeList=as.data.frame(get.edgelist(LargestComponent))
}

cat("Merge databases\n")
SourceInfo=merge(DynamicEdgeList, DataSubset, by.x="V1", by.y="Episode", all.x=T)
SourceInfo_TargetInfo=merge(SourceInfo, DataSubset, by.x="V2", by.y="Episode")

cat("Select departments and days\n")
FinalDeptTempNetEdgeList=SourceInfo_TargetInfo[,c("Department.x","Department.y","Days.x","Days.y")]
FinalDeptTempNetEdgeList$Days.y=FinalDeptTempNetEdgeList$Days.y-min(FinalDeptTempNetEdgeList$Days.y)
FinalDeptTempNetEdgeList=FinalDeptTempNetEdgeList[order(FinalDeptTempNetEdgeList$Days.y),]
rownames(FinalDeptTempNetEdgeList)=1:nrow(FinalDeptTempNetEdgeList)
# colnames(FinalDeptTempNetEdgeList)=c("id1", "id2", "time")
# colnames(FinalDeptTempNetEdgeList)=c("Source", "Target", "time")

cat("Save Edgelist\n")
# write.csv(FinalDeptTempNetEdgeList, file=paste0(writingDir,Year, " Temporal Edgelist.csv"))

cat("Get Table of Departments \n")
SourceDeptTable=table(FinalDeptTempNetEdgeList$Department.x)
SourceDeptTable
max(SourceDeptTable) #for all depts, 130 sources come from Paris (75), 61 (13), 46 (94), 35 (69), 31 (92)
TargetDeptTable=table(FinalDeptTempNetEdgeList$Department.y)
TargetDeptTable
max(TargetDeptTable) #for all depts, 44 targets from (13), 35 (75), 30 (94), 29 (92), 28 (59), 25 (91), 22 (69)

# cat("Plot Graph of Departments \n")
# par(mfrow=c(2,1), mai=c(0.5,1,0.5,0.5))
# plot(prop.table(SourceDeptTable), cex.axis=0.5, cex=0.8, 
#      ylab="Prop. as Source Depts", 
#      main=paste(Year, "CPE 21-28 Days Pairs (n=464)"),
#      xaxs="i", yaxs="i", ylim=c(0,0.25))
# grid(col = "lightgray")
# plot(prop.table(TargetDeptTable), cex.axis=0.5, cex=0.8, 
#      ylab="Prop. as Target Depts",
#      xaxs="i", yaxs="i", ylim=c(0,0.25))
# grid(col = "lightgray")

cat("Convert dynamic edgelist to graph\n")
edgelist=as.matrix(FinalDeptTempNetEdgeList[,1:2])
TempGraph=graph_from_edgelist(edgelist, directed = T)

cat("Add time to edges\n")
E(TempGraph)$Time=FinalDeptTempNetEdgeList$Days.y

cat("Add geocoding of departments\n")
GeoCodes=read.csv(file="C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/All Dept Prefecture GeoCodes.csv")
GeoCodes$Number=str_pad(GeoCodes$Number, 2, pad = "0")
GeoCodes$Number[20]="20"

cat("Select latitude and longitude of network departments\n")
LatLongInfo=GeoCodes[which(GeoCodes$Number %in% V(TempGraph)$name), c("Number","Latitude", "Longitude")]
LatLongInfo=LatLongInfo[match(V(TempGraph)$name, LatLongInfo$Number),]

cat("Add latitude and longitude attributes\n")
V(TempGraph)$lat=LatLongInfo$Latitude
V(TempGraph)$lon=LatLongInfo$Longitude

cat("Add number of yearly episodes, imported episodes, and non-imported episodes as node attributes\n")
DeptTable_AllEpisodes=as.data.frame(table(data$Department))
DeptTable_ImportedEpisodes=as.data.frame(table(data$Department[which(data$Imported == "O")]))
DeptTable_NonImportedEpisodes=as.data.frame(table(data$Department[which(data$Imported == "N")]))

#All
DeptSizes_AllEpisodes=DeptTable_AllEpisodes[which(DeptTable_AllEpisodes$Var1 %in% V(TempGraph)$name),]
DeptSizes_AllEpisodes=DeptSizes_AllEpisodes[match(V(TempGraph)$name, DeptSizes_AllEpisodes$Var1),]
DeptSizes_AllEpisodes$Var1=V(TempGraph)$name
DeptSizes_AllEpisodes[is.na(DeptSizes_AllEpisodes)]=0
V(TempGraph)$SizeAllEpisodes=DeptSizes_AllEpisodes$Freq

#Imported
DeptSizes_ImportedEpisodes=DeptTable_ImportedEpisodes[which(DeptTable_ImportedEpisodes$Var1 %in% V(TempGraph)$name),]
DeptSizes_ImportedEpisodes=DeptSizes_ImportedEpisodes[match(V(TempGraph)$name, DeptSizes_ImportedEpisodes$Var1),]
DeptSizes_ImportedEpisodes$Var1=V(TempGraph)$name
DeptSizes_ImportedEpisodes[is.na(DeptSizes_ImportedEpisodes)]=0
V(TempGraph)$SizeImportedEpisodes=DeptSizes_ImportedEpisodes$Freq

#NonImported
DeptSizes_NonImportedEpisodes=DeptTable_NonImportedEpisodes[which(DeptTable_NonImportedEpisodes$Var1 %in% V(TempGraph)$name),]
DeptSizes_NonImportedEpisodes=DeptSizes_NonImportedEpisodes[match(V(TempGraph)$name, DeptSizes_NonImportedEpisodes$Var1),]
DeptSizes_NonImportedEpisodes$Var1=V(TempGraph)$name
DeptSizes_NonImportedEpisodes[is.na(DeptSizes_NonImportedEpisodes)]=0
V(TempGraph)$SizeNonImportedEpisodes=DeptSizes_NonImportedEpisodes$Freq

cat("Add node color attributes\n")
# DeptColors=as.data.frame(cbind(Dept=V(directed.graph_Dept)$name, Colors=colorRampPalette(brewer.pal(11, "Spectral"))(93)), stringsAsFactors = F)
# save(DeptColors, file="C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/DeptColors.RData")
load(file="C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/DeptColors.RData")
DeptColorsSubset=DeptColors[which(DeptColors$Dept %in% V(TempGraph)$name),]
DeptColorsSubset=DeptColorsSubset[match(V(TempGraph)$name, DeptColorsSubset$Dept),]
V(TempGraph)$Color=DeptColorsSubset$Colors

cat("Save temporal graph\n")
write.graph(TempGraph, file=paste0(writingDir,Year, " TempGraph With Sizes and Color.gml"), format="gml")

##############
# Dept Network

cat("Upload number of hospitals by department\n")
load(file="C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/Hospital Depts.RData")

cat("Table of hospitals per department\n")
hospdeptstable=as.data.frame(table(hospital_depts))
hospdeptstable$Percent=hospdeptstable$Freq/sum(hospdeptstable$Freq)

