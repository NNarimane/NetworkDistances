####################################################################################################################
##### Does Transforming Hospital Network Edge Weights to Infection Probabilities Change Community Properties ? #####
############################## Analyzing Hospitals Based on Prop of Infection ######################################
####################################################################################################################

#################################
#### INSTALL & LOAD PACKAGES ####

cat("Load Packages\n")
library("igraph")
library("stringr")
library("foreach")

#########################
#### SET ENVIRONMENT ####

cat("Set Working Environment\n")
envNN=T
envNNwindows=T
if(envNN){
  if(envNNwindows){
    currentwd=setwd("C:/Users/Narimane/Dropbox/CPE Transmission Chains/")
  }else{
    currentwd=setwd("/Users/narimanenekkab/Dropbox/CPE Transmission Chains/")
  }
}else{
  currentwd=setwd("/Users/pascalcrepey/Google Drive/1-EPC/stageNN/") 
}

##########################################################
#### UPLOAD HOSPITAL NETWORK WITH TRANSFORMED WEIGHTS ####

load("Data/Hospital Network (Prob Weights).RData")
# load("Data/Hospital Undirected Network (Prob Weights).RData")

##############################
#### UPLOAD HOSPITAL INFO ####

load("C:/Users/Narimane/Dropbox/Hospital_Network/HospitalNetwork/Data/NodesWithHospitalInfo 201417.05.16-1004 (All Patient Network).RData")

cat("Select latitude and longitude of network hospitals\n")
NodesWithHospitalInfo$FINESS=as.character(NodesWithHospitalInfo$FINESS)
LatLongInfo_Directed=NodesWithHospitalInfo[which(NodesWithHospitalInfo$FINESS %in% V(directed.graph_ALL)$name), c("latitude", "longitude")]
# LatLongInfo_UnDirected=NodesWithHospitalInfo[which(NodesWithHospitalInfo$FINESS %in% V(undirected.graph_ALL)$name), c("latitude", "longitude")]

##########################
#### GREEDY ALGORITHM ####

# cat("Run greedy algorithm\n")
# greedy.communities=cluster_fast_greedy(undirected.graph_ALL, merges = TRUE, modularity = TRUE, membership = TRUE, weights = E(undirected.graph_ALL)$weight)
# 
# cat("Set greedy communities\n")
# V(undirected.graph_ALL)$communities=membership(greedy.communities)
# sizes(greedy.communities)
# 
# cat("Set latitude and longitude of network hospitals\n")
# V(undirected.graph_ALL)$lat=LatLongInfo_UnDirected$latitude
# V(undirected.graph_ALL)$lon=LatLongInfo_UnDirected$longitude
# 
# cat("Save\n")
# write.graph(undirected.graph_ALL, "Data/Undirected Transformed Weighted Hospital Network with Greedy Communities.gml", format="gml")
# save(undirected.graph_ALL, file="Data/Undirected Transformed Weighted Hospital Network with Greedy Communities.RData")

######################
#### MAP EQUATION ####

# cat("Run greedy algorithm\n")
# map.communities=cluster_infomap(directed.graph_ALL, modularity = TRUE, e.weights = E(directed.graph_ALL)$weight, 
#                 # v.weights = V(directed.graph_ALL)$weight, 
#                 nb.trials = 100)
# 
# cat("Set greedy communities\n")
# V(directed.graph_ALL)$communities=membership(map.communities)
# sizes(map.communities)
# 
# cat("Set latitude and longitude of network hospitals\n")
# V(directed.graph_ALL)$lat=LatLongInfo_Directed$latitude
# V(directed.graph_ALL)$lon=LatLongInfo_Directed$longitude
# 
# cat("Save\n")
# write.graph(directed.graph_ALL, "Data/Directed Prob Weighted Hospital Network with Map Communities.gml", format="gml")
# save(directed.graph_ALL, file="Data/Directed Prob Weighted Hospital Network with Map Communities.RData")

########################################################################################
#### Alternative Method of Getting Network Weight: Distribution of Weighted Degrees ####

load("Data/Directed Prob Weighted Hospital Network with Map Communities.RData")

Edgelist=cbind(get.edgelist(directed.graph_ALL), round(E(directed.graph_ALL)$weight, 10))

# cat("Set in-degree and out-degree of network\n")
# V(directed.graph_ALL)$Degree=degree(directed.graph_ALL, mode="total")
# V(directed.graph_ALL)$Indegree=degree(directed.graph_ALL, mode="in")
# V(directed.graph_ALL)$Outdegree=degree(directed.graph_ALL, mode="out")

cat("Set weighted in-degree and out-degree of network\n")
V(directed.graph_ALL)$WeightedDegree=round(strength(directed.graph_ALL, mode="total"), 10)
V(directed.graph_ALL)$WeightedIndegree=round(strength(directed.graph_ALL, mode="in"), 10)
V(directed.graph_ALL)$WeightedOutdegree=round(strength(directed.graph_ALL, mode="out"), 10)

# cat("Plot distribution of weighted in-degree and out-degree of network\n")
# Degree=V(directed.graph_ALL)$Degree
# head(table(Degree))
# Indegree=V(directed.graph_ALL)$Indegree
# head(table(Indegree))
# Outdegree=V(directed.graph_ALL)$Outdegree
# head(table(Outdegree))

cat("Put in table\n")
WeightedDegree=round(V(directed.graph_ALL)$WeightedDegree, 10)
WeightedDegree_Table=as.data.frame(table(WeightedDegree))
WeightedDegree_Table$WeightedDegree=as.numeric(as.character(WeightedDegree_Table$WeightedDegree))

WeightedIndegree=round(V(directed.graph_ALL)$WeightedIndegree, 10)
WeightedIndegree_Table=as.data.frame(table(WeightedIndegree))
WeightedIndegree_Table$WeightedIndegree=as.numeric(as.character(WeightedIndegree_Table$WeightedIndegree))
WeightedIndegree_Table=WeightedIndegree_Table[which(WeightedIndegree_Table$WeightedIndegree > 0),]

WeightedOutdegree=round(V(directed.graph_ALL)$WeightedOutdegree, 10)
WeightedOutdegree_Table=as.data.frame(table(WeightedOutdegree))

WeightedOutdegree_Table$WeightedOutdegree=as.numeric(as.character(WeightedOutdegree_Table$WeightedOutdegree))
WeightedOutdegree_Table=WeightedOutdegree_Table[which(WeightedOutdegree_Table$WeightedOutdegree > 0),]

cat("Plot\n")
par(mfrow=c(1,1))
# par(mfrow=c(3,1))

plot(WeightedDegree_Table, type="p", pch="*",ylab = "Frequency",
     main="Hospital Network Total Weighted Degree Distribution (Weight=Prob of Infection)")
grid(nx=30, ny=6, col="lightgrey", lwd = 1)
plot(WeightedIndegree_Table, type="p", pch="*", xlim=c(0,2), ylab = "Frequency",
     main="Hospital Network Weighted In-Degree Distribution (Weight=Prob of Infection)")
grid(nx=30, ny=6, col="lightgrey", lwd = 1)
plot(WeightedOutdegree_Table, type="p", pch="*", xlim=c(0,5), ylab = "Frequency",
     main="Hospital Network Weighted Out-Degree Distribution (Weight=Prob of Infection)")
grid(nx=30, ny=6, col="lightgrey", lwd = 1)


####improved

hist(log(WeightedDegree), breaks=20)
hist(log(WeightedIndegree), breaks=20)
hist(log(WeightedOutdegree), breaks=20)

WeightedDegrees=cbind(WeightedDegree, WeightedIndegree, WeightedOutdegree)
save(WeightedDegrees, file = "Data/Weighted Degrees.RData")

###Boxplots
par(mfrow=c(1,2))
boxplot(WeightedIndegree_Table$WeightedIndegree, ylab="Total Weighted InDegree")
boxplot(WeightedOutdegree_Table$WeightedOutdegree, ylab="Total Weighted OutDegree")


###Outliers via IQR
findUpperOutlierCutoff=function(x){
  Q3=quantile(x)[4]
  IQrange=IQR(x)
  Cutoff=Q3+1.5*IQrange
  return(Cutoff)
}

InDegCutoff=findUpperOutlierCutoff(WeightedIndegree_Table$WeightedIndegree)
OutDegCutoff=findUpperOutlierCutoff(WeightedOutdegree_Table$WeightedOutdegree)

InDegOutliers=WeightedIndegree_Table[which(WeightedIndegree_Table$WeightedIndegree>=InDegCutoff),]
OutDegOutliers=WeightedOutdegree_Table[which(WeightedOutdegree_Table$WeightedOutdegree>=OutDegCutoff),]

###Label Vulnerable and Infectious Hospitals
Label1=T
if(Label1){
  V(directed.graph_ALL)$InDegClassification[which(V(directed.graph_ALL)$WeightedIndegree>=InDegCutoff)]="Vulnerable"
}else{
  V(directed.graph_ALL)$InDegClassification[which(V(directed.graph_ALL)$WeightedIndegree<InDegCutoff)]=NA
}
Label2=T
if(Label2){
  V(directed.graph_ALL)$OutDegClassification[which(V(directed.graph_ALL)$WeightedOutdegree>=OutDegCutoff)]="Infectious"
}else{
  V(directed.graph_ALL)$OutDegClassification[which(V(directed.graph_ALL)$WeightedOutdegree<OutDegCutoff)]=NA
}

cat("Save\n")
write.graph(directed.graph_ALL, "Data/Directed Prob Weighted Hospital Network with Map Communities and WDeg Class.gml", format="gml")
save(directed.graph_ALL, file="Data/Directed Prob Weighted Hospital Network with Map Communities and WDeg Class.RData")

#############Hosp Info to add to network

load("../Data/NodesWithHospitalInfo 201406.10.16-1551.RData")

#Select FINESS, Type 1, Type 2
HospitalInfo=NodesWithHospitalInfo[, c(1,5,8,9)]

CorrectType2=function(){
  FINESS=as.character(NodesWithHospitalInfo$FINESS)
  
  CH=rep("Centres Hospitaliers", 2063)
  CHTF=grepl("CH ", NodesWithHospitalInfo$Name, ignore.case = F)
  CHData=as.data.frame(cbind(CH, CHTF))
  CHData$CH=as.character(CHData$CH)
  CorrectedCH=ifelse(CHData$CHTF == T, "Centres Hospitaliers", NA)
  CHData=as.data.frame(CorrectedCH, FINESS)
  
  Error=which(rownames(CHData) == 360000095)
  CHData[Error,1]<-NA
  
  CHIC=rep("Centres Hospitaliers", 2063)
  CHICTF=grepl("CHIC ", NodesWithHospitalInfo$Name, ignore.case = F)
  CHICData=as.data.frame(cbind(CHIC, CHICTF))
  CHICData$CHIC=as.character(CHICData$CHIC)
  CorrectedCHIC=ifelse(CHICData$CHICTF == T, "Centres Hospitaliers", NA)
  CHICData=as.data.frame(CorrectedCHIC, FINESS)
  
  HL=rep("Hôpitaux Locaux", 2063)
  HLTF=grepl("HL ", NodesWithHospitalInfo$Name, ignore.case = F)
  HLData=as.data.frame(cbind(HL, HLTF))
  HLData$HL=as.character(HLData$HL)
  CorrectedHL=ifelse(HLData$HLTF == T, "Hôpitaux Locaux", NA)
  HLData=as.data.frame(CorrectedHL, FINESS)
  
  CHR=rep("Centres Hospitaliers Régionaux", 2063)
  CHRTF=grepl("CHR ", NodesWithHospitalInfo$Name, ignore.case = F)
  CHRData=as.data.frame(cbind(CHR, CHRTF))
  CHRData$CHR=as.character(CHRData$CHR)
  CorrectedCHR=ifelse(CHRData$CHRTF == T, "Centres Hospitaliers Régionaux", NA)
  CHRData=as.data.frame(CorrectedCHR, FINESS)
  
  CHU=rep("Centres Hospitaliers Régionaux", 2063)
  CHUTF=grepl("CHU ", NodesWithHospitalInfo$Name, ignore.case = F)
  CHUData=as.data.frame(cbind(CHU, CHUTF))
  CHUData$CHU=as.character(CHUData$CHU)
  CorrectedCHU=ifelse(CHUData$CHUTF == T, "Centres Hospitaliers Régionaux", NA)
  CHUData=as.data.frame(CorrectedCHU, FINESS)
  
  CHRU=rep("Centres Hospitaliers Régionaux", 2063)
  CHRUTF=grepl("CHRU ", NodesWithHospitalInfo$Name, ignore.case = F)
  CHRUData=as.data.frame(cbind(CHRU, CHRUTF))
  CHRUData$CHRU=as.character(CHRUData$CHRU)
  CorrectedCHRU=ifelse(CHRUData$CHRUTF == T, "Centres Hospitaliers Régionaux", NA)
  CHRUData=as.data.frame(CorrectedCHRU, FINESS)
  
  Combined=cbind(CHData, CHICData, HLData, CHRData, CHUData, CHRUData)
  Combined=sapply(Combined, as.character)
  Combined[is.na(Combined)]=""
  CorrectedType2.Combined=paste(Combined[,1], Combined[,2], Combined[,3], Combined[,4], Combined[,5], Combined[,6], sep="")
  # CorrectedType2.Combined[which(CorrectedType2.Combined == "")] = NA
  
  CorrectedType2.Final=cbind(NodesWithHospitalInfo$Type2, CorrectedType2.Combined)
  CorrectedType2.Final[which(CorrectedType2.Final[,2]==""),]=CorrectedType2.Final[which(CorrectedType2.Final[,2]==""),1]
  
  CorrectedType2=CorrectedType2.Final[,2]
  
  
  
  return(CorrectedType2)
}
CorrectedType2=CorrectType2()
HospitalInfo=cbind(HospitalInfo, CorrectedType2)

###
FINESS=V(directed.graph_ALL)$name
InfoTable=HospitalInfo[which(HospitalInfo$FINESS %in% FINESS),c(1,3,5)]
InfoTable=cbind(FINESS,InfoTable)
table(InfoTable[,1] == InfoTable[,2])

###assign
V(directed.graph_ALL)$Type1=as.character(InfoTable[,3])
V(directed.graph_ALL)$Type2=as.character(InfoTable[,4])


cat("Save\n")
write.graph(directed.graph_ALL, "Data/Directed Prob Weighted Hospital Network with Map Communities and WDeg Class and Hosp Type.gml", format="gml")
save(directed.graph_ALL, file="Data/Directed Prob Weighted Hospital Network with Map Communities and WDeg Class and Hosp Type.RData")


######table of hospital types

load("Data/Directed Prob Weighted Hospital Network with Map Communities and WDeg Class and Hosp Type.RData")

table(V(directed.graph_ALL)$Type1[which(V(directed.graph_ALL)$InDegClassification == "Vulnerable")])
table(V(directed.graph_ALL)$Type2[which(V(directed.graph_ALL)$InDegClassification == "Vulnerable")])

table(V(directed.graph_ALL)$Type1[which(V(directed.graph_ALL)$OutDegClassification == "Infectious")])
table(V(directed.graph_ALL)$Type2[which(V(directed.graph_ALL)$OutDegClassification == "Infectious")])

table(V(directed.graph_ALL)$Type1[which(V(directed.graph_ALL)$OutDegClassification == "Infectious" & V(directed.graph_ALL)$InDegClassification == "Vulnerable")])
table(V(directed.graph_ALL)$Type2[which(V(directed.graph_ALL)$OutDegClassification == "Infectious" & V(directed.graph_ALL)$InDegClassification == "Vulnerable")])
