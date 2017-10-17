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
# load(paste0(writingDir,"50 Permutations (Reshuffled Shared Department 2015 Data Sliding Week)/MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding.RData"))

#######################
#### SELECT WINDOW ####

cat("From plots, window between Day X and Day Y showed biggest difference")
Day=19

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

cat("Get Minimum Distance Between Episode and 1 Potential Infector\n")
MinDistanceLocation=lapply(CandidateTransmitters_Departments_MinDistances, function(x) which.min(x))

PotentialCandidates=foreach(i=1:length(CandidateTransmitters)) %do% {
  if(!is.na(CandidateTransmitters_Departments_MinDistances[[i]])){
    CandidateTransmitters_Subset=CandidateTransmitters[[i]]
    MinDistanceLocation_Subset=MinDistanceLocation[[i]]
    PotentialCandidate=CandidateTransmitters_Subset[MinDistanceLocation_Subset,]
  }
}

###################
#### GET PAIRS ####

Episode="Episode"
Pairs_Episodes=foreach(i=1:length(CandidateTransmitters), .combine = 'rbind') %do% {
  if(is.null(PotentialCandidates[[i]])){
    None=c(NA, NA)
  }else{
    Pairs_Episode=c(PotentialCandidates[[i]]$Episode, data[i,Episode])}
}

Pairs_Episodes=Pairs_Episodes[complete.cases(Pairs_Episodes),]
Pairs_Episodes=Pairs_Episodes[order(Pairs_Episodes[,1]),]
Pairs_Episodes=cbind(as.character(Pairs_Episodes[,1]), as.character(Pairs_Episodes[,2]))

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

DataSubset=data[data$Episode %in% names_int, c("Episode", "Mechanism", "Department")]
DataSubset=DataSubset[match(names_int, DataSubset$Episode),]

V(TreeGraph)$Mechanism=DataSubset$Mechanism
V(TreeGraph)$Department=DataSubset$Department

write.graph(TreeGraph, file=paste0(writingDir,"2015 CPE Pairs Tree (with Attributes).gml"), format="gml")




