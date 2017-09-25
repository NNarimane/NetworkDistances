########################################################################
##### Transforming Network Edge Weights to Infection Probabilities #####
########################################################################

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

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("CPETransmissionChains/Generation Time Sensitivity Analysis Functions.R", 
       local = FALSE, verbose = getOption("verbose"))

###########################
#### LOAD DEPT NETWORK ####

cat("Deptartment Network or Hospital Network\n")
Department=F
if(Department){
  cat("Upload Department Contact Network\n")
  load("../Hospital_Network/HospitalNetwork/Data/Department Network.RData")
  cat("Upload Department Contact Network pFlow\n")
  load("C:/Users/Narimane/Desktop/Hospital Data/Created Files 23.01.17/pFlow_ALL_Dept.RData")
}else{
  cat("Upload Hospital Transfer Network\n")
  load("C:/Users/Narimane/Desktop/Hospital Network Data/directed.graph_ALL.RData")
  load("C:/Users/Narimane/Desktop/Hospital Network Data/undirected.graph_ALL.RData")
}

#############################
#### LOAD PMSI 2014 DATA ####

cat("Upload 2014 ALL PMSI Data\n")
load("C:/Users/Narimane/Desktop/Hospital Network Data/PMSI_ALL.RData")
load("C:/Users/Narimane/Desktop/Hospital Network Data/pFlow_ALL.RData")

################################################
#### Probabilty of Infection Function, Pij #####

#### Define Parameters ####

# Pij = 1 - dbinom(0,nij,K)

# Pij: defined as the probabilty that at least one infected patient from Hospital i
# infected a patient in Hospital j

# K: defined as K=P(T)*P(I)
# T: defined as the annual hospital transfer rate = total number of transfers in Y / total number of patients in Y
# Y: year we have data, set as 2014 
# I: defined as the number of CPE infectes that occured in Y / total number of patients in Y

# nij: defined as the number of transfers going from Hospital i to Hospital j (edge weight in a directed graph)

# dbinom: defined as the density binomial distribution 
# 0 is the probabilty of havaing zero infected and transferred
# K is our constant/prob of infection
# nij is our size or number of trials/transfers
# 1 - the distribution if having none so the final Pij is the prob of having at least 1 or more

#### Calculate Parameters ####

# T, annual hospital transfer rate
NumberTransfers=sum(pFlow_ALL)
NumberPatients=length(unique(PMSI_ALL$ID))
T_rate=NumberTransfers/NumberPatients

# I, CPE infection rate
NumberInfections=sum(getData("2014-01-01", "2014-12-31")$TotalCases)
I_rate=NumberInfections/NumberPatients

# K, probabilty
K=T_rate*I_rate

if(Department){
  # nij, list of edge weights (inter-Department transfers)
  Edge_Weights=as.list(c(E(directed.graph_Dept)$weight))
}else{
  # nij, list of edge weights (inter-Hospital transfers)
  Edge_Weights=as.list(c(E(directed.graph_ALL)$weight))
}

#### Calculate Pij ####

# Pij = 1 - dbinom(0,K,nij)
Probabilities=foreach(i=1:length(Edge_Weights)) %do% (1 - dbinom(0, size = Edge_Weights[[i]], prob = K))

###########################
#### Transforming Pij #####

# We must take the -log(Pij) in order for the dijkstra algorithm to correctly choose the "shortest path"
# We define "shortest path" as the path with the highest infection probability pij 
# Since -log(Pij) with higher infection Pij, the smaller the -log so the distance is storter

Probabilities_Transformed=foreach(i=1:length(Probabilities)) %do% -log(Probabilities[[i]])

###############################################
#### Edge Weight Table of Transformations #####

Edge_Weights_Table=cbind(unlist(Edge_Weights), unlist(Probabilities), unlist(Probabilities_Transformed))

######################################################################
#### Replace original edge weights with transformed edge weights #####

# if(Department){
#   cat("Replace edge weights of Department network with transformed weights")
#   # E(directed.graph_Dept)$weight=unlist(Probabilities_Transformed)
#   
#   cat("Save new network")
#   # save(directed.graph_Dept, file="Data/Department Network (Transformed Weights).RData")
# }else{
#   cat("Replace edge weights of Hospital network with transformed weights")
#   E(directed.graph_ALL)$weight=unlist(Probabilities_Transformed)
#   
#   cat("Save new network")
#   save(directed.graph_ALL, file="Data/Hospital Network (Transformed Weights).RData")
#   
#   cat("Replace edge weights of Hospital network with transformed weights")
#   Edge_Weights=as.list(c(E(undirected.graph_ALL)$weight))
#   Probabilities=foreach(i=1:length(Edge_Weights)) %do% (1 - dbinom(0, size = Edge_Weights[[i]], prob = K))
#   Probabilities_Transformed=foreach(i=1:length(Probabilities)) %do% -log(Probabilities[[i]])
#   Edge_Weights_Table=cbind(unlist(Edge_Weights), unlist(Probabilities), unlist(Probabilities_Transformed))
#   E(undirected.graph_ALL)$weight=unlist(Probabilities_Transformed)
#   
#   cat("Save new network")
#   save(undirected.graph_ALL, file="Data/Hospital Undirected Network (Transformed Weights).RData")
#   
# }


##########################################
#### Comparing the Distance Matrices #####

# cat("Distance Matrix Between Departments\n")
# Distances_Matrix_Original=as.data.frame(distances(directed.graph_Dept, weights = E(directed.graph_Dept)$weight, algorithm = "dijkstra"))
# 
# cat("Distance Matrix Between Departments with Transformed Weights\n")
# E(directed.graph_Dept)$weight=Probabilities_Transformed
# Distances_Matrix_Transformed=as.data.frame(distances(directed.graph_Dept, weights = E(directed.graph_Dept)$weight, algorithm = "dijkstra"))
# 
# #Comparing to pFlow
# #Remove intra-hospital transfers
# diag(pFlow_ALL_Dept) <- 0
# 
# #Visually it seems like direct transfers with larger amounts of patients have a shorter transformed distance



##################################################################
#### Exporting Networks for Gephi (Weights) to get real prob #####

if(Department){
  cat("Replace edge weights of Department network with transformed weights")
  # E(directed.graph_Dept)$weight=unlist(Probabilities)
  
  cat("Save new network")
  # save(directed.graph_Dept, file="Data/Department Network (Prob Weights).RData")
}else{
  cat("Replace edge weights of Hospital network with transformed weights")
  E(directed.graph_ALL)$weight=unlist(Probabilities)
  
  cat("Save new network")
  save(directed.graph_ALL, file="Data/Hospital Network (Prob Weights).RData")
  
  cat("Replace edge weights of Hospital network with transformed weights")
  Edge_Weights=as.list(c(E(undirected.graph_ALL)$weight))
  Probabilities=foreach(i=1:length(Edge_Weights)) %do% (1 - dbinom(0, size = Edge_Weights[[i]], prob = K))
  Probabilities_Transformed=foreach(i=1:length(Probabilities)) %do% -log(Probabilities[[i]])
  Edge_Weights_Table=cbind(unlist(Edge_Weights), unlist(Probabilities), unlist(Probabilities_Transformed))
  E(undirected.graph_ALL)$weight=unlist(Probabilities)
  
  cat("Save new network")
  save(undirected.graph_ALL, file="Data/Hospital Undirected Network (Prob Weights).RData")
  
}

