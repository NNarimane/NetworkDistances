########################################################################
##### Transforming Network Edge Weights to Infection Probabilities #####
########################################################################

#################################
#### INSTALL & LOAD PACKAGES ####

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))


###########################
#### LOAD DEPT NETWORK ####

cat("Deptartment Network or Hospital Network\n")
Department=T
if(Department){
  cat("Upload Department Contact Network\n")
  load("Data/Department Network.RData")
  cat("Upload Department Contact Network pFlow\n")
  load("Data/pFlow_ALL_Dept.RData")
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

#### Defined Parameters ####

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
NumberInfections=sum(getData("2014-01-01", "2015-01-01")$TotalCases)
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

#### Calculate Pij as probability of transfer only ####

# Probability that there will be at least 1 transfer per edge given T_rate
Probabilities=foreach(i=1:length(Edge_Weights)) %do% (1 - dbinom(0, size = Edge_Weights[[i]], prob = T_rate))

###########################
#### Transforming Pij #####

# We must take the -log(Pij) in order for the dijkstra algorithm to correctly choose the "shortest path"
# We define "shortest path" as the path with the highest infection probability pij 
# Since -log(Pij) with higher infection Pij, the smaller the -log so the distance is storter

Probabilities_Transformed=foreach(i=1:length(Probabilities)) %do% -log(Probabilities[[i]])

###############################################
#### Edge Weight Table of Transformations #####

Edge_Weights_Table=cbind(unlist(Edge_Weights), unlist(Probabilities), unlist(Probabilities_Transformed))

Edge_Weights_Table=as.data.frame(cbind(get.edgelist(directed.graph_Dept), unlist(Edge_Weights), unlist(Probabilities), unlist(Probabilities_Transformed)))
colnames(Edge_Weights_Table)=c("SourceDept", "TargetDept", "Transfers", "Prob T", "LogTrans")
write.table(Edge_Weights_Table, file=paste0(writingDir, "Prob K Edgelist.csv"))

InverseTransfers=lapply(Edge_Weights, function(x) 1/x)
Edge_Weights_Table=as.data.frame(cbind(get.edgelist(directed.graph_Dept), unlist(Edge_Weights), unlist(InverseTransfers)))
colnames(Edge_Weights_Table)=c("SourceDept", "TargetDept", "Transfers", "Inverse")
write.table(Edge_Weights_Table, file=paste0(writingDir, "Inverse Transfers Edgelist.csv"))

InverseLogTransfers=lapply(Edge_Weights, function(x) 1/log(x))
Edge_Weights_Table=as.data.frame(cbind(get.edgelist(directed.graph_Dept), unlist(Edge_Weights), unlist(InverseLogTransfers)))
colnames(Edge_Weights_Table)=c("SourceDept", "TargetDept", "Transfers", "InverseLog")
write.table(Edge_Weights_Table, file=paste0(writingDir, "Inverse Log Transfers Edgelist.csv"))

ExpTransfers=lapply(Edge_Weights, function(x) exp(x))
Edge_Weights_Table=as.data.frame(cbind(get.edgelist(directed.graph_Dept), unlist(Edge_Weights), unlist(InverseLogTransfers)))
colnames(Edge_Weights_Table)=c("SourceDept", "TargetDept", "Transfers", "Exp")
write.table(Edge_Weights_Table, file=paste0(writingDir, "Inverse Log Transfers Edgelist.csv"))


###########################################################
#### Edge Weight Transformed as Inverse of # Transfers ####

# InverseTransfers=lapply(Edge_Weights, function(x) 1/x)
# # Edge_Weights_Table=cbind(unlist(Edge_Weights), InverseTransfers)
# 
# cat("Replace edge weights of Department network with inverse of number of transfers")
# E(directed.graph_Dept)$weight=unlist(InverseTransfers)
# # edgelist=cbind(get.edgelist(directed.graph_Dept), E(directed.graph_Dept)$weight)
# # Distances_Matrix=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))
# # min(Distances_Matrix[Distances_Matrix > 0])
# # Distances_Matrix[which.min(Distances_Matrix[Distances_Matrix > 0])]
# 
# cat("Save new network")
# save(directed.graph_Dept, file="Data/Department Network (Inverse Transfers).RData")

######################################################
#### Edge Weight Transformed as Prob of Transfers ####

cat("Replace edge weights of Department network with inverse of number of transfers")
E(directed.graph_Dept)$weight=unlist(InverseTransfers)
# edgelist=cbind(get.edgelist(directed.graph_Dept), E(directed.graph_Dept)$weight)
# weights = E(directed.graph_Dept)$weight
algorithm = "dijkstra"
Distances_Matrix=as.data.frame(distances(directed.graph_Dept, mode="in", weights = E(directed.graph_Dept)$weight, algorithm = "dijkstra"))
min(Distances_Matrix[Distances_Matrix > 0])
# Distances_Matrix[which.min(Distances_Matrix[Distances_Matrix > 0])]

cat("Save new network")
save(directed.graph_Dept, file="Data/Department Network (Prob K).RData")

######################################################################
#### Replace original edge weights with transformed edge weights #####

if(Department){
  cat("Replace edge weights of Department network with transformed weights")
  # E(directed.graph_Dept)$weight=unlist(Probabilities_Transformed)

  cat("Save new network")
  # save(directed.graph_Dept, file="Data/Department Network (Transformed Weights).RData")
}else{
  cat("Replace edge weights of Hospital network with transformed weights")
  E(directed.graph_ALL)$weight=unlist(Probabilities_Transformed)

  cat("Save new network")
  save(directed.graph_ALL, file="Data/Hospital Network (Transformed Weights).RData")

  cat("Replace edge weights of Hospital network with transformed weights")
  Edge_Weights=as.list(c(E(undirected.graph_ALL)$weight))
  Probabilities=foreach(i=1:length(Edge_Weights)) %do% (1 - dbinom(0, size = Edge_Weights[[i]], prob = K))
  Probabilities_Transformed=foreach(i=1:length(Probabilities)) %do% -log(Probabilities[[i]])
  Edge_Weights_Table=cbind(unlist(Edge_Weights), unlist(Probabilities), unlist(Probabilities_Transformed))
  E(undirected.graph_ALL)$weight=unlist(Probabilities_Transformed)

  cat("Save new network")
  save(undirected.graph_ALL, file="Data/Hospital Undirected Network (Transformed Weights).RData")

}


##########################################
#### Comparing the Distance Matrices #####

cat("Distance Matrix Between Departments\n")
Distances_Matrix_Original=as.data.frame(distances(directed.graph_Dept, weights = E(directed.graph_Dept)$weight, algorithm = "dijkstra"))

cat("Distance Matrix Between Departments with Transformed Weights\n")
E(directed.graph_Dept)$weight=Probabilities_Transformed
Distances_Matrix_Transformed=as.data.frame(distances(directed.graph_Dept, weights = E(directed.graph_Dept)$weight, algorithm = "dijkstra"))

#Comparing to pFlow
#Remove intra-hospital transfers
diag(pFlow_ALL_Dept) <- 0

#Visually it seems like direct transfers with larger amounts of patients have a shorter transformed distance





# ###########################################
# ### Transform Matrices to Probabilities ###
# 
# load("C:/Users/Narimane/Desktop/Hospital Network Data/pFlow_ALL.RData")
# pFlow_Hospital_Transfers=pFlow_ALL
# save(pFlow_Hospital_Transfers, file="C:/Users/Narimane/Dropbox/Hospital Model/CPENetSpread/pFlow_Hospital_Transfers.RData")
# 
# load("Data/pFlow_ALL_Dept.RData")
# pFlow_Dept_Transfers=pFlow_ALL_Dept
# save(pFlow_Dept_Transfers, file="C:/Users/Narimane/Dropbox/Hospital Model/CPENetSpread/pFlow_Dept_Transfers.RData")
# 
# # Transform Hospital Matrix to Probabilities of Transfer
# pFlow_Hospital_Transfers_Probabilities=(1 - dbinom(0, size = pFlow_ALL[,], prob = T_rate))
# save(pFlow_Hospital_Transfers_Probabilities, file="C:/Users/Narimane/Dropbox/Hospital Model/CPENetSpread/pFlow_Hospital_Transfers_Probabilities.RData")
# 
# # Transform Dept Matrix to Probabilities of Transfer
# pFlow_Dept_Transfers_Probabilities=(1 - dbinom(0, size = pFlow_ALL_Dept[,], prob = T_rate))
# save(pFlow_Dept_Transfers_Probabilities, file="C:/Users/Narimane/Dropbox/Hospital Model/CPENetSpread/pFlow_Dept_Transfers_Probabilities.RData")





