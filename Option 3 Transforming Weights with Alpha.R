#########################################################
####################### Option 3 ########################
### Transforming Network Edge Weights with Alpha 0.25 ###
#########################################################

# Transform edge weights using 1/Wij^0.25

#################################
#### INSTALL & LOAD PACKAGES ####

source("CommonHeader.R")


###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))


#############################
#### LOAD PMSI 2014 DATA ####

cat("Upload Hospital Transfer Network\n")
load("C:/Users/Narimane/Desktop/Hospital Network Data/directed.graph_ALL.RData")
load("C:/Users/Narimane/Desktop/Hospital Network Data/undirected.graph_ALL.RData")

cat("Upload 2014 ALL PMSI Data\n")
load("C:/Users/Narimane/Desktop/Hospital Network Data/PMSI_ALL.RData")
load("C:/Users/Narimane/Desktop/Hospital Network Data/pFlow_ALL.RData")


########################
#### GET DEPT GRAPH ####

#Get Hopital Dept
hospital_depts=substr(rownames(pFlow_ALL), 1, 2)
pFlow_ALL_Dept=pFlow_ALL
dimnames(pFlow_ALL_Dept) = list(hospital_depts, hospital_depts)

#Compress Matrix
pFlow_ALL_Dept = pFlow_ALL_Dept %*% sapply(unique(hospital_depts),"==", hospital_depts)
pFlow_ALL_Dept = t(pFlow_ALL_Dept) %*% sapply(unique(hospital_depts),"==", hospital_depts)

#Get directed graph
directed.graph_Dept = graph.adjacency(pFlow_ALL_Dept, mode="directed", diag=FALSE, weighted=TRUE)

################################
##### ALPHA Transformation #####

# Get Edge_Weights, Wij
Edge_Weights=E(directed.graph_Dept)$weight

# Transform Wij: Dij = 1/wij^0.25
Distances=1/Edge_Weights^0.25

###############################
#### Check Distance Matrix ####

cat("Replace edge weights of Department network with transformed weights")
E(directed.graph_Dept)$weight=Distances

cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
weights = E(directed.graph_Dept)$weight
algorithm = "dijkstra"

cat("Distance Matrix Between Departments\n")
Distances_Matrix=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))

##############
#### Save ####

cat("Save new network")
save(directed.graph_Dept, file="Data/Department Network (Alpha).RData")

