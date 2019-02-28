#############################
#### DATA AND PARAMETERS ####
#############################


######################
#### DEPT NETWORK ####

# cat("Original or Transformed Weights\n")
# Transformed=T
# cat("If Trans Original or Inverse Transfer Weights\n")
# InvTrans=T
# Prob=T
# 
# cat("Upload Department Contact Network\n")
# if(Transformed){
#   if(InvTrans){
#     if(Prob){
#       cat("Upload Department Network with Transformed Prob of Transfer Weights\n")
#       load("Data/Department Network (Prob of Transfers).RData")
#     }else{
#       cat("Upload Department Network with Transformed Inverse Transfer Weights\n")
#       load("Data/Department Network (Inverse Transfers).RData")
#     }
#   }else{
#     cat("Upload Department Network with Transformed Weights\n")
#     load("Data/Department Network (Transformed).RData")
#   }
# }else{
#   cat("Upload Department Contact Network without Transformations\n")
#   load("../Hospital_Network/HospitalNetwork/Data/Department Network.RData")
# }

# cat("Upload Department Network with UPDATED Transformed Prob K Weights\n")
# load("Data/Department Network (Prob K).RData")

# cat("Upload Department Network with Total / Weights\n")
# load("Data/Department Network (Total Divided by Weight).RData")

# cat("Upload Department Network with X_Rate\n")
# load("Data/Department Network (X_Rate 0.00001).RData")

# cat("Upload Department Network with 1 over 1 plus weight\n")
# load("Data/Department Network (1 over 1 plus weight).RData")
# directed.graph_Dept=directed.graph_Dept_Transformed

# cat("Upload Department Network with Edge Weights Over Stays\n")
# load("Data/Department Network (Edge Weights Over Stays).RData")

# cat("Choose Year\n")
# Year=2014
# cat("Upload Department Network with Edge Weights Over Stays\n")
# load(paste0("Data/Department Network (Edge Weights Over Stays) ", Year, ".RData"))

# cat("Upload Department Network with Edge Weights Over Stays\n")
# load("Data/Department Network (Edge Weights Over Stays) 2014 Fixed.RData")

# cat("Upload Department Network with Alpha\n")
# load("Data/Department Network (Alpha).RData")

cat("Upload Department Network with Edge Weights Over Stays\n")
load(file="Data/Department Network (Edge Weights Over Stays) 2014 Fixed WITH DEPT 08 09.RData")

####################
#### PARAMETERS ####

cat("Set Maximum Number of Days or Weeks to Test\n")
Week=F
if(Week){
  cat("Set Maximum Number of Weeks to Test\n")
  Week=8
}else{
  cat("Set Maximum Number of Days to Test\n")
  Day=30
  cat("Sliding Week(s) by Days\n")
  Sliding=T
  WeekSlide=1
}

cat("Weighted or Un-Weighted Shortest Path Calculations\n")
Weighted=T
if(Weighted){
  cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
  weights = E(directed.graph_Dept)$weight
  algorithm = "dijkstra"
}else{
  cat("Disable Weights and Algorithm to Calculate Shortest Paths\n")
  weights = NA
  algorithm = "automatic"
}

cat("Distance Matrix Between Departments\n")
Distances_Matrix=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))
NetworkDepts=colnames(Distances_Matrix)

cat("Number of Simulations\n")
Nruns=500

cat("Mechanism or Class\n")
Mechanism=T

cat("Shared Department Between Incident Episodes and Candidates?\n")
SharedDepartment=T

cat("Reshuffled or Reassigned Departments?\n")
Reshuffled=T

##########################
#### STEP 1: CPE DATA ####

cat("Choose Year\n")
Year=2012

cat("Choose start date\n")
startDate=paste0(as.character(Year),"-01-01")

cat("Choose end date\n")
endDate=paste0(as.character(Year+1),"-01-01")

cat("Get CPE Data with Mechanism and Class Info\n")
data=getCPEData()

########################
#### WORKING FOLDER ####

# folder=paste0("1000 Permutations/", as.character(Year), " Results")
# folder="2012-2015 All Results"
folder=paste0("500 Permutations/", as.character(Year), " Results")
# folder=paste0("500 Alpha/", as.character(Year), " Results")
