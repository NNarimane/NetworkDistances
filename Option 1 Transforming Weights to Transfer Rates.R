#########################################################
####################### Option 1 ########################
## Transforming Network Edge Weights to Transfer Rates ##
#########################################################

# Transform edge weights into true transfer rates
# Where size is # of stays per hospital (per X weeks)
# And probabilty of transfer (per stay per X weeks)

# i.e. 1-dbinom(0, size = Stay[[i]] * Weeks/52, prob = Edge_Weight[[i]]/Stay[[i]] * Weeks/52)

#################################
#### INSTALL & LOAD PACKAGES ####

source("CommonHeader.R")


###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))


#############################
#### LOAD PMSI 2014 DATA ####

# cat("Upload Hospital Transfer Network\n")
# load("C:/Users/Narimane/Desktop/Hospital Network Data/directed.graph_ALL.RData")
# load("C:/Users/Narimane/Desktop/Hospital Network Data/undirected.graph_ALL.RData")
# 
# cat("Upload 2014 ALL PMSI Data\n")
# load("C:/Users/Narimane/Desktop/Hospital Network Data/PMSI_ALL.RData")
# load("C:/Users/Narimane/Desktop/Hospital Network Data/pFlow_ALL.RData")

# Load fixed graph *sent in 2019*
load(paste0(getwd(),"/Data/Fixed/matrix_bydep.RData"))
load(paste0(getwd(),"/Data/Fixed/matrix_all.RData"))

########################
#### GET DEPT GRAPH ####

# #Get Hopital Dept
# hospital_depts=substr(rownames(pFlow_ALL), 1, 2)
# pFlow_ALL_Dept=pFlow_ALL
# dimnames(pFlow_ALL_Dept) = list(hospital_depts, hospital_depts)
# 
# #Compress Matrix
# pFlow_ALL_Dept = pFlow_ALL_Dept %*% sapply(unique(hospital_depts),"==", hospital_depts)
# pFlow_ALL_Dept = t(pFlow_ALL_Dept) %*% sapply(unique(hospital_depts),"==", hospital_depts)
# 
# #Get directed graph
# directed.graph_Dept = graph.adjacency(pFlow_ALL_Dept, mode="directed", diag=FALSE, weighted=TRUE)

#Get directed graph
directed.graph_Dept = graph.adjacency(matrix_bydep, mode="directed", diag=FALSE, weighted=TRUE)
directed.graph_ALL = graph.adjacency(matrix_all, mode="directed", diag=FALSE, weighted=TRUE)

###################
#### Stay Data ####

# 2012
getStayData_2012=function(){
  All_Stays=read.csv(file = paste0("C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/Entrees_2012.csv"), sep=",")
  # Fix FINESS
  All_Stays$FINESS=str_pad(All_Stays$FINESS, 9, pad = "0")
  # Merge with network hospitals
  NetworkHospitals=data.frame(FINESS=V(directed.graph_ALL)$name, stringsAsFactors = F)
  NetworkHospitals_Stays=merge(NetworkHospitals, All_Stays, by="FINESS", all.x=T)
  # Remove NAs
  NetworkHospitals_Stays_NAsRemoved=NetworkHospitals_Stays[!is.na(NetworkHospitals_Stays$Entrees),]
  # Check for duplicates
  table(duplicated(NetworkHospitals_Stays_NAsRemoved$FINESS))
  # By Department
  Stays_Dept=data.frame(Dept=substr(NetworkHospitals_Stays_NAsRemoved$FINESS, 1, 2), Stays=NetworkHospitals_Stays_NAsRemoved$Entrees, stringsAsFactors = F)
  Stays_Dept=aggregate(Stays_Dept[,2], by=list(Stays_Dept[,1]), FUN=sum)
  colnames(Stays_Dept)=c("Dept", "AnnualStays")
  sum(Stays_Dept$AnnualStays) #11911029
  
  #### Get Edge Weights ####
  
  # Hij, list of edge weights (inter-Department transfers)
  Edge_Weights=as.list(c(E(directed.graph_Dept)$weight))
  
  # Merge Hi stays with Hi edge weight
  Edges=data.frame(Source=get.edgelist(directed.graph_Dept)[,1], 
                   Target=get.edgelist(directed.graph_Dept)[,2], 
                   Edge_Weights=unlist(Edge_Weights), stringsAsFactors = F)
  
  Edges_Stays=merge(Edges, Stays_Dept, by.x="Source", by.y="Dept", all.x=T)
  
  return(Edges_Stays)
}
Edges_Stays=getStayData_2012()


# Function for 2013-2015
getStayData=function(Year){
  #MCO
  MCO=read.csv(file = paste0(getwd(),"/Data/MCO_", Year, ".csv"), sep=";")
  
  SEJHC_MCO1=as.data.frame(cbind(as.character(MCO$FI), MCO$SEJHC_MCO), stringsAsFactors = F)
  # Fix 2A & 2B
  SEJHC_MCO1$V1[which(grepl("2A", SEJHC_MCO1$V1))]=sub("2A", "20", SEJHC_MCO1$V1[which(grepl("2A", SEJHC_MCO1$V1))])
  SEJHC_MCO1$V1[which(grepl("2B", SEJHC_MCO1$V1))]=sub("2B", "20", SEJHC_MCO1$V1[which(grepl("2B", SEJHC_MCO1$V1))])
  
  SEJHC_MCO1$V2=as.numeric(SEJHC_MCO1$V2)
  table(duplicated(SEJHC_MCO1[,1]))
  SEJHC_MCO1_Sum=aggregate(SEJHC_MCO1[,2], by=list(SEJHC_MCO1[,1]), FUN=sum, na.rm=TRUE)
  
  SEJHC_MCO2=as.data.frame(cbind(as.character(MCO$FI_EJ), MCO$SEJHC_MCO), stringsAsFactors = F)
  # Fix 2A & 2B
  SEJHC_MCO2$V1[which(grepl("2A", SEJHC_MCO2$V1))]=sub("2A", "20", SEJHC_MCO2$V1[which(grepl("2A", SEJHC_MCO2$V1))])
  SEJHC_MCO2$V1[which(grepl("2B", SEJHC_MCO2$V1))]=sub("2B", "20", SEJHC_MCO2$V1[which(grepl("2B", SEJHC_MCO2$V1))])
  
  SEJHC_MCO2$V2=as.numeric(SEJHC_MCO2$V2)
  table(duplicated(SEJHC_MCO2[,1]))
  SEJHC_MCO2_Sum=aggregate(SEJHC_MCO2[,2], by=list(SEJHC_MCO2[,1]), FUN=sum, na.rm=TRUE)
  
  #SSR
  SSR=read.csv(file = paste0(getwd(),"/Data/SSR_", Year, ".csv"), sep=";")
  
  SEJHC_SSR1=as.data.frame(cbind(as.character(SSR$FI), SSR$SEJHC), stringsAsFactors = F)
  # Fix 2A & 2B
  SEJHC_SSR1$V1[which(grepl("2A", SEJHC_SSR1$V1))]=sub("2A", "20", SEJHC_SSR1$V1[which(grepl("2A", SEJHC_SSR1$V1))])
  SEJHC_SSR1$V1[which(grepl("2B", SEJHC_SSR1$V1))]=sub("2B", "20", SEJHC_SSR1$V1[which(grepl("2B", SEJHC_SSR1$V1))])
  
  SEJHC_SSR1$V2=as.numeric(SEJHC_SSR1$V2)
  table(duplicated(SEJHC_SSR1[,1]))
  SEJHC_SSR1_Sum=aggregate(SEJHC_SSR1[,2], by=list(SEJHC_SSR1[,1]), FUN=sum, na.rm=TRUE)
  
  SEJHC_SSR2=as.data.frame(cbind(as.character(SSR$FI_EJ), SSR$SEJHC), stringsAsFactors = F)
  # Fix 2A & 2B
  SEJHC_SSR2$V1[which(grepl("2A", SEJHC_SSR2$V1))]=sub("2A", "20", SEJHC_SSR2$V1[which(grepl("2A", SEJHC_SSR2$V1))])
  SEJHC_SSR2$V1[which(grepl("2B", SEJHC_SSR2$V1))]=sub("2B", "20", SEJHC_SSR2$V1[which(grepl("2B", SEJHC_SSR2$V1))])
  
  SEJHC_SSR2$V2=as.numeric(SEJHC_SSR2$V2)
  table(duplicated(SEJHC_SSR2[,1]))
  SEJHC_SSR2_Sum=aggregate(SEJHC_SSR2[,2], by=list(SEJHC_SSR2[,1]), FUN=sum, na.rm=TRUE)
  
  #Combine
  All_Stays_1=as.data.frame(rbind(SEJHC_MCO1_Sum, SEJHC_SSR1_Sum))
  table(duplicated(All_Stays_1[,1]))
  All_Stays_1_Sum=aggregate(All_Stays_1[,2], by=list(All_Stays_1[,1]), FUN=sum, na.rm=TRUE)
  colnames(All_Stays_1_Sum)=c("FINESS", "Stays")
  
  All_Stays_2=as.data.frame(rbind(SEJHC_MCO2_Sum, SEJHC_SSR2_Sum))
  table(duplicated(All_Stays_2[,1]))
  All_Stays_2_Sum=aggregate(All_Stays_2[,2], by=list(All_Stays_2[,1]), FUN=sum, na.rm=TRUE)
  colnames(All_Stays_2_Sum)=c("FINESS", "Stays")
  
  ########################################
  # Only hospital network hospital stays #
  
  NetworkHospitals=data.frame(FINESS=V(directed.graph_ALL)$name, stringsAsFactors = F)
  # Fix FINESS
  NetworkHospitals$FINESS[which(NetworkHospitals$FINESS == "200200145")] = "200000145"
  
  NetworkHospitals_Stays_1=merge(NetworkHospitals, All_Stays_1_Sum, by="FINESS", all.x=T)
  NetworkHospitals_Stays_2=merge(NetworkHospitals, All_Stays_2_Sum, by="FINESS", all.x=T)
  # Remove NAs
  NetworkHospitals_Stays_1_NAsRemoved=NetworkHospitals_Stays_1[!is.na(NetworkHospitals_Stays_1$Stays),]
  NetworkHospitals_Stays_2_NAsRemoved=NetworkHospitals_Stays_2[!is.na(NetworkHospitals_Stays_2$Stays),]
  # Check for duplicates
  table(NetworkHospitals_Stays_1_NAsRemoved$FINESS %in% NetworkHospitals_Stays_2_NAsRemoved$FINESS)
  # Rbind
  Stays=rbind(NetworkHospitals_Stays_1_NAsRemoved, NetworkHospitals_Stays_2_NAsRemoved)
  
  # By Department
  Stays_Dept=data.frame(Dept=substr(Stays$FINESS, 1, 2), Stays=Stays$Stays, stringsAsFactors = F)
  Stays_Dept=aggregate(Stays_Dept[,2], by=list(Stays_Dept[,1]), FUN=sum)
  colnames(Stays_Dept)=c("Dept", "AnnualStays")
  
  ##########################
  #### Get Edge Weights ####
  
  # Hij, list of edge weights (inter-Department transfers)
  Edge_Weights=as.list(c(E(directed.graph_Dept)$weight))
  
  # Merge Hi stays with Hi edge weight
  Edges=data.frame(Source=get.edgelist(directed.graph_Dept)[,1], 
                   Target=get.edgelist(directed.graph_Dept)[,2], 
                   Edge_Weights=unlist(Edge_Weights), stringsAsFactors = F)
  
  Edges_Stays=merge(Edges, Stays_Dept, by.x="Source", by.y="Dept", all.x=T)
  # Edges_Stays$WeeklyStays=Edges_Stays$AnnualStays/52
  # Edges_Stays$TwoWeekStays=Edges_Stays$WeeklyStays*2
  # Edges_Stays$ThreeWeekStays=Edges_Stays$WeeklyStays*3
  # Edges_Stays$MonthlyStays=Edges_Stays$AnnualStays/12
  
  # Round
  # Edges_Stays[,3:8]=round(Edges_Stays[3:8], digits = 0)
  
  return(Edges_Stays)
}

# 2013
Edges_Stays=getStayData("2013")

# 2014
Edges_Stays=getStayData("2014")

# 2015
Edges_Stays=getStayData("2015")


#############################
#### Transformed Weights ####

New_Weights = foreach(i=1:nrow(Edges_Stays)) %do% -log(Edges_Stays$Edge_Weights[i]/Edges_Stays$AnnualStays[i])

hist(unlist(New_Weights))
range(unlist(New_Weights), na.rm = T)

New_Weights_Untransformed = foreach(i=1:nrow(Edges_Stays)) %do% (Edges_Stays$Edge_Weights[i]/Edges_Stays$AnnualStays[i])
hist(unlist(New_Weights_Untransformed))
range(unlist(New_Weights_Untransformed), na.rm = T)
Edges_Stays$EdgeWeightOverStay=unlist(New_Weights_Untransformed)

Edges_Stays$TransWeights=unlist(New_Weights)

###############################
#### Check Distance Matrix ####

cat("Replace edge weights of Department network with transformed weights")
E(directed.graph_Dept)$weight=unlist(New_Weights)

cat("Set Weights and Algorithm to Calculate Weighted Shortest Paths\n")
weights = E(directed.graph_Dept)$weight
algorithm = "dijkstra"

cat("Distance Matrix Between Departments\n")
Distances_Matrix=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))

##############
#### Save ####

cat("Save new network")
save(directed.graph_Dept, file="Data/Department Network (Edge Weights Over Stays) 2014 Fixed WITH DEPT 08 09.RData")









#######################
#### Probabilities ####
# 
# Annual_Probabilities = foreach(i=1:nrow(Stays_2014)) %do% (1-dbinom(0, size = Stays_2014[i,"AnnualStays"], prob = Stays_2014[i,"Edge_Weights"]/Stays_2014[i,"AnnualStays"]))
# hist(unlist(Annual_Probabilities))
# range(Annual_Probabilities, na.rm = T)
# table(unlist(Annual_Probabilities))
# 
# Annual_Probabilities_Transformed =foreach(i=1:length(Annual_Probabilities)) %do% -log(Annual_Probabilities[[i]])
# hist(unlist(Annual_Probabilities_Transformed))
# range(Annual_Probabilities_Transformed, na.rm = T)
# table(unlist(Annual_Probabilities_Transformed))
# 
# ThreeWeek_Probabilities = foreach(i=1:nrow(Stays_2014)) %do% (1-dbinom(0, size = Stays_2014[i,"ThreeWeekStays"], prob = Stays_2014[i,"Edge_Weights"]/Stays_2014[i,"ThreeWeekStays"]))
# hist(unlist(ThreeWeek_Probabilities))
# range(ThreeWeek_Probabilities, na.rm = T)
# table(unlist(ThreeWeek_Probabilities))
# 
# ThreeWeek_Probabilities_Transformed =foreach(i=1:length(ThreeWeek_Probabilities)) %do% -log(ThreeWeek_Probabilities[[i]])
# hist(unlist(ThreeWeek_Probabilities_Transformed))
# range(ThreeWeek_Probabilities_Transformed, na.rm = T)
# table(unlist(ThreeWeek_Probabilities_Transformed))
# 
# 
