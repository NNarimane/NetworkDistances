#################################################################
########################### Option 1 ############################
## Transforming Network Edge Weights to Transfer Probabilities ##
#################################################################

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


###########################
#### LOAD DEPT NETWORK ####

cat("Deptartment Network or Hospital Network\n")
Department=F
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


########################
#### GET DEPT GRAPH ####

#Get Hopital Dept
hospital_depts=substr(rownames(pFlow_ALL), 1, 2)
pFlow_ALL_Dept=pFlow_ALL
dimnames(pFlow_ALL_Dept) = list(hospital_depts, hospital_depts)

#Compress Matrix
pFlow_ALL_Dept = pFlow_ALL_Dept %*% sapply(unique(hospital_depts),"==", hospital_depts)
pFlow_ALL_Dept = t(pFlow_ALL_Dept) %*% sapply(unique(hospital_depts),"==", hospital_depts)


##########################
#### Upload Stay Data ####

# 2014
#MCO
MCO_2014=read.csv(file = "C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/MCO_2014.csv", sep=";")
SEJHC_MCO1=as.data.frame(cbind(as.character(MCO_2014$FI), MCO_2014$SEJHC_MCO), stringsAsFactors = F)
SEJHC_MCO1$V2=as.numeric(SEJHC_MCO1$V2)
table(duplicated(SEJHC_MCO1[,1]))
SEJHC_MCO1_Sum=aggregate(SEJHC_MCO1[,2], by=list(SEJHC_MCO1[,1]), FUN=sum, na.rm=TRUE)

SEJHC_MCO2=as.data.frame(cbind(as.character(MCO_2014$FI_EJ), MCO_2014$SEJHC_MCO), stringsAsFactors = F)
SEJHC_MCO2$V2=as.numeric(SEJHC_MCO2$V2)
table(duplicated(SEJHC_MCO2[,1]))
SEJHC_MCO2_Sum=aggregate(SEJHC_MCO2[,2], by=list(SEJHC_MCO2[,1]), FUN=sum, na.rm=TRUE)

#SSR
SSR_2014=read.csv(file = "C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/SSR_2014.csv", sep=";")
SEJHC_SSR1=as.data.frame(cbind(as.character(SSR_2014$FI), SSR_2014$SEJHC), stringsAsFactors = F)
SEJHC_SSR1$V2=as.numeric(SEJHC_SSR1$V2)
table(duplicated(SEJHC_SSR1[,1]))
SEJHC_SSR1_Sum=aggregate(SEJHC_SSR1[,2], by=list(SEJHC_SSR1[,1]), FUN=sum, na.rm=TRUE)

SEJHC_SSR2=as.data.frame(cbind(as.character(SSR_2014$FI_EJ), SSR_2014$SEJHC), stringsAsFactors = F)
SEJHC_SSR2$V2=as.numeric(SEJHC_SSR2$V2)
table(duplicated(SEJHC_SSR2[,1]))
SEJHC_SSR2_Sum=aggregate(SEJHC_SSR2[,2], by=list(SEJHC_SSR2[,1]), FUN=sum, na.rm=TRUE)

#Combine
All_Stays_2014_1=as.data.frame(rbind(SEJHC_MCO1_Sum, SEJHC_SSR1_Sum))
table(duplicated(All_Stays_2014_1[,1]))
All_Stays_2014_1_Sum=aggregate(All_Stays_2014_1[,2], by=list(All_Stays_2014_1[,1]), FUN=sum, na.rm=TRUE)
colnames(All_Stays_2014_1_Sum)=c("FINESS", "Stays")

All_Stays_2014_2=as.data.frame(rbind(SEJHC_MCO2_Sum, SEJHC_SSR2_Sum))
table(duplicated(All_Stays_2014_2[,1]))
All_Stays_2014_2_Sum=aggregate(All_Stays_2014_2[,2], by=list(All_Stays_2014_2[,1]), FUN=sum, na.rm=TRUE)
colnames(All_Stays_2014_2_Sum)=c("FINESS", "Stays")

########################################
# Only hospital network hospital stays #
NetworkHospitals=data.frame(FINESS=V(directed.graph_ALL)$name, stringsAsFactors = F)
NetworkHospitals_Stays_2014_1=merge(NetworkHospitals, All_Stays_2014_1_Sum, by="FINESS", all.x=T)
NetworkHospitals_Stays_2014_2=merge(NetworkHospitals, All_Stays_2014_2_Sum, by="FINESS", all.x=T)
# Remove NAs
NetworkHospitals_Stays_2014_1_NAsRemoved=NetworkHospitals_Stays_2014_1[!is.na(NetworkHospitals_Stays_2014_1$Stays),]
NetworkHospitals_Stays_2014_2_NAsRemoved=NetworkHospitals_Stays_2014_2[!is.na(NetworkHospitals_Stays_2014_2$Stays),]
# Check for duplicates
table(NetworkHospitals_Stays_2014_1_NAsRemoved$FINESS %in% NetworkHospitals_Stays_2014_2_NAsRemoved$FINESS)
# Rbind
Stays=rbind(NetworkHospitals_Stays_2014_1_NAsRemoved, NetworkHospitals_Stays_2014_2_NAsRemoved)

# By Department
Stays_Dept=data.frame(Dept=substr(Stays$FINESS, 1, 2), Stays=Stays$Stays, stringsAsFactors = F)
Stays_Dept=aggregate(Stays_Dept[,2], by=list(Stays_Dept[,1]), FUN=sum)
colnames(Stays_Dept)=c("Dept", "AnnualStays")
  
##########################
#### Get Edge Weights ####

Department=T
if(Department){
  cat("Upload Department Contact Network\n")
  load("Data/Department Network.RData")
  cat("Upload Department Contact Network pFlow\n")
  load("Data/pFlow_ALL_Dept.RData")
  # nij, list of edge weights (inter-Department transfers)
  Edge_Weights=as.list(c(E(directed.graph_Dept)$weight))
}else{
  # nij, list of edge weights (inter-Hospital transfers)
  Edge_Weights=as.list(c(E(directed.graph_ALL)$weight))
}


# Merge Hi stays with Hi edge weight
Edges=data.frame(Source=get.edgelist(directed.graph_Dept)[,1], 
                 Target=get.edgelist(directed.graph_Dept)[,2], 
                 Edge_Weights=unlist(Edge_Weights), stringsAsFactors = F)

Edges_Stays=merge(Edges, Stays_Dept, by.x="Source", by.y="Dept", all.x=T)
Edges_Stays$WeeklyStays=Edges_Stays$AnnualStays/52
Edges_Stays$TwoWeekStays=Edges_Stays$WeeklyStays*2
Edges_Stays$ThreeWeekStays=Edges_Stays$WeeklyStays*3
Edges_Stays$MonthlyStays=Edges_Stays$AnnualStays/12

# Round
Edges_Stays[,3:8]=round(Edges_Stays[3:8], digits = 0)


#######################
#### Probabilities ####

Annual_Probabilities = foreach(i=1:nrow(Edges_Stays)) %do% (1-dbinom(0, size = Edges_Stays[i,"AnnualStays"], prob = Edges_Stays[i,"Edge_Weights"]/Edges_Stays[i,"AnnualStays"]))
hist(unlist(Annual_Probabilities))
range(Annual_Probabilities, na.rm = T)
table(unlist(Annual_Probabilities))

Annual_Probabilities_Transformed =foreach(i=1:length(Annual_Probabilities)) %do% -log(Annual_Probabilities[[i]])
hist(unlist(Annual_Probabilities_Transformed))
range(Annual_Probabilities_Transformed, na.rm = T)
table(unlist(Annual_Probabilities_Transformed))

ThreeWeek_Probabilities = foreach(i=1:nrow(Edges_Stays)) %do% (1-dbinom(0, size = Edges_Stays[i,"ThreeWeekStays"], prob = Edges_Stays[i,"Edge_Weights"]/Edges_Stays[i,"ThreeWeekStays"]))
hist(unlist(ThreeWeek_Probabilities))
range(ThreeWeek_Probabilities, na.rm = T)
table(unlist(ThreeWeek_Probabilities))

ThreeWeek_Probabilities_Transformed =foreach(i=1:length(ThreeWeek_Probabilities)) %do% -log(ThreeWeek_Probabilities[[i]])
hist(unlist(ThreeWeek_Probabilities_Transformed))
range(ThreeWeek_Probabilities_Transformed, na.rm = T)
table(unlist(ThreeWeek_Probabilities_Transformed))

#############
# All Stays #

sum(All_Stays_2014_1_Sum$Stays) #14585906 total HC
sum(All_Stays_2014_2_Sum$Stays) #14585906 total HC
