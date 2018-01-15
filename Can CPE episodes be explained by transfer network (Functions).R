#####################################################################
#####DOES TRANSFER NETWORK HELP EXPLAIN CPE EPISODES (FUNCTIONS)#####
#####################################################################

################################################
#### CPE DATA WITH MECHANISM AND CLASS INFO ####
getData=function(startDate, endDate){
  ###########################################
  #### STEP 1a: GET DATA ON CPE EPISODES ####
  
  cat("Upload all data\n")
  data=read.csv(file=paste("Data/ALL_EPISODES (clean).csv", sep=""), header=T, sep=";", stringsAsFactors=FALSE)
  
  cat("Change dates to R dates\n")
  data$Department=as.character(data$Department)
  data$DateEpisode <- as.Date(data$DateEpisode, format = "%d/%m/%Y")
  data$DateOneCase <- as.Date(data$DateOneCase, format = "%d/%m/%Y")
  data$DateMoreOneCase <- as.Date(data$DateMoreOneCase, format = "%d/%m/%Y")
  data$DateImported <- as.Date(data$DateImported, format = "%d/%m/%Y")
  data$DateMoreFiveCases <- as.Date(data$DateMoreFiveCases, format = "%d/%m/%Y")
  
  cat("Select data by dates\n")
  data=data[which(data$DateEpisode > as.Date(startDate) & data$DateEpisode < as.Date(endDate)),]
  return(data)
}
getCPEData=function(){
  
  ###########################################
  #### STEP 1a: GET DATA ON CPE EPISODES ####
  
  cat("Upload all data\n")
  data=read.csv(file=paste("Data/ALL_EPISODES (clean).csv", sep=""), header=T, sep=";", stringsAsFactors=FALSE)
  
  cat("Change dates to R dates\n")
  data$Department=as.character(data$Department)
  data$DateEpisode <- as.Date(data$DateEpisode, format = "%d/%m/%Y")
  data$DateOneCase <- as.Date(data$DateOneCase, format = "%d/%m/%Y")
  data$DateMoreOneCase <- as.Date(data$DateMoreOneCase, format = "%d/%m/%Y")
  data$DateImported <- as.Date(data$DateImported, format = "%d/%m/%Y")
  data$DateMoreFiveCases <- as.Date(data$DateMoreFiveCases, format = "%d/%m/%Y")
  
  cat("Select data by dates\n")
  data=data[which(data$DateEpisode > as.Date(startDate) & data$DateEpisode < as.Date(endDate)),]
  
  ###########################################
  #### STEP 1b: MECHANISMS OF RESISTANCE ####
  
  cat("Split Multiple Mechanisms\n")
  Mechanisms=str_split_fixed(data$Mechanism, " ", 3)
  Mechanisms[Mechanisms == ""] = NA
  colnames(Mechanisms)=c("FirstMechanism", "SecondMechanism", "ThirdMechanism")
  Mechanisms=as.data.frame(Mechanisms, stringsAsFactors=FALSE)
  
  cat("Class Type\n")
  ClassA=c("SME", "IMI", "NMC", "GES", "KPC", "SHV", "SFC")
  ClassB=c("VIM", "IMP", "NDM")
  ClassD="OXA"
  
  cat("Add First Class Types\n")
  Mechanisms$FirstClass=NA
  Mechanisms$FirstClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassA"
  Mechanisms$FirstClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassB"
  Mechanisms$FirstClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassD"
  
  cat("Add Second Class Types\n")
  Mechanisms$SecondClass=NA
  Mechanisms$SecondClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassA"
  Mechanisms$SecondClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassB"
  Mechanisms$SecondClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassD"
  
  cat("Add Third Class Types\n")
  Mechanisms$ThirdClass=NA
  Mechanisms$ThirdClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassA"
  Mechanisms$ThirdClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassB"
  Mechanisms$ThirdClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassD"
  
  cat("Add Multiple Mechanisms to Data\n")
  data=cbind(data, Mechanisms)
  
  # cat("Add 'g' to General Mechanisms\n")
  # data$FirstMechanism[which(nchar(as.character(data$FirstMechanism)) <= 4)] = str_pad(data$FirstMechanism[which(nchar(as.character(data$FirstMechanism)) <= 4)] , 4, pad = "g", side="left")
  # data$SecondMechanism[which(nchar(as.character(data$SecondMechanism)) <= 4)] = str_pad(data$SecondMechanism[which(nchar(as.character(data$SecondMechanism)) <= 4)] , 4, pad = "g", side="left")
  data$ThirdMechanism[str_detect(data$ThirdMechanism, " ")] = NA
  # data$ThirdMechanism[which(nchar(as.character(data$ThirdMechanism)) <= 4)] = str_pad(data$ThirdMechanism[which(nchar(as.character(data$ThirdMechanism)) <= 4)] , 4, pad = "g", side="left")

  cat("Fix Department Variable\n")
  data$Department=str_pad(data$Department, 2, pad = "0")
  
  cat("Removing Episodes Occuring in Depts Other Than Depts in Network\n")
  data=data[which(data$Department %in% V(directed.graph_Dept)$name),]
  
  cat("Removing Episodes With Only One Unique Mechanism of Resistance\n")
  TableMechanisms=table(data$FirstMechanism)
  data=data[data$FirstMechanism %in% names(TableMechanisms)[TableMechanisms>1],]
  
  cat("Renaming Rows and Episode Numbers\n")
  data$Episode=1:nrow(data)
  rownames(data)=1:nrow(data)
  
  return(data)
}

##########################################################
#### GET CANDIDATE TRANSMITTERS BY MECHANISM OR CLASS ####
getCandidateTransmitters=function(data, Time){
  cat("#### STEP 2: Find All Candidate Transmitters ####\n")
  
  if(Week){
    cat(paste("Set Number of Preceeding Weeks Equal to", Time, "\n"))
    Week=Time
    
    cat("Criterion 1: Candidate Transmitters Occured N Weeks Before Incident Episode\n")
    if(NonPermutation){
      cat("For Observed Data\n")
      CandidateTransmitters=lapply(1:nrow(data), function(i){
        CaseDate=data[i,2]
        CaseID=data[i,1]
        data[data[,2] < CaseDate 
             & data[,2] >= (CaseDate-Week*7) 
             & data$Episode != CaseID,]})
      if(SharedDepartment){
        cat("Keep Candidates with Same Department as Incident Episode\n")
        CandidateTransmitters=CandidateTransmitters
      }else{
        cat("Candidates Transmitters Cannot Occur is Same Department as Incident Episode\n")
        CandidateTransmitters=foreach(j=1:length(CandidateTransmitters)) %do% {
          CandidateTransmittersSubset=CandidateTransmitters[[j]]
          if(!is.null(CandidateTransmittersSubset)){
            if(nrow(CandidateTransmittersSubset) > 0){
              Department="Department"
              Final_CandidateTransmittersSubset=CandidateTransmittersSubset[!(CandidateTransmittersSubset$Department == data[j,Department]),]
            }
          }
        }
      }
    }else{
      cat("For Permutated Data\n")
      dataPossibleCandidates=lapply(1:nrow(data), function(i){
        CaseDate=data[i,2]
        CaseID=data[i,1]
        data[data[,2] < CaseDate 
             & data[,2] >= (CaseDate-Week*7) 
             & data$Episode != CaseID,]})
      if(Reshuffled){
        if(SharedDepartment){
          cat("For each list of possible candidates, reassign 1 out of 85 departments\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnorder=names(dataPossibilities)
              dataPermutation=cbind(dataPossibilities[,c(1:13,16:21)], dataPossibilities[sample(1:nrow(dataPossibilities)),c(14:15)])
              dataPermutation=dataPermutation[,columnorder]
              dataPermutation$Department=as.character(dataPermutation$Department)
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }else{
          cat("For Each List of Possible Candidates, Reassign 1 out of 84 Departments Excluding Incident Department\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnorder=names(dataPossibilities)
              dataPermutation=cbind(dataPossibilities[,c(1:13,16:21)], dataPossibilities[sample(1:nrow(dataPossibilities)),c(14:15)])
              dataPermutation=dataPermutation[,columnorder]
              dataPermutation$Department=as.character(dataPermutation$Department)
              dataPermutation=dataPermutation[!(dataPermutation$Department == data[i, "Department"]),]
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }
      }else{
        if(SharedDepartment){
          cat("For each list of possible candidates, reassign 1 out of 85 departments\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnnames=names(dataPossibilities)
              departments=unique(data$Department)
              dataPermutation=cbind(dataPossibilities[,c(1:13)], sample(departments, size = nrow(dataPossibilities), replace = TRUE), dataPossibilities[,c(15:21)])
              colnames(dataPermutation)=columnnames
              dataPermutation$Department=as.character(dataPermutation$Department)
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }else{
          cat("For Each List of Possible Candidates, Reassign 1 out of 84 Departments Excluding Incident Department\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnnames=names(dataPossibilities)
              departments=unique(data$Department)
              departments=departments[!(departments == data[i, "Department"])]
              dataPermutation=cbind(dataPossibilities[,c(1:13)], sample(departments, size = nrow(dataPossibilities), replace = TRUE), dataPossibilities[,c(15:21)])
              colnames(dataPermutation)=columnnames
              dataPermutation$Department=as.character(dataPermutation$Department)
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }
      }
    }
  }else{
    cat(paste("Set Number of Preceeding Days Equal to", Time, "\n"))
    Day=Time
    
    cat("Criterion 1: Candidate Transmitters Occured N Days Before Incident Episode\n")
    if(NonPermutation){
      cat("For Observed Data\n")
      if(Sliding){
        cat(paste("Sliding", WeekSlide, "Week by", Day,"Day\n"))
        CandidateTransmitters=lapply(1:nrow(data), function(i){
          CaseDate=data[i,2]
          CaseID=data[i,1]
          data[data[,2] <= (CaseDate-Day) 
               & data[,2] > (CaseDate-WeekSlide*7-Day) 
               & data$Episode != CaseID,]})
      }else{
        cat(paste("For Non-Sliding Day", Day, "\n"))
        CandidateTransmitters=lapply(1:nrow(data), function(i){
          CaseDate=data[i,2]
          CaseID=data[i,1]
          data[data[,2] < CaseDate 
               & data[,2] >= (CaseDate-Day) 
               & data$Episode != CaseID,]})
      }
    }else{
      cat("For Permutated Data\n")
      if(Sliding){
        cat(paste("Sliding", WeekSlide, "Week by", Day,"\n"))
        dataPossibleCandidates=lapply(1:nrow(data), function(i){
          CaseDate=data[i,2]
          CaseID=data[i,1]
          data[data[,2] <= (CaseDate-Day) 
               & data[,2] > (CaseDate-WeekSlide*7-Day) 
               & data$Episode != CaseID,]})
      }else{
        cat(paste("For Non-Sliding Day", Day, "\n"))
        dataPossibleCandidates=lapply(1:nrow(data), function(i){
          CaseDate=data[i,2]
          CaseID=data[i,1]
          data[data[,2] < CaseDate 
               & data[,2] >= (CaseDate-Day) 
               & data$Episode != CaseID,]})
      }
      if(Reshuffled){
        if(SharedDepartment){
          cat("For each list of possible candidates, reassign 1 out of 85 departments\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnorder=names(dataPossibilities)
              dataPermutation=cbind(dataPossibilities[,c(1:13,16:21)], dataPossibilities[sample(1:nrow(dataPossibilities)),c(14:15)])
              dataPermutation=dataPermutation[,columnorder]
              dataPermutation$Department=as.character(dataPermutation$Department)
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }else{
          cat("For Each List of Possible Candidates, Reassign 1 out of 84 Departments Excluding Incident Department\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnorder=names(dataPossibilities)
              dataPermutation=cbind(dataPossibilities[,c(1:13,16:21)], dataPossibilities[sample(1:nrow(dataPossibilities)),c(14:15)])
              dataPermutation=dataPermutation[,columnorder]
              dataPermutation$Department=as.character(dataPermutation$Department)
              dataPermutation=dataPermutation[!(dataPermutation$Department == data[i, "Department"]),]
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }
      }else{
        if(SharedDepartment){
          cat("For each list of possible candidates, reassign 1 out of 85 departments\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnnames=names(dataPossibilities)
              departments=unique(data$Department)
              dataPermutation=cbind(dataPossibilities[,c(1:13)], sample(departments, size = nrow(dataPossibilities), replace = TRUE), dataPossibilities[,c(15:21)])
              colnames(dataPermutation)=columnnames
              dataPermutation$Department=as.character(dataPermutation$Department)
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }else{
          cat("For Each List of Possible Candidates, Reassign 1 out of 84 Departments Excluding Incident Department\n")
          CandidateTransmitters=lapply(1:length(dataPossibleCandidates), function(i){
            if(nrow(dataPossibleCandidates[[i]]) > 0){
              dataPossibilities=dataPossibleCandidates[[i]]
              columnnames=names(dataPossibilities)
              departments=unique(data$Department)
              departments=departments[!(departments == data[i, "Department"])]
              dataPermutation=cbind(dataPossibilities[,c(1:13)], sample(departments, size = nrow(dataPossibilities), replace = TRUE), dataPossibilities[,c(15:21)])
              colnames(dataPermutation)=columnnames
              dataPermutation$Department=as.character(dataPermutation$Department)
            }else{
              dataPermutation=dataPossibleCandidates[[i]]
            }
            return(dataPermutation)
          })
        }
      }
    }
  }
  
  cat("Criterion 2: Imported Episodes Do Not Have Candidate Transmitters\n")
  ImportationStatus="Imported"
  CandidateTransmitters_ImportationCorrection=foreach(i=1:length(CandidateTransmitters)) %do% {
    if(data[i,ImportationStatus] == "N"){
      CandidateTransmitters[[i]]=CandidateTransmitters[[i]]
    }
  }
  
  
  if(Mechanism){
    cat("Criterion 3: Same CPE Mechanism of Resistance\n")
    
    FirstMechanism="FirstMechanism"
    AnyMechanism=c("FirstMechanism", "SecondMechanism", "ThirdMechanism")
    
    CandidateTransmitters_byMechanism1=foreach(i=1:length(CandidateTransmitters_ImportationCorrection)) %do% {
      if(is.null(CandidateTransmitters_ImportationCorrection[[i]])){
        NULL
      }else{
        if(any(nchar(data[i,FirstMechanism]) <= 3, na.rm = T)){
          #Criteria 1: if general mechanism, can have any ancestor
          list=c(data[i,FirstMechanism])
          CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
          CandidateTransmitters[which(apply(CandidateTransmitters[,AnyMechanism], 1, 
                                            function(i) any(grepl(paste(list, collapse="|"), i), na.rm = T))),]
        }else{
          #Criteria 2: if specific mechanism, can have same specific mechanism or general ancestor
          list=c(paste0("^",data[i,FirstMechanism],"$"), paste0("^",substr(data[i,FirstMechanism], 1, 3),"$"))
          CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
          CandidateTransmitters=CandidateTransmitters[which(apply(CandidateTransmitters[,AnyMechanism], 1, 
                                                                  function(i) any(grepl(paste(list, collapse="|"), i), na.rm = T))),]
        }
      }
    }
    
    SecondMechanism="SecondMechanism"
    CandidateTransmitters_byMechanism2=foreach(i=1:length(CandidateTransmitters_ImportationCorrection)) %do% {
      if(is.null(CandidateTransmitters_ImportationCorrection[[i]])){
        NULL
      }else{
        if(any(nchar(data[i,SecondMechanism]) <= 3, na.rm = T)){
          #Criteria 1: if general mechanism, can have any ancestor
          list=c(data[i,SecondMechanism])
          CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
          CandidateTransmitters[which(apply(CandidateTransmitters[,AnyMechanism], 1, 
                                            function(i) any(grepl(paste(list, collapse="|"), i), na.rm = T))),]
        }else{
          #Criteria 2: if specific mechanism, can have same specific mechanism or general ancestor
          list=c(paste0("^",data[i,SecondMechanism],"$"), paste0("^",substr(data[i,SecondMechanism], 1, 3),"$"))
          CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
          CandidateTransmitters=CandidateTransmitters[which(apply(CandidateTransmitters[,AnyMechanism], 1, 
                                                                  function(i) any(grepl(paste(list, collapse="|"), i), na.rm = T))),]
        }
      }
    }
    
    
    ThirdMechanism="ThirdMechanism"
    CandidateTransmitters_byMechanism3=foreach(i=1:length(CandidateTransmitters_ImportationCorrection)) %do% {
      if(is.null(CandidateTransmitters_ImportationCorrection[[i]])){
        NULL
      }else{
        if(any(nchar(data[i,ThirdMechanism]) <= 3, na.rm = T)){
          #Criteria 1: if general mechanism, can have any ancestor
          list=c(data[i,ThirdMechanism])
          CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
          CandidateTransmitters[which(apply(CandidateTransmitters[,AnyMechanism], 1, 
                                            function(i) any(grepl(paste(list, collapse="|"), i), na.rm = T))),]
        }else{
          #Criteria 2: if specific mechanism, can have same specific mechanism or general ancestor
          list=c(paste0("^",data[i,ThirdMechanism],"$"), paste0("^",substr(data[i,ThirdMechanism], 1, 3),"$"))
          CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
          CandidateTransmitters=CandidateTransmitters[which(apply(CandidateTransmitters[,AnyMechanism], 1, 
                                                                  function(i) any(grepl(paste(list, collapse="|"), i), na.rm = T))),]
        }
      }
    }
    
    nestedlist=list(CandidateTransmitters_byMechanism1, CandidateTransmitters_byMechanism2, CandidateTransmitters_byMechanism3)
    Final_CandidateTransmitters=do.call(Map, c(f = rbind, nestedlist))
    
  }else{
    # cat("Criterion 3: Same CPE Class of Resistance\n")
    # FirstResistanceClass="FirstClass"
    # CandidateTransmitters_byFirstClass=foreach(i=1:length(CandidateTransmitters_ImportationCorrection)) %do% {
    #   CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
    #   CandidateTransmitters[CandidateTransmitters[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
    # }
    # SecondResistanceClass="SecondClass"
    # CandidateTransmitters_bySecondClass=foreach(i=1:length(CandidateTransmitters_byFirstClass)) %do% {
    #   CandidateTransmitters=CandidateTransmitters_byFirstClass[[i]]
    #   CandidateTransmitters[CandidateTransmitters[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
    # }
    # ThirdResistanceClass="ThirdClass"
    # CandidateTransmitters_byThirdClass=foreach(i=1:length(CandidateTransmitters_bySecondClass)) %do% {
    #   CandidateTransmitters=CandidateTransmitters_bySecondClass[[i]]
    #   CandidateTransmitters[CandidateTransmitters[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
    # }
    # Final_CandidateTransmitters=CandidateTransmitters_byThirdClass
    #   
    #   cat("Criterion 3: Same CPE Class of Resistance\n")
    #   ClassRows=c("FirstClass","SecondClass","ThirdClass")
    #   
    #   CandidateTransmitters_byClass==foreach(i=1:length(CandidateTransmitters_ImportationCorrection)) %do% {
    #     if(is.null(CandidateTransmitters_ImportationCorrection[[i]])){
    #       NULL
    #     }else{
    #       list=c(data[i,ClassRows])
    #       CandidateTransmitters[which(apply(CandidateTransmitters[,ClassRows], 1, function(i) any(grepl(paste(list, collapse="|"), i)))),]
    #     }
    #   }
    #   Final_CandidateTransmitters=CandidateTransmitters_byClass
    #   
    }
  
  return(Final_CandidateTransmitters)
}

########################################################
#### GET MINIMUM DISTANCE OF CANDIDATE TRANSMITTERS ####
getMinimumDistances=function(i, CandidateTransmitters, weights, algorithm){
  ##############################################################################################
  #### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####
  cat("#### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####\n")
  
  cat("List of Departments of Episode and Candidate Transmitters\n")
  Department="Department"
  CandidateTransmitters_Departments=lapply(1:length(CandidateTransmitters), function(i) {
    CandidateTransmitters_Department=as.data.frame(as.character(CandidateTransmitters[[i]]$Department), stringsAsFactors = FALSE)
    colnames(CandidateTransmitters_Department)=Department
    return(CandidateTransmitters_Department)
  })
  cat("Distance Matrix Between Departments\n")
  Distances_Matrix=as.data.frame(distances(directed.graph_Dept, mode="in", weights = weights, algorithm = algorithm))
  
  cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
  CandidateTransmitters_Departments_MinDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
    CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
    if(is.null(CandidateTransmitters_Departments_Subset)){
      Distances=NA
    }else{
      if(nrow(CandidateTransmitters_Departments_Subset) > 0 ){
        Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
          Distances=Distances_Matrix[CandidateTransmitters_Departments_Subset[j,Department],data[i,Department]]
        } 
      }else{
        Distances=NA
      }
    }
  }
  
  cat("Get Minimum Distance Between Episode and 1 Potential Infector\n")
  MinDistance=lapply(CandidateTransmitters_Departments_MinDistances, function(x) min(x))
  
  cat(paste("Unlist, Remove Inf, Minimum Distances", i, "\n"))
  MinimumDistances=unlist(MinDistance)
  
  return(MinimumDistances)
}

###########################
#### CLEANING FUNCTION ####
CleaningFunction=function(x){
  x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
  x <- lapply(x, function(x) {x[c(1:length(x))]})
  return(x)
} 
CleaningFunction2=function(x){
  x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
  return(x)
}

##################################################################################
#### GET AVERAGE MINIMUM DISTANCE OF CANDIDATE TRANSMITTERS FROM PERMUTATIONS ####
getAverageMinDistances_CandidateTransmitters_Permutations=function(MinimumDistances_PotentialInfector_Permutation){
  cat(paste("Average", Week, "runs\n"))
  CandidateTransmitters_Permutations_byWeek=foreach(i=1:length(MinimumDistances_PotentialInfector_Permutation[[1]])) %do% lapply(MinimumDistances_PotentialInfector_Permutation, `[[`, i) #get first elements i of each list
  
  cat("Run Cleaning Function for Random Simulations and Original Results\n")
  CandidateTransmitters_Permutations_byWeek_Clean=foreach(i=1:length(CandidateTransmitters_Permutations_byWeek)) %do% CleaningFunction(CandidateTransmitters_Permutations_byWeek[[i]])
  
  cat("Convert sub-list of",Nruns," values into dataframe to facilitate per case averages\n")
  CandidateTransmitters_Permutations_byWeek_Dataframes=foreach(i=1:length(CandidateTransmitters_Permutations_byWeek_Clean)) %do% data.frame(CandidateTransmitters_Permutations_byWeek_Clean[[i]], row.names = NULL) 
  
  cat("Row Means of",Nruns," simulations for every case\n")
  CandidateTransmitters_Permutations_byWeek_RowMeans=foreach(i=1:length(CandidateTransmitters_Permutations_byWeek_Dataframes)) %do% rowMeans(CandidateTransmitters_Permutations_byWeek_Dataframes[[i]], na.rm=TRUE)
  
  return(CandidateTransmitters_Permutations_byWeek_RowMeans)
}

##################################################
#### WILCOXON PAIRED RANKED SUM TEST FUNCTION ####
getWilcoxonPairedRankTestPValues=function(i, MinimumDistances, MinimumDistances_Permutations){
  # cat("Clean out Inf values from MinimumDistances\n")
  # MinimumDistances_Clean=CleaningFunction(MinimumDistances)
  cat(paste("Combine Non-Random and Random Minimum Distances for Week", i, "\n"))
  CombinedMinimumDistances=cbind(MinimumDistances[[i]], MinimumDistances_Permutations[[i]])
  cat("Test only for lengths greater than 3\n")
  length=nrow(CombinedMinimumDistances)
  if(length > 3L){
    cat(paste("Run Test for Week", i, "\n"))
    CombinedMinimumDistances_WilcoxonTest=wilcox.test(CombinedMinimumDistances[,1], CombinedMinimumDistances[,2], paired = T)
    #H0: distributions of calculated potential infector min distances is the same as the distribution of min distances of random potetial infectors
  }else{
    CombinedMinimumDistances_WilcoxonTest$p.value<-NA
  }
  return(CombinedMinimumDistances_WilcoxonTest$p.value)
}

####################################################
#### PROPORTIONS OF VALUES UNDER 5TH PERCENTILE ####
get5thQuantiles=function(Week, MinimumDistances, AllRandomMinimumDistances){
  cat("Convert random permutations into dataframes\n")
  RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
  RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
  cat("Get proportion cases that have values smaller than the 5th percentile of simulations for each week N\n")
  ProportionsTable=foreach(n=1:Week, .combine = "cbind") %do% {
    week=foreach(i=1:nrow(data)) %do% (MinimumDistances[[n]][i] <= quantile(RandomSimulationsByDays_Dataframes[[n]][i,], 0.05, na.rm=T)[[1]])
    week=unlist(week)
    resultsTable=prop.table(table(week))
  }
  return(ProportionsTable)
}
getRange=function(Week, MinimumDistances, AllRandomMinimumDistances){
  RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
  RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
  
  RangeTable=foreach(n=1:Week, .combine = "rbind") %do% {
    range=c(range(MinimumDistances[[n]], na.rm=T), range(unlist(RandomSimulationsByDays_Dataframes[[n]]), na.rm=T))
    range=unlist(range)
  }
  colnames(RangeTable)=c("Min_Observed", "Max_Observed", "Min_Permutations", "Max_Permutations")
  RangeTable=as.data.frame(RangeTable)
  return(RangeTable)
}

###################################
#### MEAN WEEKLY MIN DISTANCES ####
getMeanMinimumDistances=function(MinimumDistances, AllPermutatationMinimumDistances){
  Week=Week
  
  #Original
  #Weighted
  cat("Mean distance for every Week N for candidate transmitters\n")
  MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
  MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
  
  #Permutations
  #Weighted
  cat("Convert random simulations into dataframes\n")
  RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByWeeks_Clean=foreach(i=1:length(RandomSimulationsByWeeks)) %do% CleaningFunction(RandomSimulationsByWeeks[[i]])
  RandomSimulationsByWeeks_Dataframes=foreach(i=1:length(RandomSimulationsByWeeks_Clean)) %do% data.frame(RandomSimulationsByWeeks_Clean[[i]], row.names = NULL) 
  cat(paste("Row Means of", Week, "permutations for every case\n"))
  RandomSimulationsByWeeks_RowMeans=foreach(i=1:length(RandomSimulationsByWeeks_Dataframes)) %do% rowMeans(RandomSimulationsByWeeks_Dataframes[[i]], na.rm=TRUE)
  cat(paste("Min weighted distance means of", Week, "permutations for every Week N\n"))
  RandomSimulationsByWeeks_MeansByNWeeks=foreach(i=1:length(RandomSimulationsByWeeks_RowMeans)) %do% mean(unlist(RandomSimulationsByWeeks_RowMeans[[i]]), na.rm = T)
  
  cat("Get table\n")
  MinDistancesTableByMeanPerNWeeks=as.data.frame(cbind(unlist(MinimumDistances_MeansByNWeeks),
                                                       unlist(RandomSimulationsByWeeks_MeansByNWeeks)))
  colnames(MinDistancesTableByMeanPerNWeeks)=c("Means_Observed", "Means_Permutations")
  
  return(MinDistancesTableByMeanPerNWeeks)
}

###############################
##### GET PAIRS BY WINDOW #####
getPairsByWindow=function(Window, AllMinimumDistances, AllCandidateTransmitters){
 
  cat("#### SELECT WINDOW ####\n")
  
  cat("Get lowest observed minimum distance\n")
  MinimumDistances_Clean=foreach(i=1:length(AllMinimumDistances)) %do% CleaningFunction2(AllMinimumDistances[[i]])
  MinimumDistances_MeansByNWindow=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
  
  # cat("Identify window which has the lowest minimum distance")
  # min(unlist(MinimumDistances_MeansByNWindow))
  # Window=which.min(MinimumDistances_MeansByNWindow)
  
  cat(paste("Subset Candidates from Day =", Window, "to Day", Window+WeekSlide*7))
  CandidateTransmitters=AllCandidateTransmitters[[Window]]
  # MinDistances=MinimumDistances_byDay_byMechanism_SharedDept_Reshuffled_Sliding[[Window]]
  
  cat("#### GET POTENTIAL CANDIDATES ####\n")
  
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
  PotentialInfectors=foreach(i=1:length(CandidateTransmitters)) %do% {
    if(!is.na(CandidateTransmitters_Departments_MinDistances[[i]])){
      CandidateTransmitters_Subset=CandidateTransmitters[[i]]
      MinDistanceLocation_Subset=MinDistanceLocation[[i]]
      PotentialCandidate=CandidateTransmitters_Subset[MinDistanceLocation_Subset,]
    }
  }
  
  # cat("Save or load\n")
  # save(PotentialInfectors, file=paste0(writingDir, "50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/PotentialInfectors.RData"))
  # load(paste0(writingDir, "50 Permutations (Reshuffled Shared Department ", Year, " Data Sliding Week)/PotentialInfectors.RData"))
  
  cat("#### GET PAIRS ####\n")
  
  cat("Get all pairs of incident episodes and potential infectors by episode number\n")
  Episode="Episode"
  Pairs_Episodes=foreach(i=1:length(PotentialInfectors), .combine = 'rbind') %do% {
    if(is.null(PotentialInfectors[[i]])){
      None=c(NA, NA)
    }else{
      Pairs_Episode=c(PotentialInfectors[[i]]$Episode, data[i,Episode])}
  }
  
  cat("Remove NAs and bind\n")
  # Pairs_Episodes=Pairs_Episodes[complete.cases(Pairs_Episodes),]
  # Pairs_Episodes=Pairs_Episodes[order(Pairs_Episodes[,1]),]
  Pairs_Episodes=cbind(as.character(Pairs_Episodes[,1]), as.character(Pairs_Episodes[,2]))
  
  cat("Get all pairs of imported incident episodes and potential infectors by episode number\n")
  Pairs_Episodes_Imported=foreach(i=1:length(PotentialInfectors), .combine = 'rbind') %do% {
    if(is.null(PotentialInfectors[[i]])){
      None=c(NA, NA)
    }else{
      require(plyr)
      PotentialInfectors_Imported=llply(PotentialInfectors, function(x) subset(x, x$Imported == "O"))
      data_Imported=data[which(data$Imported == "O"),]
      Pairs_Episode=c(PotentialInfectors_Imported[[i]]$Episode, data_Imported[i,Episode])}
  }
  
  cat("Remove NAs and bind\n")
  # Pairs_Episodes_Imported=Pairs_Episodes_Imported[complete.cases(Pairs_Episodes_Imported),]
  # Pairs_Episodes_Imported=Pairs_Episodes_Imported[order(Pairs_Episodes_Imported[,1]),]
  Pairs_Episodes_Imported=cbind(as.character(Pairs_Episodes_Imported[,1]), as.character(Pairs_Episodes_Imported[,2]))
  
  cat("Combine Both Pairs to List\n")
  BothPairs=list(Pairs_Episodes, Pairs_Episodes_Imported)
  
  return(BothPairs)
}

