#####################################################################
#####DOES TRANSFER NETWORK HELP EXPLAIN CPE EPISODES (FUNCTIONS)#####
#####################################################################

################################################
#### CPE DATA WITH MECHANISM AND CLASS INFO ####
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
  
  cat("Fix Department Variable")
  data$Department=str_pad(data$Department, 2, pad = "0")
  
  cat("Removing Episodes Occuring in Depts Other Than Depts in Network")
  data=data[which(data$Department %in% V(directed.graph_Dept)$name),]
  
  cat("Renaming Rows and Episode Numbers")
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
    
    cat("Criterion 1: Candidate Transmitters Occured N Sliding Days Before Incident Episode\n")
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
    FirstResistanceMechanism="FirstMechanism"
    CandidateTransmitters_byFirstMechanism=foreach(i=1:length(CandidateTransmitters_ImportationCorrection)) %do% {
      CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
      CandidateTransmitters[CandidateTransmitters[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
    }
    SecondResistanceMechanism="SecondMechanism"
    CandidateTransmitters_bySecondMechanism=foreach(i=1:length(CandidateTransmitters_byFirstMechanism)) %do% {
      CandidateTransmitters=CandidateTransmitters_byFirstMechanism[[i]]
      CandidateTransmitters[CandidateTransmitters[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
    }
    ThirdResistanceMechanism="ThirdMechanism"
    CandidateTransmitters_byThirdMechanism=foreach(i=1:length(CandidateTransmitters_bySecondMechanism)) %do% {
      CandidateTransmitters=CandidateTransmitters_bySecondMechanism[[i]]
      CandidateTransmitters[CandidateTransmitters[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
    }
    Final_CandidateTransmitters=CandidateTransmitters_byThirdMechanism
  }else{
    cat("Criterion 3: Same CPE Class of Resistance\n")
    FirstResistanceClass="FirstClass"
    CandidateTransmitters_byFirstClass=foreach(i=1:length(CandidateTransmitters_ImportationCorrection)) %do% {
      CandidateTransmitters=CandidateTransmitters_ImportationCorrection[[i]]
      CandidateTransmitters[CandidateTransmitters[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
    }
    SecondResistanceClass="SecondClass"
    CandidateTransmitters_bySecondClass=foreach(i=1:length(CandidateTransmitters_byFirstClass)) %do% {
      CandidateTransmitters=CandidateTransmitters_byFirstClass[[i]]
      CandidateTransmitters[CandidateTransmitters[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
    }
    ThirdResistanceClass="ThirdClass"
    CandidateTransmitters_byThirdClass=foreach(i=1:length(CandidateTransmitters_bySecondClass)) %do% {
      CandidateTransmitters=CandidateTransmitters_bySecondClass[[i]]
      CandidateTransmitters[CandidateTransmitters[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
    }
    Final_CandidateTransmitters=CandidateTransmitters_byThirdClass
  }
  
  return(Final_CandidateTransmitters)
}

#Older Functions
getCandidateTransmitters_byWeek_byMechanism=function(data, Week){
  
  ##############################################
  #### STEP 2: Find All Candidate Transmitters ####
  cat("#### STEP 2: Find All Candidate Transmitters ####\n")
  
  #A potential infector
  # - occured N days/Weeks before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
  # - shares the same mechanism OR shares same class 
  # - in the same bacteria?? (will not test yet)
  
  cat(paste("Set Number of Preceeding Weeks Equal to", Week, "\n"))
  Week=Week
  
  if(NonPermutation){
    cat("Criterion 1: Episode Occured N Weeks Before\n")
    CandidateTransmitters_byWeeks=lapply(1:nrow(data), function(i){
      CaseDate=data[i,2]
      CaseID=data[i,1]
      data[data[,2] < CaseDate 
           & data[,2] >= (CaseDate-Week*7) 
           & data$Episode != CaseID,]})
  }else{
    CandidateTransmitters_byWeeks=getPermutatedData(data, Week)
    # CandidateTransmitters_byWeeks=foreach(i=length(CandidateTransmitters_byWeeks)) %do% {
    #   if(is.null(CandidateTransmitters_byWeeks[[i]]))
    #   CandidateTransmitters_byWeeks[[i]]=as.data.frame(matrix(ncol = 21, nrow = 0))
    # }

  }
  
  cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
  ImportationStatus="Imported"
  CandidateTransmitters_byWeeks_ImportationCorrection=foreach(i=1:length(CandidateTransmitters_byWeeks)) %do% {
    if(data[i,ImportationStatus] == "N"){
      CandidateTransmitters_byWeeks[[i]]=CandidateTransmitters_byWeeks[[i]]
    }
  }
  CandidateTransmitters_byWeeks_ImportationCorrection[sapply(CandidateTransmitters_byWeeks_ImportationCorrection, is.null)] = NULL
    
  cat("Criterion 3: Same CPE mechanism of resistance\n")
  FirstResistanceMechanism="FirstMechanism"
  CandidateTransmitters_byFirstMechanism=foreach(i=1:length(CandidateTransmitters_byWeeks_ImportationCorrection)) %do% {
      CandidateTransmitters=CandidateTransmitters_byWeeks_ImportationCorrection[[i]]
      CandidateTransmitters[CandidateTransmitters[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
    }
  SecondResistanceMechanism="SecondMechanism"
  CandidateTransmitters_bySecondMechanism=foreach(i=1:length(CandidateTransmitters_byFirstMechanism)) %do% {
      CandidateTransmitters=CandidateTransmitters_byFirstMechanism[[i]]
      CandidateTransmitters[CandidateTransmitters[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
    }
  ThirdResistanceMechanism="ThirdMechanism"
  CandidateTransmitters_byThirdMechanism=foreach(i=1:length(CandidateTransmitters_bySecondMechanism)) %do% {
      CandidateTransmitters=CandidateTransmitters_bySecondMechanism[[i]]
      CandidateTransmitters[CandidateTransmitters[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
    }
  
  return(CandidateTransmitters_byThirdMechanism)
}
getCandidateTransmitters_byWeek_byClass=function(data, Week){
  
  ##############################################
  #### STEP 2: Find All Potential Infectors ####
  cat("#### STEP 2: Find All Candidate Transmitters ####\n")
  
  #A potential infector
  # - occured N days/Weeks before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
  # - shares the same mechanism OR shares same class
  # - in the same bacteria?? (will not test yet)
  
  cat(paste("Set Number of Preceeding Days Equal to", Week, "\n"))
  Week=Week
  
  cat("Criterion 1: Episode Occured N Weeks Before\n")
  CandidateTransmitters_byWeeks=lapply(1:nrow(data), function(i){data[data[,2]<data[i,2] & data[,2]> (data[i,2]-Week*7),]})
  
  cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
  ImportationStatus="Imported"
  CandidateTransmitters_byWeeks_ImportationCorrection=foreach(i=1:length(CandidateTransmitters_byWeeks)) %do% {
    ConditionMet=data[i,ImportationStatus] == "N" 
    if(ConditionMet){
      CandidateTransmitters_byWeeks[[i]]=CandidateTransmitters_byWeeks[[i]]
    }
  }
  CandidateTransmitters_byWeeks_ImportationCorrection[sapply(CandidateTransmitters_byWeeks_ImportationCorrection, is.null)] = NULL
  
  
  cat("Criterion 3: Same CPE Class of resistance\n")
  FirstResistanceClass="FirstClass"
  CandidateTransmitters_byFirstClass=foreach(i=1:length(CandidateTransmitters_byWeeks_ImportationCorrection)) %do% {
    CandidateTransmitters=CandidateTransmitters_byWeeks_ImportationCorrection[[i]]
    CandidateTransmitters[CandidateTransmitters[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
  }
  SecondResistanceClass="SecondClass"
  CandidateTransmitters_bySecondClass=foreach(i=1:length(CandidateTransmitters_byFirstClass)) %do% {
    CandidateTransmitters=CandidateTransmitters_byFirstClass[[i]]
    CandidateTransmitters[CandidateTransmitters[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
  }
  ThirdResistanceClass="ThirdClass"
  CandidateTransmitters_byThirdClass=foreach(i=1:length(CandidateTransmitters_bySecondClass)) %do% {
    CandidateTransmitters=CandidateTransmitters_bySecondClass[[i]]
    CandidateTransmitters[CandidateTransmitters[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
  }
  return(CandidateTransmitters_byThirdClass)
}
getCandidateTransmitters_byWeek_byMechanism_Reassignment=function(data, Week){
  
  ##############################################
  #### STEP 2: Find All Candidate Transmitters ####
  cat("#### STEP 2: Find All Candidate Transmitters ####\n")
  
  #A potential infector
  # - occured N days/Weeks before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
  # - shares the same mechanism OR shares same class 
  # - in the same bacteria?? (will not test yet)
  
  cat(paste("Set Number of Preceeding Weeks Equal to", Week, "\n"))
  Week=Week
  
  if(NonPermutation){
    cat("Criterion 1: Episode Occured N Weeks Before\n")
    CandidateTransmitters_byWeeks=lapply(1:nrow(data), function(i){
      CaseDate=data[i,2]
      CaseID=data[i,1]
      data[data[,2] < CaseDate 
           & data[,2] >= (CaseDate-Week*7) 
           & data$Episode != CaseID,]})
  }else{
    CandidateTransmitters_byWeeks=getPermutatedData_Reassignment(data, Week)
    }
  
  cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
  ImportationStatus="Imported"
  CandidateTransmitters_byWeeks_ImportationCorrection=foreach(i=1:length(CandidateTransmitters_byWeeks)) %do% {
    if(data[i,ImportationStatus] == "N"){
      CandidateTransmitters_byWeeks[[i]]=CandidateTransmitters_byWeeks[[i]]
    }
  }
  # CandidateTransmitters_byWeeks_ImportationCorrection[sapply(CandidateTransmitters_byWeeks_ImportationCorrection, is.null)] = NULL
  
  cat("Criterion 3: Same CPE mechanism of resistance\n")
  FirstResistanceMechanism="FirstMechanism"
  CandidateTransmitters_byFirstMechanism=foreach(i=1:length(CandidateTransmitters_byWeeks_ImportationCorrection)) %do% {
    CandidateTransmitters=CandidateTransmitters_byWeeks_ImportationCorrection[[i]]
    CandidateTransmitters[CandidateTransmitters[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
  }
  SecondResistanceMechanism="SecondMechanism"
  CandidateTransmitters_bySecondMechanism=foreach(i=1:length(CandidateTransmitters_byFirstMechanism)) %do% {
    CandidateTransmitters=CandidateTransmitters_byFirstMechanism[[i]]
    CandidateTransmitters[CandidateTransmitters[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
  }
  ThirdResistanceMechanism="ThirdMechanism"
  CandidateTransmitters_byThirdMechanism=foreach(i=1:length(CandidateTransmitters_bySecondMechanism)) %do% {
    CandidateTransmitters=CandidateTransmitters_bySecondMechanism[[i]]
    CandidateTransmitters[CandidateTransmitters[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
  }
  
  return(CandidateTransmitters_byThirdMechanism)
}
getCandidateTransmitters_byDay_byMechanism_Reassignment=function(data, Day){
  
  ##############################################
  #### STEP 2: Find All Candidate Transmitters ####
  cat("#### STEP 2: Find All Candidate Transmitters ####\n")
  
  #A potential infector
  # - occured N days/Weeks before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
  # - shares the same mechanism OR shares same class 
  # - in the same bacteria?? (will not test yet)
  
  cat(paste("Set Number of Preceeding Days Equal to", Day, "\n"))
  Day=Day
  
  if(NonPermutation){
    cat("Criterion 1: Episode Occured N Days Before\n")
    CandidateTransmitters_byDays=lapply(1:nrow(data), function(i){
      CaseDate=data[i,2]
      CaseID=data[i,1]
      data[data[,2] < CaseDate 
           & data[,2] >= (CaseDate-Day) 
           & data$Episode != CaseID,]})
  }else{
    CandidateTransmitters_byDays=getPermutatedData_byDays(data, Day)
  }
  
  cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
  ImportationStatus="Imported"
  CandidateTransmitters_byDays_ImportationCorrection=foreach(i=1:length(CandidateTransmitters_byDays)) %do% {
    if(data[i,ImportationStatus] == "N"){
      CandidateTransmitters_byDays[[i]]=CandidateTransmitters_byDays[[i]]
    }
  }
  # CandidateTransmitters_byDays_ImportationCorrection[sapply(CandidateTransmitters_byDays_ImportationCorrection, is.null)] = NULL
  
  cat("Criterion 3: Same CPE mechanism of resistance\n")
  FirstResistanceMechanism="FirstMechanism"
  CandidateTransmitters_byFirstMechanism=foreach(i=1:length(CandidateTransmitters_byDays_ImportationCorrection)) %do% {
    CandidateTransmitters=CandidateTransmitters_byDays_ImportationCorrection[[i]]
    CandidateTransmitters[CandidateTransmitters[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
  }
  SecondResistanceMechanism="SecondMechanism"
  CandidateTransmitters_bySecondMechanism=foreach(i=1:length(CandidateTransmitters_byFirstMechanism)) %do% {
    CandidateTransmitters=CandidateTransmitters_byFirstMechanism[[i]]
    CandidateTransmitters[CandidateTransmitters[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
  }
  ThirdResistanceMechanism="ThirdMechanism"
  CandidateTransmitters_byThirdMechanism=foreach(i=1:length(CandidateTransmitters_bySecondMechanism)) %do% {
    CandidateTransmitters=CandidateTransmitters_bySecondMechanism[[i]]
    CandidateTransmitters[CandidateTransmitters[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
  }
  
  return(CandidateTransmitters_byThirdMechanism)
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

#Older Functions
getMinimumDistances_CandidateTransmitters_byWeek=function(i, CandidateTransmitters, weights, algorithm){
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
getMinimumGeoDistances_CandidateTransmitters_byWeek=function(i, CandidateTransmitters, weights, algorithm){
  ##############################################################################################
  #### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####
  cat("#### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####\n")
  
  cat("List of Departments of Episode and Candidate Transmitters\n")
  Department="Department"
  CandidateTransmitters_Departments=foreach(i=1:length(CandidateTransmitters)) %do% {
    CandidateTransmitters_Departments=CandidateTransmitters[[i]]
    CandidateTransmitters_Departments[Department]
  }
  
  cat("Distance Matrix Between Departments\n")
  Distances_Matrix=as.data.frame(distances(directed.graph_Dept, weights = weights, algorithm = algorithm))
  load(file = "Data/GeoDistanceMatrix.RData")
  
  cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
  CandidateTransmitters_Departments_MinDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
    CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
    if(nrow(CandidateTransmitters_Departments_Subset) > 0){
      Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
        Distances=Distances_Matrix[data[i,Department],CandidateTransmitters_Departments_Subset[j,Department]]
      } 
    }else{
      Distances=NA
    }
    
  }
  
  cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
  CandidateTransmitters_Departments_MinGeoDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
    CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
    if(nrow(CandidateTransmitters_Departments_Subset) > 0){
      Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
        Distances=GeoDistanceMatrix[data[i,Department],CandidateTransmitters_Departments_Subset[j,Department]]
      } 
    }else{
      Distances=NA
    }
    
  }
  
  cat("Get Minimum Distance Between Episode and 1 Potential Infector\n")
  MinDistanceLocation=lapply(CandidateTransmitters_Departments_MinDistances, function(x) which.min(x))
  
  CandidateTransmitters_Departments_MinGeoDistances_Values=foreach(i=1:length(CandidateTransmitters_Departments_MinGeoDistances)) %do% {
    if(!is.na(CandidateTransmitters_Departments_MinGeoDistances[[i]])){
      CandidateTransmitters_Departments_MinGeoDistances_Subset=CandidateTransmitters_Departments_MinGeoDistances[[i]]
      MinDistanceLocation_Subset=MinDistanceLocation[[i]]
      CandidateTransmitters_Departments_MinGeoDistances_Value=CandidateTransmitters_Departments_MinGeoDistances_Subset[MinDistanceLocation_Subset]
    }else{
      CandidateTransmitters_Departments_MinGeoDistances[[i]]=NA
    }
     }
  
  cat(paste("Unlist, Remove Inf, Minimum Distances", i, "\n"))
  MinimumGeoDistances=unlist(CandidateTransmitters_Departments_MinGeoDistances_Values)
  
  return(MinimumGeoDistances)
}
getMinimumUnWeightedDistances_CandidateTransmitters_byWeek=function(i, CandidateTransmitters, weights, algorithm){
  ##############################################################################################
  #### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####
  cat("#### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####\n")
  
  cat("List of Departments of Episode and Candidate Transmitters\n")
  Department="Department"
  CandidateTransmitters_Departments=foreach(i=1:length(CandidateTransmitters)) %do% {
    CandidateTransmitters_Departments=CandidateTransmitters[[i]]
    CandidateTransmitters_Departments[Department]
  }
  
  cat("Distance Matrix Between Departments\n")
  Distances_Matrix=as.data.frame(distances(directed.graph_Dept, weights = weights, algorithm = algorithm))
  UnWeighted_Distances_Matrix=as.data.frame(distances(directed.graph_Dept, weights = NA, algorithm = "automatic"))
  
  cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
  CandidateTransmitters_Departments_MinDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
    CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
    if(nrow(CandidateTransmitters_Departments_Subset) > 0){
      Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
        Distances=Distances_Matrix[data[i,Department],CandidateTransmitters_Departments_Subset[j,Department]]
      } 
    }else{
      Distances=NA
    }
    
  }
  
  cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
  CandidateTransmitters_Departments_MinUnWeightedDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
    CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
    if(nrow(CandidateTransmitters_Departments_Subset) > 0){
      Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
        Distances=UnWeighted_Distances_Matrix[data[i,Department],CandidateTransmitters_Departments_Subset[j,Department]]
      } 
    }else{
      Distances=NA
    }
    
  }
  
  cat("Get Minimum Distance Between Episode and 1 Potential Infector\n")
  MinDistanceLocation=lapply(CandidateTransmitters_Departments_MinDistances, function(x) which.min(x))
  
  CandidateTransmitters_Departments_MinUnWeightedDistances_Values=foreach(i=1:length(CandidateTransmitters_Departments_MinUnWeightedDistances)) %do% {
    if(!is.na(CandidateTransmitters_Departments_MinUnWeightedDistances[[i]])){
      CandidateTransmitters_Departments_MinUnWeightedDistances_Subset=CandidateTransmitters_Departments_MinUnWeightedDistances[[i]]
      MinDistanceLocation_Subset=MinDistanceLocation[[i]]
      CandidateTransmitters_Departments_MinUnWeightedDistances_Value=CandidateTransmitters_Departments_MinUnWeightedDistances_Subset[MinDistanceLocation_Subset]
    }else{
      CandidateTransmitters_Departments_MinUnWeightedDistances[[i]]=NA
    }
     }
  
  cat(paste("Unlist, Remove Inf, Minimum Distances", i, "\n"))
  MinimumUnWeightedDistances=unlist(CandidateTransmitters_Departments_MinUnWeightedDistances_Values)
  
  return(MinimumUnWeightedDistances)
}
getMinimumDistances_CandidateTransmitters_byDay=function(i, CandidateTransmitters, weights, algorithm){
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

################
#### GET S1 ####

# getS1_ProportionIncidentEpisodesWithAtLeastOne_CandidateTransmitters_byWeek=function(i, CandidateTransmitters, weights, algorithm){
#   ##############################################################################################
#   #### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####
#   cat("#### STEP 3: Calculate Minimum (Network) Distance Between Episode and Candidate Transmitters ####\n")
#   
#   cat("List of Departments of Episode and Candidate Transmitters\n")
#   Department="Department"
#   CandidateTransmitters_Departments=foreach(i=1:length(CandidateTransmitters)) %do% {
#     CandidateTransmitters_Departments=CandidateTransmitters[[i]]
#     CandidateTransmitters_Departments[Department]
#   }
#   
#   cat("Distance Matrix Between Departments\n")
#   Distances_Matrix=as.data.frame(distances(directed.graph_Dept, weights = weights, algorithm = algorithm))
#   
#   cat("Distance Between Department of Episode and Candidate Transmitters Departments\n")
#   CandidateTransmitters_Departments_MinDistances=foreach(i=1:length(CandidateTransmitters_Departments)) %do% {
#     CandidateTransmitters_Departments_Subset=CandidateTransmitters_Departments[[i]]
#     if(nrow(CandidateTransmitters_Departments_Subset) > 0){
#       Distances=foreach(j=1:nrow(CandidateTransmitters_Departments_Subset), .combine='c') %do% {
#         Distances=Distances_Matrix[data[i,Department],CandidateTransmitters_Departments_Subset[j,Department]]
#       } 
#     }else{
#       Distances=NA
#     }
#     
#   }
#   
#   cat("Get Number of Incident Episodes With At Least 1 Potential Infector\n")
#   IncidentEpisodesWithAtLeast1Infector=foreach(i=1:length(CandidateTransmitters_Departments_MinDistances)) %do% {
#     length(CandidateTransmitters_Departments_MinDistances[[i]]) >= 1 & !is.na(CandidateTransmitters_Departments_MinDistances[[i]])[[1]]
#   }
#   PropTable=prop.table(table(unlist(IncidentEpisodesWithAtLeast1Infector)))
# 
#   return(PropTable)
# }

########################################################
#### GET GEO DISTANCE MATRIX ####

# DeptGeoCodes=read.csv(file="Data/All Dept Prefecture GeoCodes.csv", stringsAsFactors = F)[,c(1,7:8)]
# DeptGeoCodes[which(DeptGeoCodes$Number == "2A"),1] = 20
# DeptNumber=DeptGeoCodes$Number
# DeptNumber=str_pad(DeptNumber, 2, pad = "0")
# LatLong<-DeptGeoCodes[which(names(Distances_Matrix) %in% DeptNumber),2:3]
# LongLat<-LatLong[c("Longitude","Latitude")]
# runGeoDistanceMatrix=T
# if(runGeoDistanceMatrix){
#   GeoDistanceMatrix=array(dim=c(93,93))
#   runit=T
#   for (i in 1:93){
#     if(runit){
#       Distance<-distm(LongLat[i,1:2], LongLat[1:2],  fun=distHaversine)
#       Distance<-Distance/1000
#     }
#     GeoDistanceMatrix[i,]=Distance
#   }
#   GeoDistanceMatrix=as.data.frame(GeoDistanceMatrix)
#   rownames(GeoDistanceMatrix)=names(Distances_Matrix)
#   colnames(GeoDistanceMatrix)=names(Distances_Matrix)
#   save(GeoDistanceMatrix, file="Data/GeoDistanceMatrix.RData")
# }


###################################
#### GET PERMUTATIONS FUNCTION ####
getPermutatedData=function(data, Week){
  cat("Get possible candidate transmitters for each incident case/episode\n")
  dataPossibleCandidates=lapply(1:nrow(data), function(i){
    CaseDate=data[i,2]
    CaseID=data[i,1]
    data[data[,2] < CaseDate 
         & data[,2] >= (CaseDate-Week*7) 
         & data$Episode != CaseID,]})
  cat("For each list of possible candidates, shuffle rows for Departments\n")
  dataPermutations=foreach(i=1:length(dataPossibleCandidates)) %do% {
    if(nrow(dataPossibleCandidates[[i]]) > 0){
      dataPossibilities=dataPossibleCandidates[[i]]
      columnorder=names(dataPossibilities)
      dataPermutation=cbind(dataPossibilities[,c(1:13,16:21)], dataPossibilities[sample(1:nrow(dataPossibilities)),c(14:15)])
      dataPermutation=dataPermutation[,columnorder]
    }else{
      dataPossibleCandidates[[i]]=dataPossibleCandidates[[i]]
    }
  }
  return(dataPermutations)
}
getPermutatedData_Reassignment=function(data, Week){
  cat("Get possible candidate transmitters for each incident case/episode\n")
  dataPossibleCandidates=lapply(1:nrow(data), function(i){
    CaseDate=data[i,2]
    CaseID=data[i,1]
    data[data[,2] < CaseDate 
         & data[,2] >= (CaseDate-Week*7) 
         & data$Episode != CaseID,]})
  cat("For each list of possible candidates, reassign 1 out of 85 departments\n")
  dataPermutations=lapply(1:length(dataPossibleCandidates), function(i){
    if(nrow(dataPossibleCandidates[[i]]) > 0){
      dataPossibilities=dataPossibleCandidates[[i]]
      columnnames=names(dataPossibilities)
      departments=unique(data$Department)
      dataPermutation=cbind(dataPossibilities[,c(1:13)], sample(departments, size = nrow(dataPossibilities), replace = TRUE), dataPossibilities[,c(15:21)])
      colnames(dataPermutation)=columnnames
    }else{
      dataPermutation=dataPossibleCandidates[[i]]
    }
    return(dataPermutation)
  })
  return(dataPermutations)
}
getPermutatedData_byDays=function(data, Day){
  cat("Get possible candidate transmitters for each incident case/episode\n")
  dataPossibleCandidates=lapply(1:nrow(data), function(i){
    CaseDate=data[i,2]
    CaseID=data[i,1]
    data[data[,2] < CaseDate 
         & data[,2] >= (CaseDate-Day) 
         & data$Episode != CaseID,]})
  cat("For each list of possible candidates, reassign 1 out of 85 departments\n")
  dataPermutations=lapply(1:length(dataPossibleCandidates), function(i){
    if(nrow(dataPossibleCandidates[[i]]) > 0){
      dataPossibilities=dataPossibleCandidates[[i]]
      columnnames=names(dataPossibilities)
      departments=unique(data$Department)
      dataPermutation=cbind(dataPossibilities[,c(1:13)], sample(departments, size = nrow(dataPossibilities), replace = TRUE), dataPossibilities[,c(15:21)])
      colnames(dataPermutation)=columnnames
    }else{
      dataPermutation=dataPossibleCandidates[[i]]
    }
    return(dataPermutation)
  })
  return(dataPermutations)
}

############################################################
#### GET CANDIATE TRANSMITTERS FROM RANDOM PERMUTATIONS ####
# getCandidateTransmitters_Permutations_byWeek=function(data, Week){
#   cat("Permutate Data: Mechanisms & Department\n")
#   Permutation=cbind(data[,c(1:4,6:13)], data[sample(1:nrow(data)),c(5,14:21)])
#   
#   cat(paste("Set Number of Preceeding Weeks Equal to", Week, "\n"))
#   Week=Week
#   
#   cat("Criterion 1: Candidate Transmitter Occured at least N Weeks Before Episode\n")
#   Candidate_Transmitter_Permutations_byWeek=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-Week*7 & data$DateEpisode < data[i,2]),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Candidate Transmitters\n")
#   ImportationStatus="Imported"
#   Candidate_Transmitter_Permutations_byWeek_ImportationCorrection=foreach(i=1:length(Candidate_Transmitter_Permutations_byWeek)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "N" 
#     if(ConditionMet){
#       Candidate_Transmitter_Permutations_byWeek[[i]]=Candidate_Transmitter_Permutations_byWeek[[i]]
#     }
#   }
#   Candidate_Transmitter_Permutations_byWeek_ImportationCorrection[sapply(Candidate_Transmitter_Permutations_byWeek_ImportationCorrection, is.null)] = NULL
#   
#   return(Candidate_Transmitter_Permutations_byWeek_ImportationCorrection)
# }

###########################
#### CLEANING FUNCTION ####
CleaningFunction=function(x){
  x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
  x <- lapply(x, function(x) {x[c(1:length(x))]})
  return(x)
} 

##################################################################################
#### GET AVERAGE MINIMUM DISTANCE OF CANDIDATE TRANSMITTERS FROM PERMUTATIONS ####
getAverageMinDistances_CandidateTransmitters_Permutations_byWeek=function(MinimumDistances_PotentialInfector_Permutation){
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
CleaningFunction2=function(x){
  x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
  return(x)
}
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
get5thQuantiles_NonZeroDistances=function(Week, MinimumDistances, AllRandomMinimumDistances){
  MinimumDistances=lapply(MinimumDistances, function(x) x[x != 0])
  
  cat("Convert random permutations into dataframes\n")
  RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
  RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% {
    RandomSimulationsByDays_Clean_NonZeros=lapply(RandomSimulationsByDays_Clean[[i]], function(x) replace(x, x==0, NA))
    RandomSimulationsByDays_Dataframe=data.frame(RandomSimulationsByDays_Clean_NonZeros, row.names = NULL) 
  }
  
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
getMeanMinimumDistances=function(MinimumDistances, MinimumDistances_UnWeighted, MinimumDistances_GeoDist,
                                 AllPermutatationMinimumDistances, AllPermutatationMinimumDistances_UnWeighted, AllPermutatationMinimumDistances_GeoDist){
  Week=Week
 
  #Original
  #Weighted
  cat("Mean distance for every Week N for candidate transmitters\n")
  MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
  MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
  #UnWeighted
  cat("Mean unweighted distance for every Week N for candidate transmitters\n")
  MinimumDistances_UnWeighted_Clean=foreach(i=1:length(MinimumDistances_UnWeighted)) %do% CleaningFunction2(MinimumDistances_UnWeighted[[i]])
  MinimumDistances_UnWeighted_MeansByNWeeks=foreach(i=1:length(MinimumDistances_UnWeighted_Clean)) %do% mean(unlist(MinimumDistances_UnWeighted_Clean[[i]]), na.rm = T)
  #GeoDist
  cat("Mean geo distance for every Week N for candidate transmitters\n")
  MinimumDistances_GeoDist_Clean=foreach(i=1:length(MinimumDistances_GeoDist)) %do% CleaningFunction2(MinimumDistances_GeoDist[[i]])
  MinimumDistances_GeoDist_MeansByNWeeks=foreach(i=1:length(MinimumDistances_GeoDist_Clean)) %do% mean(unlist(MinimumDistances_GeoDist_Clean[[i]]), na.rm = T)
  
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
  #UnWeighted
  cat("Convert random simulations into dataframes\n")
  RandomSimulationsByWeeks_UnWeighted=foreach(i=1:length(AllPermutatationMinimumDistances_UnWeighted[[1]])) %do% lapply(AllPermutatationMinimumDistances_UnWeighted, `[[`, i) #get first elements i of each list
  RandomSimulationsByWeeks_UnWeighted_Clean=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted)) %do% CleaningFunction(RandomSimulationsByWeeks_UnWeighted[[i]])
  RandomSimulationsByWeeks_UnWeighted_Dataframes=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted_Clean)) %do% data.frame(RandomSimulationsByWeeks_UnWeighted_Clean[[i]], row.names = NULL) 
  cat(paste("Row Means of", Week, "permutations for every case\n"))
  RandomSimulationsByWeeks_UnWeighted_RowMeans=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted_Dataframes)) %do% rowMeans(RandomSimulationsByWeeks_UnWeighted_Dataframes[[i]], na.rm=TRUE)
  cat(paste("Min unweighted distance means of", Week, "permutations for every Week N\n"))
  RandomSimulationsByWeeks_UnWeighted_MeansByNWeeks=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted_RowMeans)) %do% mean(unlist(RandomSimulationsByWeeks_UnWeighted_RowMeans[[i]]), na.rm = T)
  #GeoDist
  cat("Convert random simulations into dataframes\n")
  RandomSimulationsByWeeks_GeoDist=foreach(i=1:length(AllPermutatationMinimumDistances_GeoDist[[1]])) %do% lapply(AllPermutatationMinimumDistances_GeoDist, `[[`, i) #get first elements i of each list
  RandomSimulationsByWeeks_GeoDist_Clean=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist)) %do% CleaningFunction(RandomSimulationsByWeeks_GeoDist[[i]])
  RandomSimulationsByWeeks_GeoDist_Dataframes=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist_Clean)) %do% data.frame(RandomSimulationsByWeeks_GeoDist_Clean[[i]], row.names = NULL) 
  cat(paste("Row Means of", Week, "permutations for every case\n"))
  RandomSimulationsByWeeks_GeoDist_RowMeans=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist_Dataframes)) %do% rowMeans(RandomSimulationsByWeeks_GeoDist_Dataframes[[i]], na.rm=TRUE)
  cat(paste("Min geo distance means of", Week, "permutations for every Week N\n"))
  RandomSimulationsByWeeks_GeoDist_MeansByNWeeks=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist_RowMeans)) %do% mean(unlist(RandomSimulationsByWeeks_GeoDist_RowMeans[[i]]), na.rm = T)
  
  cat("Get table\n")
  MinDistancesTableByMeanPerNWeeks=as.data.frame(cbind(unlist(MinimumDistances_MeansByNWeeks), unlist(MinimumDistances_UnWeighted_MeansByNWeeks), unlist(MinimumDistances_GeoDist_MeansByNWeeks),
                                                       unlist(RandomSimulationsByWeeks_MeansByNWeeks), unlist(RandomSimulationsByWeeks_UnWeighted_MeansByNWeeks), unlist(RandomSimulationsByWeeks_GeoDist_MeansByNWeeks)))
  colnames(MinDistancesTableByMeanPerNWeeks)=c("MinimumDistances_MeansByNWeeks", "MinimumDistances_UnWeighted_MeansByNWeeks", "MinimumDistances_GeoDist_MeansByNWeeks",
                                               "RandomSimulationsByWeeks_MeansByNWeeks", "RandomSimulationsByWeeks_UnWeighted_MeansByNWeeks", "RandomSimulationsByWeeks_GeoDist_MeansByNWeeks")
  
  return(MinDistancesTableByMeanPerNWeeks)
}

getMeanMinimumDistances_NonZeroDistances=function(MinimumDistances, MinimumDistances_UnWeighted, MinimumDistances_GeoDist, 
                                                  AllPermutatationMinimumDistances, AllPermutatationMinimumDistances_UnWeighted, AllPermutatationMinimumDistances_GeoDist){
  Week=Week
  
  #Original
  #Weighted
  cat("Mean distance for every Week N for candidate transmitters\n")
  MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
  MinimumDistances_MeansByNWeeks=foreach(i=1:length(MinimumDistances_Clean)) %do% {
    MinimumDistances_Unlist=unlist(MinimumDistances_Clean[[i]])
    MinimumDistances_NonZeros=MinimumDistances_Unlist[MinimumDistances_Unlist != 0]
    mean(MinimumDistances_NonZeros, na.rm = T) 
  }
  #UnWeighted
  cat("Mean unweighted distance for every Week N for candidate transmitters\n")
  MinimumDistances_UnWeighted_Clean=foreach(i=1:length(MinimumDistances_UnWeighted)) %do% CleaningFunction2(MinimumDistances_UnWeighted[[i]])
  MinimumDistances_UnWeighted_MeansByNWeeks=foreach(i=1:length(MinimumDistances_UnWeighted_Clean)) %do% {
    MinimumDistances_Unlist=unlist(MinimumDistances_UnWeighted_Clean[[i]])
    MinimumDistances_NonZeros=MinimumDistances_Unlist[MinimumDistances_Unlist != 0]
    mean(MinimumDistances_NonZeros, na.rm = T) 
  }
  #GeoDistance
  cat("Mean unweighted distance for every Week N for candidate transmitters\n")
  MinimumDistances_GeoDist_Clean=foreach(i=1:length(MinimumDistances_GeoDist)) %do% CleaningFunction2(MinimumDistances_GeoDist[[i]])
  MinimumDistances_GeoDist_MeansByNWeeks=foreach(i=1:length(MinimumDistances_GeoDist_Clean)) %do% {
    MinimumDistances_Unlist=unlist(MinimumDistances_GeoDist_Clean[[i]])
    MinimumDistances_NonZeros=MinimumDistances_Unlist[MinimumDistances_Unlist != 0]
    mean(MinimumDistances_NonZeros, na.rm = T) 
  }
  
  #Permutations
  #Weighted
  cat("Convert random simulations into dataframes\n")
  RandomSimulationsByWeeks=foreach(i=1:length(AllPermutatationMinimumDistances[[1]])) %do% lapply(AllPermutatationMinimumDistances, `[[`, i) #get first elements i of each list
  RandomSimulationsByWeeks_Clean=foreach(i=1:length(RandomSimulationsByWeeks)) %do% CleaningFunction(RandomSimulationsByWeeks[[i]])
  # RandomSimulationsByWeeks_Dataframes=foreach(i=1:length(RandomSimulationsByWeeks_Clean)) %do% data.frame(RandomSimulationsByWeeks_Clean[[i]], row.names = NULL) 
  cat(paste("Row Means of", Week, "permutations for every case\n"))
  RandomSimulationsByWeeks_RowMeans=foreach(i=1:length(RandomSimulationsByWeeks_Clean)) %do% {
    RandomSimulationsByWeeks_NonZeros=lapply(RandomSimulationsByWeeks_Clean[[i]], function(x) replace(x, x==0, NA))
    RandomSimulationsByWeeks_Dataframes=data.frame(RandomSimulationsByWeeks_NonZeros, row.names = NULL) 
    RandomSimulationsByWeeks_RowMeans=rowMeans(RandomSimulationsByWeeks_Dataframes, na.rm=TRUE)
  }
  
  cat(paste("Min distance means of", Week, "permutations for every Week N\n"))
  RandomSimulationsByWeeks_MeansByNWeeks=foreach(i=1:length(RandomSimulationsByWeeks_RowMeans)) %do% {
    RandomSimulationsByWeeks_RowMeans_Unlist=unlist(RandomSimulationsByWeeks_RowMeans[[i]])
    RandomSimulationsByWeeks_RowMeans_NonZeros=RandomSimulationsByWeeks_RowMeans_Unlist[RandomSimulationsByWeeks_RowMeans_Unlist != 0]
    mean(RandomSimulationsByWeeks_RowMeans_NonZeros, na.rm = T) 
  }
  #UnWeighted
  cat("Convert random simulations into dataframes\n")
  RandomSimulationsByWeeks_UnWeighted=foreach(i=1:length(AllPermutatationMinimumDistances_UnWeighted[[1]])) %do% lapply(AllPermutatationMinimumDistances_UnWeighted, `[[`, i) #get first elements i of each list
  RandomSimulationsByWeeks_UnWeighted_Clean=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted)) %do% CleaningFunction(RandomSimulationsByWeeks_UnWeighted[[i]])
  # RandomSimulationsByWeeks_UnWeighted_Dataframes=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted_Clean)) %do% data.frame(RandomSimulationsByWeeks_UnWeighted_Clean[[i]], row.names = NULL) 
  cat(paste("Row Means of", Week, "permutations for every case\n"))
  RandomSimulationsByWeeks_UnWeighted_RowMeans=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted_Clean)) %do% {
    RandomSimulationsByWeeks_NonZeros=lapply(RandomSimulationsByWeeks_UnWeighted_Clean[[i]], function(x) replace(x, x==0, NA))
    RandomSimulationsByWeeks_Dataframes=data.frame(RandomSimulationsByWeeks_NonZeros, row.names = NULL) 
    RandomSimulationsByWeeks_RowMeans=rowMeans(RandomSimulationsByWeeks_Dataframes, na.rm=TRUE)
  }
  cat(paste("Min distance means of", Week, "permutations for every Week N\n"))
  RandomSimulationsByWeeks_UnWeighted_MeansByNWeeks=foreach(i=1:length(RandomSimulationsByWeeks_UnWeighted_RowMeans)) %do% {
    RandomSimulationsByWeeks_RowMeans_Unlist=unlist(RandomSimulationsByWeeks_UnWeighted_RowMeans[[i]])
    RandomSimulationsByWeeks_RowMeans_NonZeros=RandomSimulationsByWeeks_RowMeans_Unlist[RandomSimulationsByWeeks_RowMeans_Unlist != 0]
    mean(RandomSimulationsByWeeks_RowMeans_NonZeros, na.rm = T) 
  }
  #GeoDist
  cat("Convert random simulations into dataframes\n")
  RandomSimulationsByWeeks_GeoDist=foreach(i=1:length(AllPermutatationMinimumDistances_GeoDist[[1]])) %do% lapply(AllPermutatationMinimumDistances_GeoDist, `[[`, i) #get first elements i of each list
  RandomSimulationsByWeeks_GeoDist_Clean=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist)) %do% CleaningFunction(RandomSimulationsByWeeks_GeoDist[[i]])
  # RandomSimulationsByWeeks_GeoDist_Dataframes=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist_Clean)) %do% data.frame(RandomSimulationsByWeeks_GeoDist_Clean[[i]], row.names = NULL) 
  cat(paste("Row Means of", Week, "permutations for every case\n"))
  RandomSimulationsByWeeks_GeoDist_RowMeans=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist_Clean)) %do% {
    RandomSimulationsByWeeks_NonZeros=lapply(RandomSimulationsByWeeks_GeoDist_Clean[[i]], function(x) replace(x, x==0, NA))
    RandomSimulationsByWeeks_Dataframes=data.frame(RandomSimulationsByWeeks_NonZeros, row.names = NULL) 
    RandomSimulationsByWeeks_RowMeans=rowMeans(RandomSimulationsByWeeks_Dataframes, na.rm=TRUE)
  }
  cat(paste("Min distance means of", Week, "permutations for every Week N\n"))
  RandomSimulationsByWeeks_GeoDist_MeansByNWeeks=foreach(i=1:length(RandomSimulationsByWeeks_GeoDist_RowMeans)) %do% {
    RandomSimulationsByWeeks_RowMeans_Unlist=unlist(RandomSimulationsByWeeks_GeoDist_RowMeans[[i]])
    RandomSimulationsByWeeks_RowMeans_NonZeros=RandomSimulationsByWeeks_RowMeans_Unlist[RandomSimulationsByWeeks_RowMeans_Unlist != 0]
    mean(RandomSimulationsByWeeks_RowMeans_NonZeros, na.rm = T) 
  }
  
  cat("Get table\n")
  MinDistancesTableByMeanPerNWeeks=as.data.frame(cbind(unlist(MinimumDistances_MeansByNWeeks), 
                                                       unlist(MinimumDistances_UnWeighted_MeansByNWeeks), 
                                                       unlist(MinimumDistances_GeoDist_MeansByNWeeks),
                                                       unlist(RandomSimulationsByWeeks_MeansByNWeeks), 
                                                       unlist(RandomSimulationsByWeeks_UnWeighted_MeansByNWeeks),
                                                       unlist(RandomSimulationsByWeeks_GeoDist_MeansByNWeeks)))
  colnames(MinDistancesTableByMeanPerNWeeks)=c("MinimumDistances_MeansByNWeeks", "MinimumDistances_UnWeighted_MeansByNWeeks", "MinimumDistances_GeoDist_MeansByNWeeks", 
                                               "RandomSimulationsByWeeks_MeansByNWeeks", "RandomSimulationsByWeeks_UnWeighted_MeansByNWeeks", "RandomSimulationsByWeeks_GeoDist_MeansByNWeeks")
  
  return(MinDistancesTableByMeanPerNWeeks)
}

getMeanMinimumDistances_Simple=function(MinimumDistances, AllPermutatationMinimumDistances){
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


#################################################################################################################################################################
################################## OLD ##########################################################################################################################
#################################################################################################################################################################

# ################################################
# #### CPE DATA WITH MECHANISM AND CLASS INFO ####
# getCPEData=function(){
#   
#   ###########################################
#   #### STEP 1a: GET DATA ON CPE EPISODES ####
#   
#   cat("Upload all data\n")
#   data=read.csv(file=paste("Data/ALL_EPISODES (clean).csv", sep=""), header=T, sep=";", stringsAsFactors=FALSE)
#   
#   cat("Change dates to R dates\n")
#   data$Department=as.character(data$Department)
#   data$DateEpisode <- as.Date(data$DateEpisode, format = "%d/%m/%Y")
#   data$DateOneCase <- as.Date(data$DateOneCase, format = "%d/%m/%Y")
#   data$DateMoreOneCase <- as.Date(data$DateMoreOneCase, format = "%d/%m/%Y")
#   data$DateImported <- as.Date(data$DateImported, format = "%d/%m/%Y")
#   data$DateMoreFiveCases <- as.Date(data$DateMoreFiveCases, format = "%d/%m/%Y")
#   
#   # cat("Select data by dates\n")
#   # cat("Choose start date\n")
#   # startDate="2015-01-01"
#   #
#   # cat("Choose end date\n")
#   # endDate="2015-06-30"
#   #
#   # cat("Get subset of data\n")
#   # data=data[which(data$DateEpisode > as.Date(startDate) & data$DateEpisode < as.Date(endDate)),]
#   
#   ###########################################
#   #### STEP 1b: MECHANISMS OF RESISTANCE ####
#   
#   cat("Split Multiple Mechanisms\n")
#   Mechanisms=str_split_fixed(data$Mechanism, " ", 3)
#   Mechanisms[Mechanisms == ""] = NA
#   colnames(Mechanisms)=c("FirstMechanism", "SecondMechanism", "ThirdMechanism")
#   Mechanisms=as.data.frame(Mechanisms, stringsAsFactors=FALSE)
#   
#   cat("Class Type\n")
#   ClassA=c("SME", "IMI", "NMC", "GES", "KPC", "SHV", "SFC")
#   ClassB=c("VIM", "IMP", "NDM")
#   ClassD="OXA"
#   
#   cat("Add First Class Types\n")
#   Mechanisms$FirstClass=NA
#   Mechanisms$FirstClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassA"
#   Mechanisms$FirstClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassB"
#   Mechanisms$FirstClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$FirstMechanism)] = "ClassD"
#   
#   cat("Add Second Class Types\n")
#   Mechanisms$SecondClass=NA
#   Mechanisms$SecondClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassA"
#   Mechanisms$SecondClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassB"
#   Mechanisms$SecondClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$SecondMechanism)] = "ClassD"
#   
#   cat("Add Third Class Types\n")
#   Mechanisms$ThirdClass=NA
#   Mechanisms$ThirdClass[grepl(paste(ClassA, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassA"
#   Mechanisms$ThirdClass[grepl(paste(ClassB, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassB"
#   Mechanisms$ThirdClass[grepl(paste(ClassD, collapse = "|"), Mechanisms$ThirdMechanism)] = "ClassD"
#   
#   cat("Add Multiple Mechanisms to Data\n")
#   data=cbind(data, Mechanisms)
#   
#   return(data)
# }
# 
# #######################################
# #### POTENTIAL INFECTORS FUNCTIONS ####
# getPotentialInfectors_byMechanism=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-NDays & data$DateEpisode < data[i,2]),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE mechanism of resistance\n")
#   FirstResistanceMechanism="FirstMechanism"
#   PotentialInfectors_byFirstMechanism=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
#   }
#   SecondResistanceMechanism="SecondMechanism"
#   PotentialInfectors_bySecondMechanism=foreach(i=1:length(PotentialInfectors_byFirstMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
#   }
#   ThirdResistanceMechanism="ThirdMechanism"
#   PotentialInfectors_byThirdMechanism=foreach(i=1:length(PotentialInfectors_bySecondMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
#   }
#   return(PotentialInfectors_byThirdMechanism)
# }
# getPotentialInfectors_byClass=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-NDays & data$DateEpisode < data[i,2]),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE Class of resistance\n")
#   FirstResistanceClass="FirstClass"
#   PotentialInfectors_byFirstClass=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
#   }
#   SecondResistanceClass="SecondClass"
#   PotentialInfectors_bySecondClass=foreach(i=1:length(PotentialInfectors_byFirstClass)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstClass[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
#   }
#   ThirdResistanceClass="ThirdClass"
#   PotentialInfectors_byThirdClass=foreach(i=1:length(PotentialInfectors_bySecondClass)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondClass[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
#   }
#   return(PotentialInfectors_byThirdClass)
# }
# 
# ########################################
# #### GET NETWORK DISTANCES FUNCTION ####
# getMinimumDistances=function(i, PotentialInfectors, weights, algorithm){
#   ##############################################################################################
#   #### STEP 3: Calculate Minimum (Network) Distance Between Episode and Potential Infectors ####
#   cat("#### STEP 3: Calculate Minimum (Network) Distance Between Episode and Potential Infectors ####\n")
#   
#   cat("List of Departments of Episode and Potential Infectors\n")
#   Department="Department"
#   PotentialInfectors_Departments=foreach(i=1:length(PotentialInfectors)) %do% {
#     PotentialInfector_Departments=PotentialInfectors[[i]]
#     PotentialInfector_Departments[Department]
#   }
#   
#   cat("Distance Matrix Between Departments\n")
#   Distances_Matrix=as.data.frame(distances(directed.graph_Dept, weights = weights, algorithm = algorithm))
#   cat("Mean Distance Between Departments\n")
#   mean_distance(directed.graph_Dept)
#   
#   cat("Distance Between Department of Episode and Potential Infectors Departments\n")
#   PotentialInfectors_Departments_MinDistances=foreach(i=1:length(PotentialInfectors_Departments)) %do% {
#     PotentialInfectors_Departments_Subset=PotentialInfectors_Departments[[i]]
#     Distances=foreach(j=1:length(PotentialInfectors_Departments_Subset)) %do% {
#       Distances=Distances_Matrix[data[i,Department],PotentialInfectors_Departments_Subset[j,Department]]
#     }
#     MinDistance=lapply(Distances, min)
#   }
#   
#   cat(paste("Unlist, Remove Inf, Minimum Distances", i, "\n"))
#   MinimumDistances=unlist(PotentialInfectors_Departments_MinDistances)
#   #MinimumDistances=MinimumDistances[which(is.finite(MinimumDistances))]
#   
#   return(MinimumDistances)
# }
# 
# #################################################
# #### GET RANDOM POTENTIAL INFECTORS FUNCTION ####
# getRandomPotentialInfectors=function(NDays, PotentialInfectors){
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-NDays & data$DateEpisode < data[i,2]),]
#   
#   cat("Criterion 4: Same Number of Random Infectors as Original Potential Infector\n")
#   randomRows = function(df,n){
#     return(df[sample(nrow(df),n),])
#   }
#   PotentialInfectors_Size=lapply(PotentialInfectors, nrow)
#   Random_PotentialInfectors=foreach(i=1:length(PotentialInfectors_byDays)) %do% randomRows(PotentialInfectors_byDays[[i]], PotentialInfectors_Size[[i]])
#   return(Random_PotentialInfectors)
# }
# getRandomMinimumDistances=function(PotentialInfectors){
#   cat(paste("Get Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   PotentialInfectors_RI=foreach(i=1:MaxDays) %do% getRandomPotentialInfectors(i, PotentialInfectors[[i]])
#   cat(paste("Get Minimum Distances for Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   MinimumDistances_RI=foreach(i=1:length(PotentialInfectors_RI)) %do% getMinimumDistances(i, PotentialInfectors_RI[[i]], weights = weights, algorithm = algorithm)
#   return(MinimumDistances_RI)
# }
# 
# #############################################
# #### AVERAGE RANDOM SIMULATIONS FUNCTION ####
# 
# #### CLEANING FUNCTION ####
# CleaningFunction=function(x){
#   x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
#   x <- lapply(x, function(x) {x[c(1:2686)]})
#   return(x)
# } 
# CleaningFunction2=function(x){
#   x <- lapply(x, function(j) if(is.numeric(j)) ifelse(is.infinite(j), NA, j) else j)
#   return(x)
# } 
# getAverageRandomSimulationsMinDistances=function(AllRandomMinimumDistances){
#   cat(paste("Average", Nruns, "runs\n"))
#   RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
#   
#   cat("Run Cleaning Function for Random Simulations and Original Results\n")
#   RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
#   
#   cat("Convert sub-list of 100 values into dataframe to facilitate per case averages\n")
#   RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
#   
#   cat("Row Means of 100 simulations for every case\n")
#   RandomSimulationsByDays_RowMeans=foreach(i=1:length(RandomSimulationsByDays_Dataframes)) %do% rowMeans(RandomSimulationsByDays_Dataframes[[i]], na.rm=TRUE)
#   
#   return(RandomSimulationsByDays_RowMeans)
# }
# 
# ##################################################
# #### WILCOXON PAIRED RANKED SUM TEST FUNCTION ####
# getWilcoxonPairedRankTestPValues=function(i, MinimumDistances, AllRandomMinimumDistances){
#   cat("Clean out Inf values from MinimumDistances\n")
#   MinimumDistances_Clean=CleaningFunction(MinimumDistances)
#   cat(paste("Combine Non-Random and Random Minimum Distances for", i, "Days\n"))
#   CombinedMinimumDistances=cbind(MinimumDistances_Clean[[i]], AllRandomMinimumDistances[[i]])
#   cat("Test only for lengths greater than 3\n")
#   length=nrow(CombinedMinimumDistances)
#   if(length > 3L){
#     cat(paste("Run Test for", i, "Days\n"))
#     CombinedMinimumDistances_WilcoxonTest=wilcox.test(CombinedMinimumDistances[,1], CombinedMinimumDistances[,2], paired = T)
#     #H0: distributions of calculated potential infector min distances is the same as the distribution of min distances of random potetial infectors
#   }else{
#     CombinedMinimumDistances_WilcoxonTest$p.value<-NA
#   }
#   return(CombinedMinimumDistances_WilcoxonTest$p.value)
# }
# 
# ####################################################
# #### PROPORTIONS OF VALUES UNDER 5TH PERCENTILE ####
# get5thQuantiles=function(Week, MinimumDistances, AllRandomMinimumDistances){
#   cat("Convert random permutations into dataframes\n")
#   RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
#   RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
#   RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
#   cat("Get proportion cases that have values smaller than the 5th percentile of simulations for each week N\n")
#   ProportionsTable=foreach(n=1:Week, .combine = "cbind") %do% {
#     day=foreach(i=1:nrow(data)) %do% (MinimumDistances[[n]][i] <= quantile(RandomSimulationsByDays_Dataframes[[n]][i,], 0.05, na.rm=T)[[1]])
#     day=unlist(day)
#     resultsTable=prop.table(table(day))
#   }
#   return(ProportionsTable)
# }
# getMeanMinimumDistances=function(MinimumDistances, AllRandomMinimumDistances){
#   Week=Week
#   
#   cat("Convert random simulations into dataframes\n")
#   RandomSimulationsByDays=foreach(i=1:length(AllRandomMinimumDistances[[1]])) %do% lapply(AllRandomMinimumDistances, `[[`, i) #get first elements i of each list
#   RandomSimulationsByDays_Clean=foreach(i=1:length(RandomSimulationsByDays)) %do% CleaningFunction(RandomSimulationsByDays[[i]])
#   RandomSimulationsByDays_Dataframes=foreach(i=1:length(RandomSimulationsByDays_Clean)) %do% data.frame(RandomSimulationsByDays_Clean[[i]], row.names = NULL) 
#   cat(paste("Row Means of", Week, "permutations for every case\n"))
#   RandomSimulationsByDays_RowMeans=foreach(i=1:length(RandomSimulationsByDays_Dataframes)) %do% rowMeans(RandomSimulationsByDays_Dataframes[[i]], na.rm=TRUE)
#   
#   cat(paste("Min distance means of", Week, "permutations for every Week N\n"))
#   RandomSimulationsByDays_MeansByNDays=foreach(i=1:length(RandomSimulationsByDays_RowMeans)) %do% mean(unlist(RandomSimulationsByDays_RowMeans[[i]]), na.rm = T)
#   
#   cat("Mean distance for every Week N for potential infectors\n")
#   MinimumDistances_Clean=foreach(i=1:length(MinimumDistances)) %do% CleaningFunction2(MinimumDistances[[i]])
#   MinimumDistances_MeansByNDays=foreach(i=1:length(MinimumDistances_Clean)) %do% mean(unlist(MinimumDistances_Clean[[i]]), na.rm = T)
#   
#   cat("Get table\n")
#   MinDistancesTableByMeanPerNDays=as.data.frame(cbind(unlist(MinimumDistances_MeansByNDays), unlist(RandomSimulationsByDays_MeansByNDays)))
#   colnames(MinDistancesTableByMeanPerNDays)=c("MinimumDistances_MeansByNDays", "RandomSimulationsByDays_MeansByNDays")
#   
#   return(MinDistancesTableByMeanPerNDays)
# }
# 
# ###################################################
# #### POTENTIAL INFECTORS FUNCTIONS (DAY RANGE) ####
# getPotentialInfectors_DayRange_byMechanism=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE mechanism of resistance\n")
#   FirstResistanceMechanism="FirstMechanism"
#   PotentialInfectors_byFirstMechanism=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
#   }
#   SecondResistanceMechanism="SecondMechanism"
#   PotentialInfectors_bySecondMechanism=foreach(i=1:length(PotentialInfectors_byFirstMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
#   }
#   ThirdResistanceMechanism="ThirdMechanism"
#   PotentialInfectors_byThirdMechanism=foreach(i=1:length(PotentialInfectors_bySecondMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
#   }
#   return(PotentialInfectors_byThirdMechanism)
# }
# getPotentialInfectors_DayRange_byClass=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE Class of resistance\n")
#   FirstResistanceClass="FirstClass"
#   PotentialInfectors_byFirstClass=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
#   }
#   SecondResistanceClass="SecondClass"
#   PotentialInfectors_bySecondClass=foreach(i=1:length(PotentialInfectors_byFirstClass)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstClass[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
#   }
#   ThirdResistanceClass="ThirdClass"
#   PotentialInfectors_byThirdClass=foreach(i=1:length(PotentialInfectors_bySecondClass)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondClass[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
#   }
#   return(PotentialInfectors_byThirdClass)
# }
# getRandomPotentialInfectors_DayRange=function(NDays, PotentialInfectors){
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 4: Same Number of Random Infectors as Original Potential Infector\n")
#   randomRows = function(df,n){
#     return(df[sample(nrow(df),n),])
#   }
#   PotentialInfectors_Size=lapply(PotentialInfectors, nrow)
#   Random_PotentialInfectors=foreach(i=1:length(PotentialInfectors_byDays)) %do% randomRows(PotentialInfectors_byDays[[i]], PotentialInfectors_Size[[i]])
#   return(Random_PotentialInfectors)
# }
# getRandomMinimumDistances_DayRange=function(PotentialInfectors){
#   cat(paste("Get Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   PotentialInfectors_RI=foreach(i=1:MaxDays) %do% getRandomPotentialInfectors_DayRange(i, PotentialInfectors[[i]])
#   cat(paste("Get Minimum Distances for Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   MinimumDistances_RI=foreach(i=1:length(PotentialInfectors_RI)) %do% getMinimumDistances(i, PotentialInfectors_RI[[i]], weights = weights, algorithm = algorithm)
#   return(MinimumDistances_RI)
# }
# 
# ###################################################
# #### POTENTIAL INFECTORS FUNCTIONS (DAY RANGE) ####
# getPotentialInfectors_DayRange_byMechanism=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE mechanism of resistance\n")
#   FirstResistanceMechanism="FirstMechanism"
#   PotentialInfectors_byFirstMechanism=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceMechanism] %in% data[i,FirstResistanceMechanism],]
#   }
#   SecondResistanceMechanism="SecondMechanism"
#   PotentialInfectors_bySecondMechanism=foreach(i=1:length(PotentialInfectors_byFirstMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceMechanism] %in% data[i,SecondResistanceMechanism],]
#   }
#   ThirdResistanceMechanism="ThirdMechanism"
#   PotentialInfectors_byThirdMechanism=foreach(i=1:length(PotentialInfectors_bySecondMechanism)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondMechanism[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceMechanism] %in% data[i,ThirdResistanceMechanism],]
#   }
#   return(PotentialInfectors_byThirdMechanism)
# }
# getPotentialInfectors_DayRange_byClass=function(NDays){
#   
#   ##############################################
#   #### STEP 2: Find All Potential Infectors ####
#   cat("#### STEP 2: Find All Potential Infectors ####\n")
#   
#   #A potential infector
#   # - occured N days before episode (7 days, 14 days, 21 days, 28 days, 35 days, 42 days)
#   # - shares the same mechanism OR shares same class (test mechanism)
#   # - in the same bacteria?? (will not test yet)
#   
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 2: Imported Episodes Do Not Have Potential Infectors\n")
#   ImportationStatus="Imported"
#   PotentialInfectors_byDays_ImportationCorrection=foreach(i=1:length(PotentialInfectors_byDays)) %do% {
#     ConditionMet=data[i,ImportationStatus] == "O"
#     if(ConditionMet){
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]][0,]
#     }else{
#       PotentialInfectors_byDays[[i]]=PotentialInfectors_byDays[[i]]
#     }
#   }
#   
#   cat("Criterion 3: Same CPE Class of resistance\n")
#   FirstResistanceClass="FirstClass"
#   PotentialInfectors_byFirstClass=foreach(i=1:length(PotentialInfectors_byDays_ImportationCorrection)) %do% {
#     PotentialInfectors=PotentialInfectors_byDays_ImportationCorrection[[i]]
#     PotentialInfectors[PotentialInfectors[,FirstResistanceClass] %in% data[i,FirstResistanceClass],]
#   }
#   SecondResistanceClass="SecondClass"
#   PotentialInfectors_bySecondClass=foreach(i=1:length(PotentialInfectors_byFirstClass)) %do% {
#     PotentialInfectors=PotentialInfectors_byFirstClass[[i]]
#     PotentialInfectors[PotentialInfectors[,SecondResistanceClass] %in% data[i,SecondResistanceClass],]
#   }
#   ThirdResistanceClass="ThirdClass"
#   PotentialInfectors_byThirdClass=foreach(i=1:length(PotentialInfectors_bySecondClass)) %do% {
#     PotentialInfectors=PotentialInfectors_bySecondClass[[i]]
#     PotentialInfectors[PotentialInfectors[,ThirdResistanceClass] %in% data[i,ThirdResistanceClass],]
#   }
#   return(PotentialInfectors_byThirdClass)
# }
# getRandomPotentialInfectors_DayRange=function(NDays, PotentialInfectors){
#   cat(paste("Set Number of Preceeding Days Equal to", NDays, "\n"))
#   NDays=NDays
#   
#   cat("Criterion 1: Episode Occured N Days Before\n")
#   PotentialInfectors_byDays=foreach(i=1:nrow(data)) %do% data[which(data$DateEpisode >= data[i,2]-(NDays+1) & data$DateEpisode <= data[i,2]-(NDays-1)),]
#   
#   cat("Criterion 4: Same Number of Random Infectors as Original Potential Infector\n")
#   randomRows = function(df,n){
#     return(df[sample(nrow(df),n),])
#   }
#   PotentialInfectors_Size=lapply(PotentialInfectors, nrow)
#   Random_PotentialInfectors=foreach(i=1:length(PotentialInfectors_byDays)) %do% randomRows(PotentialInfectors_byDays[[i]], PotentialInfectors_Size[[i]])
#   return(Random_PotentialInfectors)
# }
# getRandomMinimumDistances_DayRange=function(PotentialInfectors){
#   cat(paste("Get Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   PotentialInfectors_RI=foreach(i=1:MaxDays) %do% getRandomPotentialInfectors_DayRange(i, PotentialInfectors[[i]])
#   cat(paste("Get Minimum Distances for Random Potential Infectors for Day 1 to", MaxDays, "\n"))
#   MinimumDistances_RI=foreach(i=1:length(PotentialInfectors_RI)) %do% getMinimumDistances(i, PotentialInfectors_RI[[i]], weights = weights, algorithm = algorithm)
#   return(MinimumDistances_RI)
# }