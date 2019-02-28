
getCandidateTransmitters_CleanedUp=function(data, Time){
    cat(paste("Set Number of Preceeding Days Equal to", Time, "\n"))
    Day=Time
    
    cat("Criterion 1: Candidate Transmitters Occured N Days Before Incident Episode\n")
    if(NonPermutation){
      cat("For Observed Data\n")
      cat(paste("Sliding", WeekSlide, "Week by", Day,"Day\n"))
      CandidateTransmitters=lapply(1:nrow(data), function(i){
          CaseDate=data[i,2]
          CaseID=data[i,1]
          data[data[,2] <= (CaseDate-Day) 
               & data[,2] > (CaseDate-WeekSlide*7-Day) 
               & data$Episode != CaseID,]})
      }else{
      cat("For Permutated Data\n")
      columnorder=names(data_original)
      dataPermutation=cbind(data_original[,c(1:13,16:21)], data_original[sample(1:nrow(data_original)),c(14:15)])
      dataPermutation=dataPermutation[,columnorder]
      dataPermutation$Department=as.character(dataPermutation$Department)
      data=dataPermutation

      cat(paste("Sliding", WeekSlide, "Week by", Day,"\n"))
      CandidateTransmitters=lapply(1:nrow(data), function(i){
          CaseDate=data[i,2]
          CaseID=data[i,1]
          data[data[,2] <= (CaseDate-Day) 
               & data[,2] > (CaseDate-WeekSlide*7-Day) 
               & data$Episode != CaseID,]})
      }
      
  cat("Criterion 2: Imported Episodes Do Not Have Candidate Transmitters\n")
  ImportationStatus="Imported"
  CandidateTransmitters_ImportationCorrection=foreach(i=1:length(CandidateTransmitters)) %do% {
        if(data[i,ImportationStatus] == "N"){
          CandidateTransmitters[[i]]=CandidateTransmitters[[i]]
    }
  }
  
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
    
  return(Final_CandidateTransmitters)
}

######################################################################################

getMinimumDistances_CleanedUp=function(i, CandidateTransmitters, weights, algorithm){
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
          Distances=Distances_Matrix[CandidateTransmitters_Departments_Subset[j,Department],data_original[i,Department]]
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