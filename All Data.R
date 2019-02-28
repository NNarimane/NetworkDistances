################
##### Data #####

cat("Choose Year\n")
Year=2012
startDate=paste0(as.character(Year),"-01-01")
endDate=paste0(as.character(Year+1),"-01-01")
data2012=getCPEData()

cat("Choose Year\n")
Year=2013
startDate=paste0(as.character(Year),"-01-01")
endDate=paste0(as.character(Year+1),"-01-01")
data2013=getCPEData()

cat("Choose Year\n")
Year=2014
startDate=paste0(as.character(Year),"-01-01")
endDate=paste0(as.character(Year+1),"-01-01")
data2014=getCPEData()

cat("Choose Year\n")
Year=2015
startDate=paste0(as.character(Year),"-01-01")
endDate=paste0(as.character(Year+1),"-01-01")
data2015=getCPEData()

cat("Merge\n")
data=rbind(data2012, data2013, data2014, data2015)

cat("Fix Episode Number\n")
data$Episode=1:nrow(data)