#########################
##### Suburst Chart #####
#########################

# install.packages("sunburstR")
# install.packages("pitchRx")

library("sunburstR")
library("pitchRx")
library("dplyr")

source("CommonHeader.R")
source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))


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

##################
##### Counts #####

# YearCounts=data.frame(Var1=c("2012","2013","2014","2015"), 
#                       Freq=c(nrow(data2012), nrow(data2013), nrow(data2014), nrow(data2015)),
#                       stringsAsFactors = F)
# ImportedCounts=data.frame(Var1=c("Imported", "Local"),
#                           Freq=c(nrow(data[which(data$Imported == "O"),]),nrow(data[which(data$Imported == "N"),])),
#                           stringsAsFactors = F)
# MechanismCounts=as.data.frame(table(as.vector(as.matrix(data[,c("FirstMechanism", "SecondMechanism", "ThirdMechanism")]))),
#                               stringsAsFactors = F)
# 
# FinalTable=rbind(YearCounts, ImportedCounts, MechanismCounts)
# FinalTable$Depth=c(rep(1, nrow(YearCounts)),
#                    rep(2, nrow(ImportedCounts)),
#                    rep(3, nrow(MechanismCounts)))




data[which(data$Imported == "O"),"Imported"] = "Imported"
data[which(data$Imported == "N"),"Imported"] = "Local"

Sequence=as.data.frame(cbind(substr(data[,"DateEpisode"], 1, 4),
           data$Imported,
           substr(data[,"FirstMechanism"], 1, 3),
           substr(data[,"SecondMechanism"], 1, 3),
           substr(data[,"ThirdMechanism"], 1, 3)),
           stringsAsFactors = F)

# Sequence=Sequence[!(Sequence$V3 == "GES" | Sequence$V3 == "IMI" | Sequence$V3 == "IMP"),] 

SequencePaste=foreach(i=1:nrow(Sequence), .combine = "rbind") %do% {
  Seq=Sequence[i,]
  Seq=Seq[!is.na(Seq)]
  SeqPaste=paste0(Seq, collapse = "-")
}

SequenceFreq=as.data.frame(table(SequencePaste))
SequenceDepth=unlist(lapply(str_split(SequenceFreq$SequencePaste,"-"),length))
SequenceTable=cbind(SequenceFreq, SequenceDepth)
colnames(SequenceTable)=c("event", "count", "depth")

sb <- SequenceTable %>%
  arrange(desc(depth), event) %>%
  sunburst(percent = TRUE, legend = list(w=100),
           legendOrder = unique(unlist(str_split(SequenceTable[,1],"-"))),
           explanation = "function(d){return d.data.name}"
  )
sb


#### Plot
library(RColorBrewer)
library("htmlwidgets")

#Colors
colors=c("#79ff77",
         "#144349", "#1f6771", "#2b8b99", "#36afc1",
         "#8c8c8c", "#a6a6a6",
         "#ffb977", "	#ff9f44", "#ff4447", "#ff9192")
colors=brewer.pal(11, "RdYlGn")
colors=c("#1a1334", 
         "#26294a", "#01545a", "#017351", "#03c383",
         "#aad962", "#fbbf45", 
         "#ef6a32","#ed0345","#a12a5e","#710162")

#Plot
htmlwidgets::onRender(
  sunburst(SequenceTable, withD3 = T,
           colors = colors),
  "
  function(el,x){
  d3.select(el).select('.sunburst-togglelegend').property('checked', true);
  d3.select(el).select('.sunburst-legend').style('visibility', '');
}
  "
)

