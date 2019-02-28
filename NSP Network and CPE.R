##############################
#### Network and CPE Data ####
##############################

source("CommonHeader.R")

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###################
##### Network #####

# Network with data
load(file=paste0("Department Network with CPE Episodes.RData"))
V(directed.graph_Dept)$CPE=as.integer(V(directed.graph_Dept)$CPE)

####################
##### CPE Data #####

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

###############################
##### Potential Infectors #####

PairsDistanceTable=read.csv(file=paste0(writingDir, "2012-2015 All Results/PairsDistanceInfoTable.csv"))
Freq2012to2015=as.data.frame(table(table(PairsDistanceTable$PotentialInfector)), stringsAsFactors = F)
Freq2012to2015$Var1=as.numeric(Freq2012to2015$Var1)
Freq2012to2015$Freq=as.numeric(Freq2012to2015$Freq)

TemporalData=data.frame(Source=as.character(PairsDistanceTable$PiDepartment), 
                        Target=as.character(PairsDistanceTable$EiDepartment),
                        time=as.numeric(PairsDistanceTable$PotentialInfector),
                        stringsAsFactors = F)
TemporalData$Source=str_pad(TemporalData$Source, 2, side="left", pad="0")
TemporalData$Target=str_pad(TemporalData$Target, 2, side="left", pad="0")


SourceFreq=as.data.frame(table(TemporalData$Source), stringsAsFactors=F)
colnames(SourceFreq)=c("SourceDept", "SourceFreq")
SourceFreq=SourceFreq[match(V(directed.graph_Dept)$name,SourceFreq$SourceDept),]
SourceFreq$SourceDept=V(directed.graph_Dept)$name

TargetFreq=as.data.frame(table(TemporalData$Target), stringsAsFactors=F)
colnames(TargetFreq)=c("TargetDept", "TargetFreq")
TargetFreq=TargetFreq[match(V(directed.graph_Dept)$name,TargetFreq$TargetDept),]
TargetFreq$TargetDept=V(directed.graph_Dept)$name

###################

# Add source and target depts
V(directed.graph_Dept)$SourceSize <- SourceFreq$SourceFreq
V(directed.graph_Dept)$TargetSize <- TargetFreq$TargetFreq

# Dataframe, layout, node size
meta <- data.frame("name" = as.character(V(directed.graph_Dept)$name), 
                   "lon" = as.numeric(V(directed.graph_Dept)$lon), 
                   "lat" = as.numeric(V(directed.graph_Dept)$lat), stringsAsFactors = F)
lo <- (as.matrix(meta[,2:3]))

# Size
SourceSize<-setNames(c(log(as.numeric(V(directed.graph_Dept)$SourceSize))*12),c(V(directed.graph_Dept)$names))
SourceSize[is.na(SourceSize)] = 0

TargetSize<-setNames(c(log(as.numeric(V(directed.graph_Dept)$TargetSize))*12),c(V(directed.graph_Dept)$names))
TargetSize[is.na(TargetSize)] = 0

Source=F
if(Source){
  node.size=SourceSize
  node.size=node.size+5
  size=cut(as.numeric(V(directed.graph_Dept)$SourceSize), breaks=c(0,1,50,100,150,200,250,300,400), right=F)
  levels(size)
  Title="Potential Infectors"
  # Vertex gradient color
  a=0.7
  fine = 50 # this will adjust the resolving power.
  Colors=c("black", 
           rgb(38/255,41/255,74/255,a), 
           rgb(1/255,115/255,81/255,a),
           rgb(3/255,195/255,131/255,a),
           rgb(251/255,191/255,69/255,a),
           rgb(239/255,106/255,50/255,a),
           rgb(161/255,42/255,94/255,a),
           rgb(113/255,1/255,98/255,a))
  pal = colorRampPalette(Colors,alpha=TRUE)
  #this gives you the colors you want for every point
  graphCol = pal(fine)[as.numeric(cut(node.size, breaks = fine))]
}else{
  node.size=TargetSize
  node.size=node.size+5
  size=cut(as.numeric(V(directed.graph_Dept)$TargetSize), breaks=c(0,1,50,100,150), right=F)
  levels(size)
  Title="Incident Episodes"
  # Vertex gradient color
  a=0.7
  fine = 50 # this will adjust the resolving power.
  Colors=c("black", 
           rgb(38/255,41/255,74/255,a), 
           rgb(1/255,115/255,81/255,a),
           rgb(3/255,195/255,131/255,a))
  pal = colorRampPalette(Colors,alpha=TRUE)
  #this gives you the colors you want for every point
  graphCol = pal(fine)[as.numeric(cut(node.size, breaks = fine))]
}


####################
### Plot Network ###

originalpar=par()

par(mar = c(0,0,0,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')


# Main plot
dev.copy(png,paste0("Network CPE ", Title,".png"), height=8, width=8, units="in", res=300)
map('france', fill = TRUE, col = "white")
plot.igraph(directed.graph_Dept, 
            layout=lo, add = TRUE, 
            rescale = FALSE,
            vertex.size = as.matrix(node.size),
            # vertex.color=adjustcolor("SkyBlue2", alpha.f = .5),
            vertex.color=graphCol,
            vertex.label=NA,
            # vertex.label.cex = 0.7,
            vertex.frame.color = NA, 
            # vertex.label.color = 'black', 
            # vertex.label.dist = 0, 
            # vertex.label.degree = 1, 
            # vertex.label.font=2,
            # vertex.label.family="arial",
            edge.arrow.size = 0, 
            edge.arrow.width = 0,
            edge.color = rgb(190/255,190/255,190/255, 0.5),
            # edge.width = (E(directed.graph_Dept)$weight / 10),
            edge.width = 0.005
)

# Legend
if(Source){
  legend("bottomleft",
         title=paste0("Number of \n",Title),
         cex=1,
         legend=c("0", "1 - 50", "50 - 100", "100 - 150", "150 - 200",
                  "200 - 250", "250 - 300", "300+"),
         col=Colors,
         pt.cex=seq(from=0.75, to=2.5, by=0.25),
         pch=19,
         bty="n")
}else{
  legend("bottomleft",
         title=paste0("Number of \n",Title),
         cex=1,
         legend=c("0", "1 - 50", "50 - 100", "100+"),
         col=Colors,
         pt.cex=seq(from=0.75, to=2.5, by=0.25),
         pch=19,
         bty="n")
}

dev.off()

# Zoom
dev.copy(png,paste0("Network CPE ", Title," ZOOM.png"), height=8, width=10, units="in", res=300)
par(mar=c(0,0,0,0))
map('france', fill = TRUE, col = "white", xlim = c(1.4,3.5), ylim = c(48.2,49.1))
plot.igraph(directed.graph_Dept, 
            layout=lo, add = TRUE, 
            rescale = FALSE,
            vertex.size = as.matrix(node.size)/2.5,
            # vertex.color=adjustcolor("SkyBlue2", alpha.f = .5),
            vertex.color=graphCol,
            vertex.label=NA,
            # vertex.label.cex = 0.7,
            vertex.frame.color = NA, 
            edge.arrow.size = 0, 
            edge.arrow.width = 0,
            edge.color = rgb(190/255,190/255,190/255, 0.5),
            # edge.width = (E(directed.graph_Dept)$weight / 10),
            edge.width = 0.005
)
dev.off()

##############################
##### Multiple Spreading #####

FreqPI=as.data.frame(table((PairsDistanceTable$PotentialInfector)), stringsAsFactors = F)
Freq2012to2015=as.data.frame(table(table(PairsDistanceTable$PotentialInfector)), stringsAsFactors = F)

# MaxEvent=FreqPI$Var1[which(FreqPI$Freq == max(FreqPI$Freq))]
# MaxEvents=TemporalData[which(TemporalData$time == MaxEvent),]
# V(directed.graph_Dept)$MaxEvent <- ifelse(V(directed.graph_Dept)$name %in% MaxEvents$Target, "orange", "lightgrey")
# V(directed.graph_Dept)$MaxEvent[(which(V(directed.graph_Dept)$name %in% MaxEvents$Source))] <- "red"

#Assign colors
a=0.7
Colors=data.frame(Size=as.numeric(Freq2012to2015$Var1), 
             Colors=as.character(c(rgb(26/255,19/255,52/255,a), 
               rgb(38/255,41/255,74/255,a),
               rgb(1/255,115/255,81/255,a),
               rgb(3/255,195/255,131/255,a),
               rgb(251/255,191/255,69/255,a),
               rgb(239/255,106/255,50/255,a),
               rgb(161/255,42/255,94/255,a),
               rgb(113/255,1/255,98/255,a))), stringsAsFactors = F)
FreqPI$Var1=as.numeric(FreqPI$Var1)
ColorsMerged=merge(FreqPI, Colors, by.x="Freq", by.y="Size", all.x=T)
ColorsMergedDept=merge(ColorsMerged, TemporalData, by.x="Var1", by.y="time", all.x=T)

Colors1=ColorsMergedDept[which(ColorsMergedDept$Freq == 1),]
Colors2=ColorsMergedDept[which(ColorsMergedDept$Freq == 2),]
Colors3=ColorsMergedDept[which(ColorsMergedDept$Freq == 3),]
Colors4=ColorsMergedDept[which(ColorsMergedDept$Freq == 4),]
Colors5=ColorsMergedDept[which(ColorsMergedDept$Freq == 5),]
Colors6=ColorsMergedDept[which(ColorsMergedDept$Freq == 6),]
Colors7=ColorsMergedDept[which(ColorsMergedDept$Freq == 7),]
Colors9=ColorsMergedDept[which(ColorsMergedDept$Freq == 9),]


# V(directed.graph_Dept)$ColorsEventSize = ColorsMergedDept$Freq
V(directed.graph_Dept)$ColorsEvent = "lightgrey"
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors1$Target))] <- Colors[1,2]
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors2$Target))] <- Colors[2,2]
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors3$Target))] <- Colors[3,2]
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors4$Target))] <- Colors[4,2]
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors5$Target))] <- Colors[5,2]
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors6$Target))] <- Colors[6,2]
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors7$Target))] <- Colors[7,2]
V(directed.graph_Dept)$ColorsEvent[(which(V(directed.graph_Dept)$name %in% Colors9$Target))] <- Colors[8,2]
#Size
V(directed.graph_Dept)$ColorsEventSize = 1
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors1$Target))] <- 1*5
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors2$Target))] <- 2*5
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors3$Target))] <- 3*5
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors4$Target))] <- 4*5
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors5$Target))] <- 5*5
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors6$Target))] <- 6*5
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors7$Target))] <- 7*5
V(directed.graph_Dept)$ColorsEventSize[(which(V(directed.graph_Dept)$name %in% Colors9$Target))] <- 8*5


originalpar=par()

par(mar = c(0,0,0,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')

dev.copy(png,'Network CPE Multilple Spreading.png', height=8, width=8, units="in", res=300)

map('france', fill = TRUE, col = "white")
plot.igraph(directed.graph_Dept, 
            layout=lo, add = TRUE, 
            rescale = FALSE,
            vertex.size = 30,
            # vertex.color=adjustcolor("SkyBlue2", alpha.f = .5),
            vertex.color=V(directed.graph_Dept)$ColorsEvent,
            vertex.label=NA,
            # vertex.label.cex = 0.7,
            vertex.frame.color = NA, 
            # vertex.label.color = 'black', 
            # vertex.label.dist = 0, 
            # vertex.label.degree = 1, 
            # vertex.label.font=2,
            # vertex.label.family="arial",
            edge.arrow.size = 0, 
            edge.arrow.width = 0,
            # edge.color = rgb(190/255,190/255,190/255, 0.5),
            edge.color=rgb(190/255,190/255,190/255, 0.5),
            # edge.width = (E(directed.graph_Dept)$weight / 10),
            edge.width = 0.5
)


# Legend
legend("bottomleft",
       title="Size of Largest \nMultiple Spreading \nEvent",
       cex=1,
       legend=c("None","1", "2", "3", "4",
                "5", "6", "7", "9"),
       col=c("lightgrey",
             rgb(26/255,19/255,52/255,a), 
             rgb(38/255,41/255,74/255,a), 
             # rgb(1/255,84/255,90/255,a), 
             rgb(1/255,115/255,81/255,a), 
             rgb(3/255,195/255,131/255,a),
             # rgb(170/255,217/255,98/255,a), 
             rgb(251/255,191/255,69/255,a), 
             rgb(239/255,106/255,50/255,a),
             # rgb(237/255,3/255,69/255,a),
             rgb(161/255,42/255,94/255,a),
             rgb(113/255,1/255,98/255,a)),
       pt.cex=1.25,
       pch=19,
       bty="n")


dev.off()

# Zoom
dev.copy(png,'Network CPE Multilple Spreading ZOOM.png', height=8, width=10, units="in", res=300)
par(mar=margin(0,0,0,0))
map('france', fill = TRUE, col = "white", xlim = c(1.4,3.5), ylim = c(48.2,49.1))
plot.igraph(directed.graph_Dept, 
            layout=lo, add = TRUE, 
            rescale = FALSE,
            vertex.size = 20,
            # vertex.color=adjustcolor("SkyBlue2", alpha.f = .5),
            vertex.color=V(directed.graph_Dept)$ColorsEvent,
            vertex.label=NA,
            # vertex.label.cex = 0.7,
            vertex.frame.color = NA, 
            edge.arrow.size = 0, 
            edge.arrow.width = 0,
            edge.color = rgb(190/255,190/255,190/255, 0.5),
            # edge.width = (E(directed.graph_Dept)$weight / 10),
            edge.width = 0.005,
            xlim = c(46,50),
            ylim = c(2,4)
)
dev.off()
