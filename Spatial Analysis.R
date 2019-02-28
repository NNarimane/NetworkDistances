##########################################
################# Spatial ################
#### Analysis of Departments of Pairs ####

source("CommonHeader.R")

###################################
#### GET FUNCTIONS SOURCE CODE ####

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

#############################
#### DATA AND PARAMETERS ####

source("NetworkDistances/DataParameters.R")

############################
# Spatial Summary function #

getSpatialSummary=function(Year){
  Year=Year
  folder=paste0("Feb 14 Results/", as.character(Year), " Results")
  PairsDistanceTable=read.csv(file=paste0(writingDir,folder,"/PairsDistanceInfoTable.csv"))
  
  # Number of different departments link
  UniqueSourceDepts=length(unique(PairsDistanceTable$SourceDepartment))
  UniqueTargetDepts=length(unique(PairsDistanceTable$TargetDepartment))
  UniqueSourceDepts_Imported=length(unique(PairsDistanceTable[which(PairsDistanceTable$SourceImported == "O"),]$SourceDepartment))
  UniqueTargetDepts_Imported=length(unique(PairsDistanceTable[which(PairsDistanceTable$SourceImported == "O"),]$TargetDepartment))
  
  # Old Regions
  library(readxl)
  Dept_Region_GeoCodes = read_excel("C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Data/Dept Region GeoCodes.xlsx")
  colnames(Dept_Region_GeoCodes)[3]="OldRegions"
  # New regions
  NewRegions=data.frame(OldRegions=c("Bourgogne", "Franche-Comté",
                                     "Aquitaine","Limousin","Poitou-Charente",
                                     "Alsace","Champagne","Lorraine",
                                     "Languedoc","Midi-Pyrénées",
                                     "Haute-Normandie","Normandie","Basse-Normandie",
                                     "Nord","Pas-de-Calais","Picardie",
                                     "Auvergne","Rhône-Alpes",
                                     "Provence-Alpes-Côte d'Azur",
                                     "Centre",
                                     "Corse",
                                     "Bretagne",
                                     "Ile-de-France",
                                     "Pays-de-la-Loire"), 
                        NewRegions=c("Bourgogne-Franche-Comté","Bourgogne-Franche-Comté",
                                     "Nouvelle-Aquitaine","Nouvelle-Aquitaine","Nouvelle-Aquitaine",
                                     "Grand Est","Grand Est","Grand Est",
                                     "Occitanie","Occitanie",
                                     "Normandie","Normandie","Normandie",
                                     "Hauts-de-France","Hauts-de-France","Hauts-de-France",
                                     "Auvergne-Rhône-Alpes","Auvergne-Rhône-Alpes",
                                     "Provence-Alpes-Côte d'Azur",
                                     "Centre-Val-de-Loire",
                                     "Corse",
                                     "Bretagne",
                                     "Ile-de-France",
                                     "Pays-de-la-Loire"))
  # Merge
  Dept_Region_GeoCodes=merge(Dept_Region_GeoCodes, NewRegions, by="OldRegions", all.x=T)
  
  # Add regions
  PairsDistanceTable_Regions=merge(PairsDistanceTable, Dept_Region_GeoCodes, by.x="SourceDepartment", by.y="Department", all.x=T)
  PairsDistanceTable_Regions=merge(PairsDistanceTable_Regions, Dept_Region_GeoCodes, by.x="TargetDepartment", by.y="Department", all.x=T)
  colnames(PairsDistanceTable_Regions)=c("Target.Department","Source.Department","X","Source","Source.Imported","ShortestPathDistance",
                                         "Source.Cases","Target","Mechanism","Source.OldRegion","Source.DeptName","Source.Latitude","Source.Longitude",
                                         "Source.NewRegion","Target.OldRegion","Target.DeptName","Target.Latitude","Target.Longitude","Target.NewRegions")
  PairsDistanceTable_Regions=PairsDistanceTable_Regions[,c("X","Source","Source.Department","Source.DeptName","Source.Imported","Source.Cases","Source.OldRegion","Source.Latitude","Source.Longitude",
                                                           "Source.NewRegion","ShortestPathDistance","Mechanism",
                                                           "Target","Target.Department","Target.DeptName","Target.OldRegion","Target.Latitude","Target.Longitude","Target.NewRegions")]
  PairsDistanceTable_Regions=PairsDistanceTable_Regions[order(PairsDistanceTable_Regions$X),]
  
  # Proportion of same dept source and target
  PercentSameDeptPairs=prop.table(table(PairsDistanceTable_Regions$Source.Department == PairsDistanceTable_Regions$Target.Department))[2]
  # Proportion of same region source and target
  PercentSameOldRegionPairs=prop.table(table(PairsDistanceTable_Regions$Source.OldRegion == PairsDistanceTable_Regions$Target.OldRegion))[2]
  PercentSameNewRegionPairs=prop.table(table(PairsDistanceTable_Regions$Source.NewRegion == PairsDistanceTable_Regions$Target.NewRegion))[2]
  
  # Distance between pairs
  library("geosphere")
  PairsDistanceTable_Regions$GeoDistanceBetweenPairs=foreach(i=1:nrow(PairsDistanceTable_Regions), .combine = "c") %do% distm(PairsDistanceTable_Regions[i,c("Source.Longitude","Source.Latitude")], PairsDistanceTable_Regions[i,c("Target.Longitude","Target.Latitude")],  fun=distHaversine)/1000
  
  # Mean geo distance among non-shared department pairs
  MeanDistAllPairs=mean(PairsDistanceTable_Regions$GeoDistanceBetweenPairs)
  MeanDistAllPairs
  
  CI1 =  1.96*sd(PairsDistanceTable_Regions$GeoDistanceBetweenPairs)/sqrt(length(PairsDistanceTable_Regions$GeoDistanceBetweenPairs))
  Lower_CI1=MeanDistAllPairs - 1.96*sd(PairsDistanceTable_Regions$GeoDistanceBetweenPairs)/sqrt(length(PairsDistanceTable_Regions$GeoDistanceBetweenPairs))
  Upper_CI1=MeanDistAllPairs + 1.96*sd(PairsDistanceTable_Regions$GeoDistanceBetweenPairs)/sqrt(length(PairsDistanceTable_Regions$GeoDistanceBetweenPairs))
  
  # Mean geo distance among non-shared department pairs
  MeanDistNonSameDeptPairs=mean(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs)
  MeanDistNonSameDeptPairs
  
  CI2 =  1.96*sd(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs)/sqrt(length(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs))
  Lower_CI2=MeanDistNonSameDeptPairs - 1.96*sd(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs)/sqrt(length(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs))
  Upper_CI2=MeanDistNonSameDeptPairs + 1.96*sd(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs)/sqrt(length(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs))
  
  
  MeanDistNonSameOldRegionPairs=mean(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.OldRegion != PairsDistanceTable_Regions$Target.OldRegion),]$GeoDistanceBetweenPairs)
  MeanDistNonSameOldRegionPairs
  MeanDistNonSameNewRegionPairs=mean(PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.NewRegion != PairsDistanceTable_Regions$Target.NewRegion),]$GeoDistanceBetweenPairs)
  MeanDistNonSameNewRegionPairs
  
  DistNonSameDeptPairs=PairsDistanceTable_Regions[which(PairsDistanceTable_Regions$Source.Department != PairsDistanceTable_Regions$Target.Department),]$GeoDistanceBetweenPairs
  NumberNonSameDeptPairs=length(DistNonSameDeptPairs)
  hist(DistNonSameDeptPairs, breaks=seq(from=0, to=900, by=100), freq = F, ylim = c(0,0.005))
  DistNonSameDeptPairs=data.frame(DistNonSameDeptPairs, Cut=cut(DistNonSameDeptPairs, seq(from=0, to=900, by=100)))
  DistributionDistNonSameDeptPairs=data.frame(prop.table(table(DistNonSameDeptPairs$Cut)))
  rownames(DistributionDistNonSameDeptPairs)=DistributionDistNonSameDeptPairs$Var1
  DistributionDistNonSameDeptPairs$Var1=NULL
  colnames(DistributionDistNonSameDeptPairs)=as.character(Year)
  
  # Get department pairs analysis summary table
  SpatialSummary=t(data.frame(UniqueSourceDepts,UniqueTargetDepts,
                              UniqueSourceDepts_Imported,UniqueTargetDepts_Imported,
                              PercentSameDeptPairs, PercentSameOldRegionPairs,
                              PercentSameNewRegionPairs, MeanDistAllPairs, CI1,
                              Lower_CI1, Upper_CI1, MeanDistNonSameDeptPairs, CI2,
                              Lower_CI2, Upper_CI2, 
                              MeanDistNonSameOldRegionPairs, MeanDistNonSameNewRegionPairs,
                              NumberNonSameDeptPairs))
  colnames(SpatialSummary)=as.character((Year))
  SpatialSummary=rbind(SpatialSummary, DistributionDistNonSameDeptPairs)
  
  #Save
  write.csv(SpatialSummary, file=paste0(writingDir,folder,"/SpatialSummary.csv"))
  
  return(SpatialSummary)
}

#####################
# Get all summaries #

AllSpatialSummaries=foreach(i=c(2012,2013,2014,2015), .combine = "cbind") %do% getSpatialSummary(i)
# Save
write.table(AllSpatialSummaries, file=paste0(writingDir, "Feb 14 Results/AllSpatialSummaries Updated.csv"))

######################
# Shared Region/Dept #

# library(plotly)

AllSpatialSummaries=read.csv(file=paste0(writingDir, "Feb 14 Results/AllSpatialSummaries Updated.csv"), sep = " ")

SameDept=data.frame(Year=as.integer(c(2012,2013,2014,2015)),
                    Type=as.character(rep("Linked episodes in \n same county",4)),
                    PercentSameDeptPairs=t(AllSpatialSummaries["PercentSameDeptPairs",])*100)
colnames(SameDept)=c("Year", "Type", "Percent")
SameRegion=data.frame(Year=as.integer(c(2012,2013,2014,2015)),
                      Type=as.character(rep("Linked episodes in \n same region",4)),
                    PercentSameNewRegionPairs=t(AllSpatialSummaries["PercentSameNewRegionPairs",])*100)
colnames(SameRegion)=c("Year", "Type", "Percent")



SharedDeptsRegions=rbind(SameDept, SameRegion)
SharedDeptsRegions$Type=as.character(SharedDeptsRegions$Type)
SharedDeptsRegions$Percent=round(SharedDeptsRegions$Percent, digits = 0)


#GGplot
colfunc <- colorRampPalette(c("deepskyblue", "aquamarine1","aquamarine4"))
col1 = colfunc(8)

P1=ggplot(SharedDeptsRegions, aes(as.character(Year), Type)) +   
  geom_point(aes(size = Percent, fill=Percent, stroke = 5, colour = Percent), shape=21) +
  geom_text(aes(label=c(paste(SharedDeptsRegions$Percent, "%", sep=""))), size=3.5) +
  scale_fill_gradient(low = "aquamarine1") +
  scale_colour_gradient(low = "aquamarine1") +
  theme_minimal() +
  scale_size(range = c(10, 25)) +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size=7, vjust=-10),
        axis.text.x=element_text(face="bold", size=13),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 19, 0, 0)) +
  scale_x_discrete(expand=c(0,0.465), position = "top")
P1


#################
# Mean Distance #

MeanDistances2=MeanDistances[1:4,]
P2=ggplot(MeanDistances2, aes(x=as.character(Year), y=Distances, group=Group)) +
  geom_line(aes(colour=Group), size=1.3) +
  geom_errorbar(aes(ymin=Distances-CI, ymax=Distances+CI, colour=Group), size=0.8,
                width=0.1) +
  scale_color_manual("Distances between:", values=c("aquamarine4")) +
  theme_minimal() +
  labs(y="Mean distances (km) between episodes [95% CI]") +
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size=8),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=8),
        plot.margin = margin(0, 25, 0, 0)) +
  scale_x_discrete(expand=c(0,0.462))
P2

###########################
# Stacked Distance Ranges #


DistanceRanges=data.frame(Year=(c(2012,2013,2014,2015)),
                          t(AllSpatialSummaries[19:27,]))
colnames(DistanceRanges)=c("Year","1-100","100-200",
                           "200-300", "300-400", "400-500",
                           "500-600", "600-700", "700-800",
                           "800-900")

# melt the data frame for plotting
Test <- melt(DistanceRanges, id.vars='Year')

colfunc <- colorRampPalette(c("deepskyblue", "aquamarine1","aquamarine4"))
col = colfunc(9)

# Stacked
P3=ggplot(Test, aes(Year, value, fill = variable)) +   
  geom_bar(stat = "identity") +
  scale_fill_manual(values=col,
                    labels=c("1-100","100-200",
                             "200-300", "300-400", "400-500",
                             "500-600", "600-700", "700-800",
                             "800-900")) +
  theme_minimal() +
  labs(y="Proportion of geographic distances between \nlinked episodes in different counties") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size=8, margin = margin(t = 0, r = 0, b = 0, l = -4), vjust=-1),
        axis.text.y=element_blank(),
        axis.text.x=element_text(face="bold", size=13),
        legend.position="bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.title=element_text(size=9, face="bold"))  +
  guides(fill=guide_legend(title="Distances (km)"))
P3




library("cowplot")
finalplot=plot_grid(P1, P2, P3,ncol=1, rel_heights=c(1,1,2))
finalplot

save_plot("Spatial Plot 5.png", finalplot,
          ncol = 1, # we're saving a grid plot of 2 columns
          nrow = 3, # and 2 rows
          base_aspect_ratio = 1.8)



