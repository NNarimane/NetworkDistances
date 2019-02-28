############################
##### Polar Area Chart #####
############################

library(ggplot2)

source("CommonHeader.R")
source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))
source("NetworkDistances/DataParameters.R")

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

###################

data[which(data$Imported == "O"),"Imported"] = "Imported"
data[which(data$Imported == "N"),"Imported"] = "Local"

Sequence=as.data.frame(cbind(substr(data[,"DateEpisode"], 1, 4),
                             data$Imported,
                             substr(data[,"FirstMechanism"], 1, 3),
                             substr(data[,"SecondMechanism"], 1, 3),
                             substr(data[,"ThirdMechanism"], 1, 3)),
                       stringsAsFactors = F)

# Sequence=Sequence[!(Sequence$V3 == "GES" | Sequence$V3 == "IMI" | Sequence$V3 == "IMP"),] 

X=as.data.frame(table(Sequence$V1))
X=cbind(X, rep("X", nrow(X)))
colnames(X)=c("Event", "Freq", "Type")
prop.table(X$Freq)

Y=as.data.frame(table(Sequence$V2))
Y=cbind(Y, rep("Y", nrow(Y)))
colnames(Y)=c("Event", "Freq", "Type")
prop.table(Y$Freq)

Z=as.data.frame(table(Sequence$V3))
Z=cbind(Z, rep("Z", nrow(Z)))
colnames(Z)=c("Event", "Freq", "Type")
prop.table(Z$Freq)

SeqFinal=rbind.data.frame(X, Y, Z)

###################

# colors=c("#79ff77",
#          "#144349", "#1f6771", "#2b8b99", "#36afc1",
#          "#8c8c8c", "#a6a6a6",
#          "#ffb977", "	#ff9f44", "#ff4447", "#ff9192")
# colors=brewer.pal(11, "RdYlGn")
# colors=c("#1a1334", 
#          "#26294a", "#01545a", "#017351", "#03c383",
#          "#aad962", "grey","grey", "grey", "#fbbf45",
#          "#ef6a32","#ed0345","#710162")
colors=c("#1a1334",
         "#26294a", "#01545a", "#017351", "#03c383",
         "#aad962", "grey","grey", "grey", "#0f2342",
         "#2d3057", "#01666d", "#01865f")
# colors=c("#1a1334", 
#          "#26294a", "#01545a", "#017351", "#03c383",
#          "#aad962", "grey","grey", "grey", "#163462", 
#          "#3a3e71", "#017880", "#019a6c")


dev.copy(png,'Polar Coordinate Chart 3.png', height=8, width=9, units="in", res=300)
ggplot(SeqFinal, aes(x = Type, y = Freq, fill = Event, label=Event))+
  geom_bar(stat = "identity") +
  coord_polar(theta="y") +
  scale_fill_manual(values=colors) +
  theme_void()
dev.off()

# ggsave("Polar Coordinate Chart 2.jpg", plot = last_plot(), path="C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/")


ggplot(SeqFinal, aes(x = Type, y = Freq, fill = Event, label=Event)) +
  geom_bar(stat = "identity") +
  coord_polar(theta="y") +
  scale_fill_manual(values=colors) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent") # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg,
    , axis.title.x=element_blank()
    , axis.text.x=element_blank()
    , axis.ticks.x=element_blank()
    , axis.title.y=element_blank()
    , axis.text.y=element_blank()
    , axis.ticks.y=element_blank()
)

ggsave("Polar Coordinate Chart.png", plot = last_plot(), path="C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/",
       bg="transparent")
