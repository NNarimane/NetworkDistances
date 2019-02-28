##############################
#### Network and CPE Data ####
##############################

source("CommonHeader.R")

source("NetworkDistances/Can CPE episodes be explained by transfer network (Functions).R", 
       local = FALSE, verbose = getOption("verbose"))

###################
##### Network #####

cat("Upload Department Network with Edge Weights Over Stays\n")
load("Data/Department Network (Edge Weights Over Stays) 2014 Fixed.RData")

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

####################
##### CPE Data #####

CPEbyDept=data.frame(table(data$Department))
CPEbyDept$Var1=as.character(CPEbyDept$Var1)
CPEbyDept=CPEbyDept[match(V(directed.graph_Dept)$name,CPEbyDept$Var1),]
CPEbyDept$Var1=V(directed.graph_Dept)$name

# Add attribute
V(directed.graph_Dept)$CPE <- as.character(CPEbyDept$Freq)

#####################
##### GeoCoding #####

DeptGeoCodes=read.csv(file="Data/All Dept Prefecture GeoCodes.csv", stringsAsFactors = F)[,c(1,7:8)]

#Remove extra depts 97+
DeptGeoCodes=DeptGeoCodes[1:96,]
#Change 2A & 2B to 20 and add
Dept20=as.character(apply(DeptGeoCodes[20:21,2:3], 2, mean))
Dept20=c(20, Dept20)
DeptGeoCodes=rbind(DeptGeoCodes, Dept20)
#Pad depts with "0"
DeptGeoCodes$Number=str_pad(DeptGeoCodes$Number, 2, pad = "0")
#Merge vertices with lat/long
# vertices=as.data.frame(V(directed.graph_Dept)$name)
# mergedGeoCodes=merge(vertices, DeptGeoCodes, by.x="V(directed.graph_Dept)$name", by.y="Number")

#Set vertice latitude
DeptGeoCodes=DeptGeoCodes[which(DeptGeoCodes$Number %in% V(directed.graph_Dept)$name),]
DeptGeoCodes=DeptGeoCodes[match(V(directed.graph_Dept)$name,DeptGeoCodes$Number),]

V(directed.graph_Dept)$lat=DeptGeoCodes$Latitude
V(directed.graph_Dept)$lon=DeptGeoCodes$Longitude

#Save graph
# save(directed.graph_Dept, file=paste0("Department Network with CPE Episodes.RData"))
# write.graph(directed.graph_Dept, "Department Network with CPE Episodes.gml", format="gml")


###########################

# Network with data
# load(file=paste0("Department Network with CPE Episodes.RData"))
V(directed.graph_Dept)$CPE=as.integer(V(directed.graph_Dept)$CPE)

# Dataframe, layout, node size
meta <- data.frame("name" = as.character(V(directed.graph_Dept)$name), 
                   "lon" = as.numeric(V(directed.graph_Dept)$lon), 
                   "lat" = as.numeric(V(directed.graph_Dept)$lat), stringsAsFactors = F)
lo <- (as.matrix(meta[,2:3]))
node.size<-setNames(c(log(as.numeric(V(directed.graph_Dept)$CPE))*12),c(V(directed.graph_Dept)$names))
node.size[is.na(node.size)] = 0
node.size=node.size+5

################
### Plot Map ###

originalpar=par()

par(mar = c(0,0,0,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')


dev.copy(png,'Network CPE Plot.png', height=8, width=8, units="in", res=300)

map('france', fill = TRUE, col = "white")

####################
### Plot Network ###

col2rgb("grey", alpha=T)

a=0.7

# Vertex gradient color
fine = 50 # this will adjust the resolving power.
# pal = colorRampPalette(c(rgb(26/255,19/255,52/255,a), 
#                          rgb(38/255,41/255,74/255,a), 
#                          # rgb(1/255,84/255,90/255,a), 
#                          rgb(1/255,115/255,81/255,a), 
#                          rgb(3/255,195/255,131/255,a),
#                          # rgb(170/255,217/255,98/255,a), 
#                          rgb(251/255,191/255,69/255,a), 
#                          rgb(239/255,106/255,50/255,a),
#                          # rgb(237/255,3/255,69/255,a),
#                          rgb(161/255,42/255,94/255,a),
#                          rgb(113/255,1/255,98/255,a)), alpha=TRUE)

pal=colorRampPalette(c("black", rgb(22/255,85/255,187/255,a),
                       rgb(14/255,54/255,119/255,a),
                       rgb(11/255,44/255,96/255,a),
                       rgb(6/255,23/255,51/255,a),
                       rgb(0/255,39/255,28/255,a),
                       rgb(1/255,90/255,63/255,a),
                       rgb(1/255,115/255,81/255,a)),
                     alpha=T)

#this gives you the colors you want for every point
graphCol = pal(fine)[as.numeric(cut(node.size, breaks = fine))]

plot.igraph(directed.graph_Dept, 
            layout=lo, add = TRUE, 
            rescale = FALSE,
            vertex.size = as.matrix(node.size),
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


# Legend
size=cut(as.numeric(V(directed.graph_Dept)$CPE), breaks=c(0,1,50,100,150,200,250,300,400), right=F)
levels(size)
legend("bottomleft",
       title="CPE Episodes \n in France, 2012-2015",
       cex=1,
       legend=c("0", "1 - 50", "50 - 100", "100 - 150", "150 - 200",
                "200 - 250", "250 - 300", "300+"),
       col=c("black",
         rgb(22/255,85/255,187/255,a),
             rgb(14/255,54/255,119/255,a),
             rgb(11/255,44/255,96/255,a),
             rgb(6/255,23/255,51/255,a),
             rgb(0/255,39/255,28/255,a),
             rgb(1/255,90/255,63/255,a),
             rgb(1/255,115/255,81/255,a)),
       pt.cex=seq(from=0.5, to=2.6, by=0.3),
       pch=19,
       bty="n")

dev.off()


# Zoom
dev.copy(png,'Network CPE Plot ZOOM sans label 2.png', height=8, width=10, units="in", res=300)
map('france', fill = TRUE, col = "white", xlim = c(1.4,3.5), ylim = c(48.2,49.1))
plot.igraph(directed.graph_Dept, 
            layout=lo, add = TRUE, 
            rescale = FALSE,
            vertex.size = as.matrix(node.size)/4,
            # vertex.color=adjustcolor("SkyBlue2", alpha.f = .5),
            vertex.color=graphCol,
            # vertex.label.color="white",
            vertex.label=NA,
            # vertex.label.cex = 0.7,
            vertex.frame.color = NA, 
            edge.arrow.size = 0, 
            edge.arrow.width = 0,
            edge.color = rgb(190/255,190/255,190/255, 0.5),
            # edge.width = (E(directed.graph_Dept)$weight / 10),
            edge.width = 0.1
)
dev.off()


# #################################
# ### Multiple Spreading Events ###
# 
# PairsDistanceTable=read.csv(file=paste0(writingDir, "2012-2015 All Results/PairsDistanceInfoTable.csv"))
# Freq2012to2015=as.data.frame(table(table(PairsDistanceTable$PotentialInfector)), stringsAsFactors = F)
# Freq2012to2015$Var1=as.numeric(Freq2012to2015$Var1)
# Freq2012to2015$Freq=as.numeric(Freq2012to2015$Freq)
# 
# TemporalData=data.frame(Source=as.character(PairsDistanceTable$PiDepartment), 
#                         Target=as.character(PairsDistanceTable$EiDepartment),
#                         time=as.numeric(PairsDistanceTable$PotentialInfector),
#                         stringsAsFactors = F)
# TemporalData$Source=str_pad(TemporalData$Source, 2, side="left", pad="0")
# TemporalData$Target=str_pad(TemporalData$Target, 2, side="left", pad="0")
# 
# ###################################################################################
# 
# # Network of All Spreading Events
# infect=TemporalData
# names(infect) <- c('from', 'to', 'time')
# infect$timebins <- as.numeric(cut(infect$time, breaks = 100))
# 
# # We want that nice fading effect so we need to add extra data for the trailing
# # infectAnim <- lapply(1:10, function(i) {infect$timebins  <- infect$timebins + i; infect$delay <- i; infect})
# # infect$delay <- 0
# # infectAnim <- rbind(infect, do.call(rbind, infectAnim))
# 
# # infectGraph <- graph_from_data_frame(infectAnim, directed = F)
# infectGraph <- graph_from_data_frame(infect, directed = T)
# 
# #Layout
# layout_orig=create_layout(infectGraph, layout = 'igraph', algorithm = 'kk')
# layout_merge=merge(layout_orig, meta, by="name", all.x=T)
# layout=layout_merge[,c("lat","lon","name","ggraph.orig_index","circular","ggraph.index")]
# colnames(layout)=c("x","y","name","ggraph.orig_index","circular","ggraph.index")
# layout=layout[order(layout$ggraph.orig_index),]
# layout_orig$x=layout$y
# layout_orig$y=layout$x
# 
# #Plot
# dev.copy(png,'Network CPE Linked Map.png', height=8, width=8, units="in", res=300)
# 
# p1=ggraph(layout_orig) + 
#   geom_node_point(size=6, colour = '#2b8b99') + 
#   geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
#                  end_cap = circle(2.5, 'mm')) + 
#   geom_edge_loop() +
#   # geom_edge_density() +
#   geom_node_text(aes(label = name), size=3) +
#   scale_edge_alpha(range = c(1, 0), guide = 'none') + 
#   scale_edge_width(range = c(0.5, 1.5), trans = 'exp', guide = 'none') + 
#   scale_size(guide = 'none') + 
#   ggtitle('Network-Supported Paths: CPE Episodes Linked Between 2012-2015') +
#   ggforce::theme_no_axes() + 
#   theme(plot.background = element_blank(), 
#         panel.background = element_blank(), 
#         panel.border = element_blank(), 
#         plot.title = element_text(color = '#cecece'))
# p1
# 
# 
# dev.off()
# 
# 
# ###################################################################################
# 
# FreqPI=as.data.frame(table((PairsDistanceTable$PotentialInfector)), stringsAsFactors = F)
# MaxEvent=FreqPI$Var1[which(FreqPI$Freq == max(FreqPI$Freq))]
# 
# MaxEvents=TemporalData[which(TemporalData$time == MaxEvent),]
# 
# # Network of All Spreading Events
# infect=MaxEvents
# names(infect) <- c('from', 'to', 'time')
# infect$timebins <- as.numeric(cut(infect$time, breaks = 9))
# 
# # We want that nice fading effect so we need to add extra data for the trailing
# # infectAnim <- lapply(1:10, function(i) {infect$timebins  <- infect$timebins + i; infect$delay <- i; infect})
# # infect$delay <- 0
# # infectAnim <- rbind(infect, do.call(rbind, infectAnim))
# 
# # infectGraph <- graph_from_data_frame(infectAnim, directed = F)
# infectGraph <- graph_from_data_frame(infect, directed = T)
# 
# layout_orig=create_layout(infectGraph, layout = 'igraph', algorithm = 'kk')
# layout_merge=merge(layout_orig, meta, by="name", all.x=T)
# layout=layout_merge[,c("lat","lon","name","ggraph.orig_index","circular","ggraph.index")]
# colnames(layout)=c("x","y","name","ggraph.orig_index","circular","ggraph.index")
# layout=layout[order(layout$ggraph.orig_index),]
# 
# layout_orig$x=layout$y
# layout_orig$y=layout$x
# 
# 
# p2=ggraph(layout_orig) + 
#   geom_node_point(size=6, colour = '#2b8b99') + 
#   geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
#                  end_cap = circle(2.5, 'mm')) + 
#   geom_edge_loop() +
#   # geom_edge_density() +
#   geom_node_text(aes(label = name), size=3) +
#   scale_edge_alpha(range = c(1, 0), guide = 'none') + 
#   scale_edge_width(range = c(0.5, 1.5), trans = 'exp', guide = 'none') + 
#   scale_size(guide = 'none') + 
#   ggtitle('Network-Supported Paths: CPE Episodes Linked Between 2012-2015') +
#   ggforce::theme_no_axes() + 
#   theme(plot.background = element_blank(), 
#         panel.background = element_blank(), 
#         panel.border = element_blank(), 
#         plot.title = element_text(color = '#cecece'))
# p2
# 
# # dev.copy(png,'Network CPE Linked Map.png', height=8, width=8, units="in", res=300)
# # p2
# # dev.off()
# 
# 
# ###################################################################################
# 
# #this version of the script has been tested on igraph 1.0.1
# 
# #load the edges with time stamp
# #there are three columns in edges: id1,id2,time
# edges <- TemporalData
# 
# #generate the full graph
# g <- graph.data.frame(edges,directed=F)
# 
# #generate a cool palette for the graph (darker colors = older nodes)
# YlOrBr.pal <- colorRampPalette(brewer.pal(8,"YlOrRd"))
# #colors for the nodes are chosen from the very beginning
# V(g)$color <- rev(YlOrBr.pal(vcount(g)))[as.numeric(V(g)$name)]
# 
# #time in the edges goes from 1 to 300. We kick off at time 3
# ti <- 3
# #remove edges which are not present
# gt <- delete_edges(g,which(E(g)$time > ti))
# #generate first layout using graphopt with normalized coordinates. This places the initially connected set of nodes in the middle. If you use fruchterman.reingold it will place that initial set in the outer ring.
# layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)
# 
# #total time of the dynamics
# total_time <- max(E(g)$time)
# #This is the time interval for the animation. In this case is taken to be 1/10
# #of the time (i.e. 10 snapshots) between adding two consecutive nodes
# dt <- 10
# #Output for each frame will be a png with HD size 1600x900 <img draggable="false" class="emoji" alt="🙂" src="https://s.w.org/images/core/emoji/2.4/svg/1f642.svg">
# png(file="animation/example%03d.png", width=1600,height=900)
# #Time loop starts
# for(time in seq(3,total_time,dt)){
#   #remove edges which are not present
#   gt <- delete_edges(g,which(E(g)$time > time))
#   #with the new graph, we update the layout a little bit
#   layout.new <- layout_with_fr(gt,coords=layout.old,niter=10,start.temp=0.05,grid="nogrid")
#   #plot the new graph
#   plot.igraph(gt,
#               layout=layout.new,
#               vertex.label="",
#               # vertex.size=1+2*log(degree(gt)),
#               # vertex.frame.color=V(g)$color,
#               edge.width=1.5,
#               asp=9/16,
#               margin=-0.15)
#   #use the new layout in the next round
#   layout.old <- layout.new
# }
# dev.off()
# 
# ################################################################################
# 
# # library(ggraph)
# # library(devtools)
# # library(RCurl)
# # library(httr)
# # set_config( config( ssl_verifypeer = 0L ) )
# # devtools::install_github("RcppCore/Rcpp")
# # devtools::install_github("dgrtwo/gganimate")
# # library(gganimate)
# 
# 
# # Data from http://konect.uni-koblenz.de/networks/sociopatterns-infectious
# # infect <- read.table('out.sociopatterns-infectious', skip = 2, sep = ' ', stringsAsFactors = FALSE)
# # infect <- read.table(file = "sg_infectious_contact_list.tgz", sep = '\t', header = F)
# # infect$V3 <- NULL
# infect=TemporalData[1:100,]
# names(infect) <- c('from', 'to', 'time')
# infect$timebins <- as.numeric(cut(infect$time, breaks = 100))
# 
# # We want that nice fading effect so we need to add extra data for the trailing
# infectAnim <- lapply(1:10, function(i) {infect$timebins  <- infect$timebins + i; infect$delay <- i; infect})
# infect$delay <- 0
# infectAnim <- rbind(infect, do.call(rbind, infectAnim))
# 
# infectGraph <- graph_from_data_frame(infectAnim, directed = F)
# 
# # We use only original data for the layout
# # subGr <- subgraph.edges(infectGraph, which(E(infectGraph)$delay == 0))
# # V(subGr)$degree <- degree(subGr)
# # lay <- create_layout(subGr, 'igraph', algorithm = 'lgl')
# # 
# # # Then we reassign the full graph with edge trails
# # attr(lay, 'graph') <- infectGraph
# # 
# # # Now we create the graph with timebins as frame
# # gr <- make_graph('bull')
# layout_orig=create_layout(infectGraph, layout = 'igraph', algorithm = 'kk')
# layout_merge=merge(layout_orig, meta, by="name", all.x=T)
# layout=layout_merge[,c("lat","lon","name","ggraph.orig_index","circular","ggraph.index")]
# colnames(layout)=c("x","y","name","ggraph.orig_index","circular","ggraph.index")
# layout=layout[order(layout$ggraph.orig_index),]
# 
# layout_orig$x=layout$y
# layout_orig$y=layout$x
# 
# p=ggraph(layout_orig) + 
#   geom_node_point(size=5, colour = '#8b4836') + 
#   geom_edge_link() +
#   geom_node_text(aes(label = name), size=2) +
#   geom_edge_link0(aes(frame = timebins, alpha = delay, width = delay), edge_colour = '#dccf9f') + 
#   scale_edge_alpha(range = c(1, 0), guide = 'none') + 
#   scale_edge_width(range = c(0.5, 1.5), trans = 'exp', guide = 'none') + 
#   scale_size(guide = 'none') + 
#   ggtitle('NSP CPE') +
#   ggforce::theme_no_axes() + 
#   theme(plot.background = element_rect(fill = '#1d243a'), 
#         panel.background = element_blank(), 
#         panel.border = element_blank(), 
#         plot.title = element_text(color = '#cecece'))
# 
# 
# # p <- ggraph(data = lay) + 
# #   geom_node_point(aes(size = degree), colour = '#8b4836') + 
# #   geom_edge_link0(aes(frame = timebins, alpha = delay, width = delay), edge_colour = '#dccf9f') + 
# #   scale_edge_alpha(range = c(1, 0), guide = 'none') + 
# #   scale_edge_width(range = c(0.5, 1.5), trans = 'exp', guide = 'none') + 
# #   scale_size(guide = 'none') + 
# #   ggtitle('Human Interactions') +
# #   ggforce::theme_no_axes() + 
# #   theme(plot.background = element_rect(fill = '#1d243a'), 
# #         panel.background = element_blank(), 
# #         panel.border = element_blank(), 
# #         plot.title = element_text(color = '#cecece'))
# 
# # And then we animate
# # animation::ani.options(interval=0.1)
# # install.packages("magick")
# # library("magick")
# # magickPath <- shortPathName("C:\\Program Files\\ImageMagick-7.0.3-Q16\\magick.exe")
# # Sys.setenv(PATH = paste("C:/Program Files/ImageMagick/bin",
# #                          Sys.getenv("PATH"), sep = ";"))
# # ani.options(convert=magickPath)
# # gganimate(p, 'animation.mp4', title_frame = FALSE)
