###Degrees of Dept Network###

#Get Hopital Dept
hospital_depts=substr(rownames(pFlow_ALL), 1, 2)
pFlow_ALL_Dept=pFlow_ALL
dimnames(pFlow_ALL_Dept) = list(hospital_depts, hospital_depts)
#Compress Matrix
pFlow_ALL_Dept = pFlow_ALL_Dept %*% sapply(unique(hospital_depts),"==", hospital_depts)
pFlow_ALL_Dept = t(pFlow_ALL_Dept) %*% sapply(unique(hospital_depts),"==", hospital_depts)
diag(pFlow_ALL_Dept) = 0
str(pFlow_ALL_Dept)
pFlow_ALL_Dept=as.matrix(pFlow_ALL_Dept)

#Get directed graph
directed.graph_Dept = graph.adjacency(pFlow_ALL_Dept, mode="directed", diag=FALSE, weighted=TRUE)
directed.graph_Dept_diag = graph.adjacency(pFlow_ALL_Dept, mode="directed", diag=T, weighted=TRUE)
directed.graph_Dept_unweighted = graph.adjacency(pFlow_ALL_Dept, mode="directed", diag=FALSE, weighted=NULL)


###From edgelist
directed.graph = graph.adjacency(pFlow_ALL, mode="directed", diag=FALSE, weighted=TRUE)
mean(degree(directed.graph))

edgelist=get.edgelist(directed.graph)

edgelist[,1]=substr(edgelist[,1], 1, 2)
edgelist[,2]=substr(edgelist[,2], 1, 2)

edgelist=edgelist[!duplicated(edgelist),]

graph=graph_from_edgelist(edgelist, directed=T)
vcount(graph)
ecount(graph)
degree(graph)

degrees=cbind(V(graph)$name, degree(graph))
indegrees=cbind(V(graph)$name, degree(graph, mode="in"))
outdegrees=cbind(V(graph)$name, degree(graph, mode="out"))

