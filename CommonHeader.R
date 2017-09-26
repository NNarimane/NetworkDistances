### Common header

#################################
#### INSTALL & LOAD PACKAGES ####

cat("Load Packages\n")
library("outbreaker2")
library("data.table")
library("igraph")
library("stringr")
library("ape")
library("adegenet")
library("R0")
library("visNetwork")
library("plyr")
library("combinat")
library("doSNOW")
library("snow")
library("foreach")
library("splitstackshape")

#########################
#### SET ENVIRONMENT ####

cat("Set Working Environment\n")

envNNwindows=T

if(Sys.getenv("LOGNAME")=="pascalcrepey"){
  currentwd=setwd("~/Dropbox/Network Distances and CPE Episodes/")
  writingDir="~/Google Drive/1-EPC/NetworkDistances/Results/"
}else{
  if(envNNwindows){
    currentwd=setwd("C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/")
    writingDir="C:/Users/Narimane/Dropbox/Network Distances and CPE Episodes/Results/"
  }else{
    currentwd=setwd("/Users/narimanenekkab/Dropbox/Network Distances and CPE Episodes/")
    writingDir="/Users/narimanenekkab/Dropbox/Network Distances and CPE Episodes/Results/"
  }
}


