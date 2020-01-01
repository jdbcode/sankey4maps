#install.packages(c('raster', 'networkD3', 'dplyr'))
library(tidyverse)
library(raster)
library(networkD3)
library(dplyr)
library(rgdal)


########################################################################################
########################################################################################
########################################################################################

# define path to period info file
periodInfoFile = "D:/work/temp/reilly_mc2_sankey/periodInfo.csv"

# define path to class info file
classInfoFile = "D:/work/temp/reilly_mc2_sankey/classInfo.csv"

# define minimum area of a class to include in figure
minAreaFrac = 0.01   # this means that classes per period making up less than 5% of total study area will be excluded from the plot

# define plot features
fontSize = 12
fontFamily = "sans-serif"
nodeWidth = 30

########################################################################################
########################################################################################
########################################################################################

# read in the info files
classInfo = read_csv(classInfoFile, col_names = c('class', 'value', 'color'))
periodInfo = read_csv(periodInfoFile, col_names = c('rasterFile', 'rasterBand', 'nodeCol'))

# create the nodeInfo table
nodeInfo = data.frame(nodeName=NA, nodeID=NA, mapClass=NA, nodeCol=NA, nodeGroup=NA, color=NA)
id = -1
for(p in 1:nrow(periodInfo)){
  for(c in 1:nrow(classInfo)){
    id = id+1
    nodeInfo = nodeInfo %>%
      rbind(data.frame(nodeName=classInfo$class[c], 
                       nodeID=id,  
                       mapClass=classInfo$value[c], 
                       nodeCol=p, 
                       nodeGroup=str_pad(classInfo$value[c], 10, pad = "0"),
                       color=classInfo$color[c]))
  }
}
nodeInfo = nodeInfo[2:nrow(nodeInfo),]

# add a default area fraction for a cover class per preiod - gets filled in later
nodeInfo$areaFrac = 0

# join fileInfo to nodeInfo
nodeInfo <- dplyr::left_join(nodeInfo, periodInfo, by='nodeCol')

# convert factors to characters
nodeInfo$nodeName <- as.character(nodeInfo$nodeName)
nodeInfo$rasterFile <- as.character(nodeInfo$rasterFile)
nodeInfo$nodeGroup <- as.factor(nodeInfo$nodeGroup)

# figure out the area of each class per period
for(i in 1:nrow(nodeInfo)){
  vals = values(raster(nodeInfo$rasterFile[i], nodeInfo$rasterBand[i]))
  totArea = length(which(is.na(vals) == T))
  area = length(which(vals == nodeInfo$mapClass[i]))
  nodeInfo$areaFrac[i] = area/totArea
}

# filter out classes per period that have less than 5% area of the total study area
nodeInfo = nodeInfo %>%
  filter(areaFrac >= minAreaFrac)

# re-assign nodeIDs 
nodeInfo$nodeID = seq(0, nrow(nodeInfo)-1)

# define the the links
NodeCols <- sort(unique(nodeInfo$nodeCol))
linkInfo <- data.frame()
for(i in 1:(length(NodeCols)-1)){
  fromCol <- dplyr::filter(nodeInfo, nodeCol==NodeCols[i])
  toCol <- dplyr::filter(nodeInfo, nodeCol==NodeCols[i+1])
  fromR <- values(raster(fromCol$rasterFile[1], fromCol$rasterBand[1]))
  toR <- values(raster(toCol$rasterFile[1], toCol$rasterBand[1]))
  for(f in 1:nrow(fromCol)){
    for(t in 1:nrow(toCol)){
      nFromTo <- length(which(fromR == fromCol$mapClass[f] & toR == toCol$mapClass[t]))
      linkInfo <- rbind(linkInfo, data.frame(source=fromCol$nodeID[f], target=toCol$nodeID[t], value=nFromTo))
    }
  }
}

# re-factor the groups 
groups = sort(unique(nodeInfo$nodeGroup))
nodeInfo$nodeGroup = factor(nodeInfo$nodeGroup, levels = groups)

# make color list
levList = levels(nodeInfo$nodeGroup)
colors = c()
for(i in 1:length(levList)){
  colors[i] = nodeInfo$color[which(nodeInfo$nodeGroup == levList[i])[1]]
}

# make the sankey plot
sankeyNetwork(Links = linkInfo, Nodes = nodeInfo,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "nodeName",
              NodeGroup = "nodeGroup",
              fontSize = fontSize,
              fontFamily = fontFamily,
              nodeWidth = nodeWidth,
              colourScale = paste0('d3.scaleOrdinal().range(["',paste0(colors,collapse = '","'),'"])'))




