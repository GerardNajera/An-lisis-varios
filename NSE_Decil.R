# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

setwd("C:/Users/Gerardo Nájera/OneDrive - Planning Quant, S.A de C.V/Códigos R/Análisis varios")

Data<- read_csv("NSE_Decil.csv")

Data<- as.data.frame(Data)
row.names(Data)<- Data$NSE



library(networkD3)

# I need a long format
data_long <- Data  %>%
  pivot_longer(names_to = 'key', values_to ='value', -NSE) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

data_long <- as.data.frame(data_long)


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network

network <-sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

saveNetwork(network, 'NSE.html', selfcontained = TRUE)


############################################################################################################3


library(ggplot2)
library(ggpubr)
library(viridis)
theme_set(theme_pubr())

ggballoonplot(Data[,2:11], fill = "value")+
  scale_fill_viridis_c(option = "C")

ggballoonplot(Data[,2:11], size = "value",fill = "value")+
  scale_fill_viridis_c(option = "D")