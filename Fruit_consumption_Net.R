##*Fruit remotion rates*
#**Step 1. **Open Martinazo and Matasgordas matrix

#Opening a matrix file with weighted links with frugivores fruit remotion in individual plants. Other specifications of the matrix are described in the previous section.

#Martinazo
mart_f<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_fruits.csv",header=TRUE,check.names=FALSE,row.names=1)

#Matasgordas
matas_f<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/matas_fruits.csv",header=TRUE,check.names=FALSE,row.names=1)

##**Step 2.** Using "bipartite" package 

library(biparite)
##*Step 2.1.* Ploting the web.
#Creates a bipartite network were the high level will be represented by the frugivores and low level to each individual plant. *Links are weigthed with the proportion fruit remotion proportion/animal/individual plant*.[^2] 

#Martinazo Bipartite network
par(mfrow = c(2,1))
plotweb(mart_f, col.low=c(gray(seq(0.1,1,length=24))), col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8)

#Matasgordas Bipartite Network
plotweb(matas_f, col.low=c(gray(seq(0.1,1,length=39))), col.high=c("white","white","gray30","gray30","black","black","darkgray"), method="normal", text.rot=90,low.lablength=10, high.lablength=8)

##**Step 3.** Calculate different indexes of a network.[^2]

#Centrality measures

#Closeness
specieslevel(mart_f,index="closeness")
specieslevel(matas_f,index="closeness")

#Betweeness
specieslevel(mart_f,index="betweenness")
specieslevel(matas_f,index="betweenness")

#Network level
networklevel(mart_f)
mod_mart_f<-computeModules(mart_f)
linklevel(mart_f)

networklevel(matas_f)
specieslevel(matas_f)
mod_matas_f<-computeModules(matas_f)
link_lev_matas_f<-linklevel(matas_f)


#####**Step 3.1.** Ploting modules in the networks

par(mfrow=c(1,2))
plotModuleWeb(mod_mart_f)
plotModuleWeb(mod_matas_f)

##**Step 4.** Gathering Martinazo and Matasgordas' matrixs as one; plants are ordered from more to less aggregated


par(mfrow=c(1,1))
mart_matas_fruits<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/Mart_matas_fruits.csv",header=TRUE,check.names=FALSE,row.names=1)
plotweb(mart_matas_fruits, col.low=c(gray(seq(0.1,1,length=56))), col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8)

##**Step 5.** Changing the matrix by eliminating unknown visitors


#Martinazo
mart_fnun<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_fruits_no_unk.csv",header=TRUE,check.names=FALSE,row.names=1)

#Matasgordas
matas_fnun<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/matas_fruits_no_unk.csv",header=TRUE,check.names=FALSE,row.names=1)

##Bipartite plot

#Martinazo Bipartite Network
par(mfrow=c(2,1))
plotweb(mart_fnun, col.low=c(gray(seq(0.1,1,length=22))), col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8)

#Matasgordas Bipartite Network
plotweb(matas_fnun, col.low=c(gray(seq(0.1,1,length=30))), col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8)
```