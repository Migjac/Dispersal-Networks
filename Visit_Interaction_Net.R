####Author: Miguel Jácome-Flores

  ##*Frugivores visits*

####**Step 1. **Open Martinazo and Matasgordas matrix

#Martinazo
mart<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_visit.csv",header=TRUE,check.names=FALSE,row.names=1)
row.names(mart)
#Matasgordas
matas<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/matas_visit.csv",header=TRUE,check.names=FALSE,row.names=1)
row.names(matas)
####**Step 2.** Using "bipartite" package

#####*Step 2.1.* Ploting the web.

par(mfrow = c(2,1))
plotweb(mart, col.low=c(gray(seq(0.1,1,length=24))),col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8) 
plotweb(matas, col.low=c(gray(seq(0.1,1,length=42))), col.high=c("white","white","gray30","gray30","black","black","darkgray"), method="normal", text.rot=90,low.lablength=10, high.lablength=8)

####**Step 3.** Calculate indices describing network topography. 

#Centrality measures

#Degree
specieslevel(mart,index="normalised degree")
specieslevel(matas,index="normalised degree")

#Closeness
specieslevel(mart,index="closeness")
specieslevel(matas,index="closeness")

#Betweeness
specieslevel(mart,index="betweenness")
specieslevel(matas,index="betweenness")


##Networks analysis. Network topology indexes

networklevel(mart)
networklevel(matas)

#Modularity
mod_mart<-computeModules(mart)
mod_matas<-computeModules(matas)

#Dependence
linklevel(mart)
linklevel(matas)


#Coping the results to my folder in local repository**

write.table(net_lev_matas, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/netlevmat.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")


