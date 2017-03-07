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

clos_mart<-closeness_w(mart, gconly=TRUE, precomp.dist=NULL, alpha=1)
bet_mart<-betweenness_w(mart)
mod_mart<-computeModules(mart)
spp_lev_mart<-specieslevel(mart,PDI.normalise=FALSE)
link_lev_mart<-linklevel(mart)
net_lev_mart<-networklevel(mart)

net_lev_mart
clos_mart
bet_mart
mod_mart
spp_lev_mart
link_lev_mart
net_lev_mart

computeModules(mart)

clos_matas<-closeness_w(matas, gconly=TRUE, precomp.dist=NULL, alpha=1)
bet_matas<-betweenness_w(matas)
mod_matas<-computeModules(matas)
spp_lev_matas<-specieslevel(matas, PDI.normalise=FALSE)
link_lev_matas<-linklevel(matas)
net_lev_matas<-networklevel(matas)
strength(mart, type="Barrat")
strength(matas, type = "Barrat")

clos_matas
bet_matas
mod_matas
spp_lev_matas
link_lev_matas
net_lev_matas
computeModules(matas)


#Coping the results to my folder in local repository**

write.table(net_lev_matas, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/netlevmat.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")
write.table(clos_mart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/clos_mart.txt", sep="\t")


