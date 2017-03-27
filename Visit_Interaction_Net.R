####Author: Miguel Jácome-Flores

  ##*Frugivores visits*


####Calculate aggregation degree of given plants via neares neighbour function in "spatstat package"

attach(martcoord)
nnmart<-nndist(martcoord$x, martcoord$y)
write.table(nnmart, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal-Networks/nnmart.txt", sep="\t")
detach(martcoord)

attach(matascoord)
nnmatas<-nndist(matascoord$x_proj, matascoord$y_proj)
write.table(nnmatas, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal-Networks/nnmatas.txt", sep="\t")
detach(martcoord)

####**Step 1. **Open Martinazo and Matasgordas matrix

#Martinazo
mart<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_visit.csv",header=TRUE,check.names=FALSE,row.names=1)
row.names(mart)
#Matasgordas
matas<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/matas_visit.csv",header=TRUE,check.names=FALSE,row.names=1)
row.names(matas)
####**Step 2.** Using "bipartite" package

library(biparite)

#####*Step 2.1.* Ploting the web.

par(mfrow = c(2,1))
plotweb(mart, col.low=c(gray(seq(0.1,1,length=24))),col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8) 
plotweb(matas, col.low=c(gray(seq(0.1,1,length=42))), col.high=c("white","white","gray30","gray30","black","black","darkgray"), method="normal", text.rot=90,low.lablength=10, high.lablength=8)

par(mfrow = c(1,2))
gplot(mart, "graph",vertex.col="gray", edge.col="grey75", mode="circle",vertex.cex=log(deg), main="Martinazo")
deg<-degree(mart,gmode="graph")
get.vertex.attribute(mart,"vertex.names")
str(mart)

gplot(matas, main="Matasgordas")

####**Step 3.** Calculate indices describing network topography. 

##**Step 3.** Calculate different indexes of a network.

#Centrality measures

#Degree, Betweeness and Closeness

HLspplvlmart<-specieslevel(mart)
HLspplvlmart$`higher level`[,c(2,3,7,10,11,12,13,14,17)]
LLspplvlmart<-specieslevel(mart)
LLspplvlmart$`lower level`[,c(2,3,7,10,11,12,13,14,17)]

LLspplvlmart

spplvlmatas<-specieslevel(matas)
spplvlmatas$`higher level`[,c(2,3,7,10,11,12,13,14,17)]
LLspplvlmatas<-specieslevel(matas)
LLspplvlmatas$`lower level`[,c(2,3,7,10,11,12,13,14,17)]



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

#Full data of visits

par(mfrow=c(1,1))
mart_matas_visits<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_matas_visits.csv",header=TRUE,check.names=FALSE,row.names=1)
plotweb(mart_matas_visits, col.low=c(gray(seq(0.1,1,length=63))),col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8)

#Degree, Betweeness and Closeness

HLspplvlmartmatasvisits<-specieslevel(mart_matas_visits)
martmatasHL<-HLspplvlmartmatasvisits$`higher level`[,c(2,3,7,10,11,12,13,14,17)]
LLspplvlmartmatasvisits<-specieslevel(mart_matas_visits)
martmatasLL<-LLspplvlmartmatasvisits$`lower level`[,c(1,2,3,7,10,11,12,13,14,17)]
martmatasLL
write.csv(martmatasHL, file = "martmatasHL.csv")
write.csv(martmatasLL, file = "martmatasLL.csv")

#Centrality related to aggregation or nearest neighbour analysis


library(ISLR)
#Martinazo

par(mfrow=c(1,1))
mart_wv<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_wv.csv",header=TRUE,check.names=FALSE,row.names=1)
mart_wv


mart_corr <- mart_wv[c(1:10)]
mart_corr
m<-cor(mart_corr, method="kendall")
corrplot(m, method = "number", type = "lower")
m



mart_v<-glm(nd~nn+ dens + del.area, data = mart_wv)
par(mfrow=c(3,3))
plot(mart_v)
summary(mart_v)
anova(mart_v)


#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.347703   0.083478   4.165 0.000563 ***
 # dens        0.001372   0.030349   0.045 0.964427    
#  edf Ref.df     F p-value
#s(nn)       1.000  1.000 0.585   0.454
#s(del.area) 2.688  3.311 1.061   0.373
#R-sq.(adj) =  0.0148   Deviance explained = 21.6%
mart_vnd<-gam(nd~s(nn)+s(del.area)+dens,data = mart_wv)
plot(mart_vnd,residuals=TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Normalised degree")
summary(mart_vnd)

#Estimate Std. Error t value Pr(>|t|)
#(Intercept)  0.07088    0.05740   1.235    0.232
#dens        -0.01150    0.02059  -0.558    0.583
# edf Ref.df     F p-value
#s(nn)       1.614  1.965 0.929   0.364
#s(del.area) 1.000  1.000 0.185   0.672
#R-sq.(adj) =  -0.0313   Deviance explained = 13.1%
mart_vwb<-gam(wbtw~s(nn)+s(del.area)+dens,data = mart_wv)
plot(mart_vwb,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(mart_vwb)

#Estimate Std. Error t value Pr(>|t|)
#(Intercept)  0.07088    0.05740   1.235    0.232
#dens        -0.01150    0.02059  -0.558    0.583
# edf Ref.df     F p-value
#s(nn)       1.614  1.965 0.929   0.364
#s(del.area) 1.000  1.000 0.185   0.672
#R-sq.(adj) =  -0.0313   Deviance explained = 13.1%
mart_vwc<-gam(wcls~s(nn)+s(del.area)+dens,data = mart_wv)
plot(mart_vwc,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Closeness")
summary(mart_vwc)

#Matasgordas
matas_wv<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/matas_wv.csv",header=TRUE)
matas_wv

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.358628   0.043767   8.194  7.5e-10 ***
 # dens        -0.006808   0.007391  -0.921    0.363    
#  edf Ref.df     F p-value  
#s(nn)       1.157  1.298 0.057  0.8089  
#s(del.area) 1.674  2.054 3.161  0.0514 .
#R-sq.(adj) =  0.177   Deviance explained = 25.4%
matas_vnd<-gam(nd~s(nn)+s(del.area)+dens,data = matas_wv)
plot(matas_vnd,residuals=TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Normalised Degree")
summary(matas_vnd)

#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.036681   0.020425   1.796   0.0817 .
#dens        -0.002469   0.003560  -0.693   0.4929  
#  edf Ref.df     F p-value   
#s(nn)       1.441  1.748 4.391 0.06918 . 
#s(del.area) 5.804  6.520 4.601 0.00147 **
#R-sq.(adj) =   0.42   Deviance explained = 53.7%
matas_vwb<-gam(wbtw~s(nn)+s(del.area)+dens,data = matas_wv)
plot(matas_vwb,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(matas_vwb)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.0267349  0.0032962   8.111 8.16e-10 ***
 # dens        -0.0008312  0.0005509  -1.509     0.14    
#  edf Ref.df     F p-value
#s(nn)         1      1 0.013   0.910
#s(del.area)   1      1 0.014   0.906
#R-sq.(adj) =  0.0151   Deviance explained = 8.72%
matas_vwc<-gam(wcls~s(nn)+s(del.area)+dens,data = matas_wv)
plot(matas_vwc,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Closeness")
summary(matas_vwc)


#Fulldata with values of centrality pasting the matrix analyzed separetly

mart_matas_wv<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_matas_wv.csv",header=TRUE)
mart_matas_wv

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.362154   0.036258   9.988 2.79e-14 ***
 # dens        -0.006793   0.007441  -0.913    0.365    
#  edf Ref.df     F p-value
#s(nn)       1.474  1.800 0.287   0.663
#s(del.area) 3.726  4.474 0.784   0.464
#R-sq.(adj) =  0.0798   Deviance explained = 16.8%
mart_matas_vnd<-gam(nd~s(nn)+s(del.area)+dens,data = mart_matas_wv)
plot(mart_matas_vnd,residuals=TRUE, se=TRUE, shade=TRUE, all.terms = TRUE, main = "Normalised Degree")
summary(mart_matas_vnd)
plot(mart_matas_wv$nd,mart_matas_wv$nn)

#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.056167   0.020745   2.708  0.00882 **
 # dens        -0.006097   0.004233  -1.440  0.15502   
#  edf Ref.df     F p-value
#s(nn)       1.749  2.176 2.233   0.109
#s(del.area) 2.265  2.831 2.066   0.129
#R-sq.(adj) =  0.107   Deviance explained = 17.6%
mart_matas_vwb<-gam(wbtw~s(nn)+s(del.area)+dens,data = mart_matas_wv)
plot(mart_matas_vwb,residuals= TRUE, se=TRUE, shade=TRUE, all.terms = TRUE, main = "Weighted Betweeness")
summary(mart_matas_vwb)
plot(mart_matas_wv$wbtw,mart_matas_wv$nn)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.0366969  0.0043246   8.486 6.28e-12 ***
 # dens        -0.0017501  0.0008746  -2.001   0.0498 *  
#  edf Ref.df     F p-value
#s(nn)       1.784   2.23 1.706   0.222
#s(del.area) 1.000   1.00 0.358   0.552
#R-sq.(adj) =  0.125   Deviance explained = 17.6%
mart_matas_vwc<-gam(wcls~s(nn)+s(del.area)+dens,data = mart_matas_wv)
plot(mart_matas_vwc,residuals= TRUE, se=TRUE, shade=TRUE, all.terms=TRUE, main = "Weighted Closeness")
summary(mart_matas_vwc)
plot(mart_matas_wv$wcls,mart_matas_wv$nn)


#Fulldata with values of centrality using the matrix analyzed together

mart_matas_wv1<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal-Networks/martmatasLL.csv",header=TRUE)
mart_matas_wv1

#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.365951   0.035586  10.283 1.21e-14 ***
#  dens        -0.010590   0.007736  -1.369    0.176    
#  edf Ref.df     F p-value
#s(nn)       2.505  3.116 0.476   0.671
#s(del.area) 1.000  1.000 0.454   0.503
#R-sq.(adj) =  0.0516   Deviance explained =   12%
mart_matas_vnd1<-gam(nd~s(nn)+s(del.area)+dens,data = mart_matas_wv1)
plot(mart_matas_vnd1,residuals=TRUE, se=TRUE, shade=TRUE, all.terms = TRUE, main = "Normalised Degree")
summary(mart_matas_vnd1)
plot(mart_matas_wv1$nd,mart_matas_wv1$nn)


#            Estimate Std. Error t value Pr(>|t|)
#(Intercept)  0.035548   0.022063   1.611    0.113
#dens        -0.004998   0.004709  -1.061    0.293
# edf Ref.df     F p-value
#s(nn)       1.463  1.789 1.227   0.243
#s(del.area) 1.441  1.767 0.171   0.775
#R-sq.(adj) =  -0.00119   Deviance explained = 6.18%
#GCV = 0.0097787  Scale est. = 0.0090175  n = 63
mart_matas_vwb1<-gam(wbtw~s(nn)+s(del.area)+dens,data = mart_matas_wv1)
plot(mart_matas_vwb1,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(mart_matas_vwb1)
plot(mart_matas_wv1$wbtw,mart_matas_wv1$nn)


#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.0186029  0.0022797    8.16  3.1e-11 ***
 # dens        -0.0008436  0.0004847   -1.74   0.0871 .  
#  edf Ref.df     F p-value
#s(nn)       1.482   1.82 1.428   0.342
#s(del.area) 1.000   1.00 0.193   0.662
#R-sq.(adj) =  0.118   Deviance explained = 16.8%
#GCV = 0.00010552  Scale est. = 9.8013e-05  n = 63
mart_matas_vwc1<-gam(wcls~s(nn)+s(del.area)+dens,data = mart_matas_wv1)
plot(mart_matas_vwc1,residuals= TRUE, se=TRUE, shade=TRUE, all.terms=TRUE, main = "Weighted Closeness")
summary(mart_matas_vwc1)
plot(mart_matas_wv1$wcls,mart_matas_wv1$nn)


#Interaction motifs

#Obtaining Median of non-zero interactions. 

##Creating a matrix with 0=NA
martmat_vNA<-mart_matas_visits
martmat_vNA[martmat_vNA==0]<-NA
martmat_vNA

##Median excluding NAs(e.g. 0). "The median for proportional visits is (0.1667)". All datas above this value will mean strong interaction and all below weak interaction
median(as.matrix(martmat_vNA),na.rm=TRUE)





