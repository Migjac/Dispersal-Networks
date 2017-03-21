##*Fruit removal rates*
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

#Degree, Betweeness and Closeness

HLspplvlmartf<-specieslevel(mart_f)
HLspplvlmartf$`higher level`[,c(2,11,12,13,14)]
LLspplvlmartf<-specieslevel(mart_f)
LLspplvlmartf$`lower level`[,c(2,11,12,13,14)]

spplvlmatasf<-specieslevel(matas_f)
spplvlmatasf$`higher level`[,c(2,11,12,13,14)]
LLspplvlmatasf<-specieslevel(matas_f)
LLspplvlmatasf$`lower level`[,c(2,11,12,13,14)]




##Networks analysis. Network topology indexes

networklevel(mart_f)
networklevel(matas_f)

#Modularity
mod_mart_f<-computeModules(mart_f)
mod_matas_f<-computeModules(matas_f)

#Dependence
linklevel(mart_f)
linklevel(matas_f)


#####**Step 3.1.** Ploting modules in the networks

par(mfrow=c(1,2))
plotModuleWeb(mod_mart_f)
plotModuleWeb(mod_matas_f)

##**Step 4.** Gathering Martinazo and Matasgordas' matrixs as one; plants are ordered from more to less aggregated


par(mfrow=c(1,1))
mart_matas_fruits<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/Mart_matas_fruits.csv",header=TRUE,check.names=FALSE,row.names=1)
plotweb(mart_matas_fruits, col.low=c(gray(seq(0.1,1,length=56))), col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", text.rot=90,low.lablength=10, high.lablength=8)

#Centrality measures

#Degree, Betweeness and Closeness

HLspplvlmartf<-specieslevel(mart_matas_fruits)
HLspplvlmartf$`higher level`[,c(2,11,12,13,14)]
LLspplvlmartf<-specieslevel(mart_matas_fruits)
LLspplvlmartf$`lower level`[,c(2,11,12,13,14)]


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


#Do the centrality is related to the spatial location of individual in a population?
#Centrality related to aggregation and density 

##GAM
par(mfrow=c(3,3))
library(ISLR)
#Martinazo
mart_wf

#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.30200    0.08626   3.501  0.00257 **
#  dens         0.00321    0.02947   0.109  0.91448   
#  edf Ref.df     F p-value
#s(nn)       1.000  1.000 0.217   0.647
#s(del.area) 1.115  1.219 1.254   0.338
#R-sq.(adj) =  -0.0692   Deviance explained = 8.94%
mart_fnd<-gam(nd~s(nn)+s(del.area)+dens,data = mart_wf)
plot(mart_fnd,residuals=TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Normalised degree")
summary(mart_fnd)

#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.060454   0.019164   3.155   0.0168 *
# dens        -0.005593   0.006813  -0.821   0.4397  
#  edf Ref.df    F  p-value    
#s(nn)       8.900  8.988 37.3 1.18e-10 ***
# s(del.area) 4.342  5.076 47.8 3.87e-09 ***
#R-sq.(adj) =  0.959   Deviance explained = 98.7%
#GCV = 0.0023964  Scale est. = 0.00073607  n = 22
mart_fwb<-gam(wbtw~s(nn)+s(del.area)+dens,data = mart_wf)
plot(mart_fwb,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(mart_fwb)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.029254   0.006872   4.257 0.000474 ***
#  dens        0.002356   0.002345   1.005 0.328289    
#  edf Ref.df     F p-value
#s(nn)         1      1 0.678   0.421
#s(del.area)   1      1 0.090   0.767
#R-sq.(adj) =  -0.0966   Deviance explained = 6.01%
#GCV = 0.00020691  Scale est. = 0.00016929  n = 22
mart_fwc<-gam(wcls~s(nn)+s(del.area)+dens,data = mart_wf)
plot(mart_fwc,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Closeness")
summary(mart_fwc)

#Matasgordas
matas_wf<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/matas_wf.csv",header=TRUE)
matas_wf

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.332488   0.050840   6.540 3.12e-07 ***
#  dens        -0.006715   0.008677  -0.774    0.445    
#  edf Ref.df     F p-value
#s(nn)         1      1 0.008   0.928
#s(del.area)   1      1 0.000   0.998
#R-sq.(adj) =  -0.067   Deviance explained =    3%
#GCV = 0.02448  Scale est. = 0.0216    n = 34
matas_fnd<-gam(nd~s(nn)+s(del.area)+dens,data = matas_wf)
plot(matas_fnd,residuals=TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Normalised Degree")
summary(matas_fnd)

#Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.0278111  0.0195036   1.426    0.164
#dens        0.0003146  0.0033288   0.095    0.925
# edf Ref.df     F p-value
#s(nn)         1      1 0.343   0.562
#s(del.area)   1      1 0.231   0.634
#R-sq.(adj) =  -0.0611   Deviance explained = 3.53%
#GCV = 0.0036027  Scale est. = 0.0031789  n = 34
matas_fwb<-gam(wbtw~s(nn)+s(del.area)+dens,data = matas_wf)
plot(matas_fwb,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(matas_fwb)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.0225772  0.0028818   7.834 9.64e-09 ***
#  dens        0.0005464  0.0004919   1.111    0.275    
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  edf Ref.df     F p-value
#s(nn)         1      1 0.168   0.684
#s(del.area)   1      1 0.620   0.437
#R-sq.(adj) =  -0.046   Deviance explained = 4.91%
#GCV = 7.8658e-05  Scale est. = 6.9404e-05  n = 34
matas_fwc<-gam(wcls~s(nn)+s(del.area)+dens,data = matas_wf)
plot(matas_fwc,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Closeness")
summary(matas_fwc)


#Fulldata using the data of the matrixs analysed separetly
mart_matas_wf<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_matas_wf.csv",header=TRUE)
mart_matas_wf

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.329839   0.036682   8.992 3.57e-12 ***
#  dens        -0.006443   0.007426  -0.868     0.39    
#  edf Ref.df    F p-value
#s(nn)         1      1 0.01   0.922
#s(del.area)   1      1 0.40   0.530
#R-sq.(adj) =  -0.0363   Deviance explained = 2.03%
#GCV = 0.024075  Scale est. = 0.022355  n = 56
mart_matas_fnd<-gam(nd~s(nn)+s(del.area)+dens,data = mart_matas_wf)
plot(mart_matas_fnd,residuals=TRUE, se=TRUE, shade=TRUE, all.terms = TRUE, main = "Normalised Degree")
summary(mart_matas_fnd)

#Estimate Std. Error t value Pr(>|t|)
#(Intercept)  0.0378415  0.0237189   1.595    0.117
#dens        -0.0005135  0.0050278  -0.102    0.919
# edf Ref.df     F p-value  
#s(nn)       5.638  6.791 2.078  0.0647 .
#s(del.area) 1.000  1.000 2.280  0.1376  
#R-sq.(adj) =   0.17   Deviance explained = 28.5%
#GCV = 0.0085227  Scale est. = 0.007208  n = 56
mart_matas_fwb<-gam(wbtw~s(nn)+s(del.area)+dens,data = mart_matas_wf)
plot(mart_matas_fwb,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(mart_matas_fwb)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.0282913  0.0027622  10.242 4.52e-14 ***
#  dens        0.0002605  0.0005592   0.466    0.643    
#  edf Ref.df     F p-value
#s(nn)         1      1 1.614   0.210
#s(del.area)   1      1 0.034   0.855
#R-sq.(adj) =  -0.0214   Deviance explained = 3.43%
#GCV = 0.00013651  Scale est. = 0.00012676  n = 56
mart_matas_fwc<-gam(wcls~s(nn)+s(del.area)+dens,data = mart_matas_wf)
plot(mart_matas_fwc,residuals= TRUE, se=TRUE, shade=TRUE, all.terms=TRUE, main = "Weighted Closeness")
summary(mart_matas_fwc)

#With a subset excluding all palms far away from 20 m. A reduce in the significance an the explained variance
prb<-mart_matas_wf[1:48,]

#Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.0392092  0.0262428   1.494    0.144
#dens        0.0001987  0.0053611   0.037    0.971
#  edf Ref.df     F p-value   
#s(nn)       8.427  8.901 3.714 0.00144 **
#  s(del.area) 1.000  1.000 0.257 0.61522   
#R-sq.(adj) =   0.38   Deviance explained = 51.8%
#GCV = 0.0080653  Scale est. = 0.0061452  n = 48
mart_matas_fwb1<-gam(wbtw~s(nn)+s(del.area)+dens,data = prb)
plot(mart_matas_fwb1,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(mart_matas_fwb1)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.0265263  0.0029441   9.010  1.5e-11 ***
#  dens        0.0004750  0.0005661   0.839    0.406    
#  edf Ref.df     F p-value
#s(nn)         1      1 2.266   0.139
#s(del.area)   1      1 0.194   0.662
#R-sq.(adj) =  -0.0127   Deviance explained = 5.19%
#GCV = 0.00012651  Scale est. = 0.00011596  n = 48
mart_matas_fwc1<-gam(wcls~s(nn)+s(del.area)+dens,data = prb)
plot(mart_matas_fwc1,residuals= TRUE, se=TRUE, shade=TRUE, all.terms=TRUE, main = "Weighted Closeness")
summary(mart_matas_fwc1)



#Using the data of the matrix analysed together
mart_matas_wf2<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal-Networks/MartmatasLL_wf.csv",header=TRUE)
mart_matas_wf2

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.305141   0.034871   8.751 8.46e-12 ***
#  dens        -0.004689   0.007059  -0.664    0.509    
#  edf Ref.df     F p-value
#s(nn)         1      1 0.089   0.767
#s(del.area)   1      1 0.191   0.664
#R-sq.(adj) =  -0.0471   Deviance explained = 1.01%
#GCV = 0.021757  Scale est. = 0.020203  n = 56
mart_matas_fnd2<-gam(nd~s(nn)+s(del.area)+dens,data = mart_matas_wf2)
plot(mart_matas_fnd2,residuals=TRUE, se=TRUE, shade=TRUE, all.terms = TRUE, main = "Normalised Degree")
summary(mart_matas_fnd2)

#Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.0143885  0.0092280   1.559    0.125
#dens        0.0008373  0.0018680   0.448    0.656
# edf Ref.df     F p-value
#s(nn)         1      1 0.002   0.961
#s(del.area)   1      1 0.682   0.413
#R-sq.(adj) =  -0.0285   Deviance explained = 2.76%
#GCV = 0.0015236  Scale est. = 0.0014148  n = 56
mart_matas_fwb2<-gam(wbtw~s(nn)+s(del.area)+dens,data = mart_matas_wf2)
plot(mart_matas_fwb2,residuals= TRUE, se=TRUE, shade=TRUE,all.terms = TRUE, main = "Weighted Betweeness")
summary(mart_matas_fwb2)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.0139563  0.0011472  12.165   <2e-16 ***
#  dens        0.0001236  0.0002322   0.532    0.597    
#  edf Ref.df     F p-value
#s(nn)         1      1 0.178   0.675
#s(del.area)   1      1 0.965   0.331
#R-sq.(adj) =  -0.0335   Deviance explained = 2.29%
#GCV = 2.3548e-05  Scale est. = 2.1866e-05  n = 56
mart_matas_fwc2<-gam(wcls~s(nn)+s(del.area)+dens,data = mart_matas_wf2)
plot(mart_matas_fwc2,residuals= TRUE, se=TRUE, shade=TRUE, all.terms=TRUE, main = "Weighted Closeness")
summary(mart_matas_fwc2)


#Interaction motifs

#Obtaining Median of non-zero interactions. 

martmat_fNA<-mart_matas_fruits

martmat_fNA<-mart_matas_fruits
martmat_fNA[martmat_fNA==0]<-NA
martmat_fNA

##Median excluding NAs(e.g. 0). "The median for proportional visits is (0.1667)". All datas above this value will mean strong interaction and all below weak interaction
median(as.matrix(martmat_fNA),na.rm=TRUE)














