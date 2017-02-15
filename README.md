##Step 1
#Opening a matrix file with weighted visits of frugivores in individual plants, the "row.names=1" is necessary to run the model and to detect the "name of plants" in the matrix. 
#Note: visit frequency= animal-visi-days/total-survey days.

mart<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_visit.csv",header=TRUE,check.names=FALSE,row.names=1)
mart

#Step 2. Checking row names
row.names(mart)

#Step 3. Using "bipartite" package 

#Step 3.1 Ploting the web. Gray scale in "col.low" goes to black(more aggregated plants) to white (more isolated) 
#;different colors in "col.high" means different guild white=predators,dark gray=defleshers, black=dispersers, light gray=unknow
#Creating the space for the two population networks
par(mfrow = c(1, 2))
plotweb(mart)
plotweb(mart, col.low=c(gray(seq(0.1,1,length=24))), col.high=c("white","white","gray30","gray30","black","black","darkgray"),method="normal", 
        text.rot=90,low.lablength=10, high.lablength=8)


visweb(mart)


#calculates closeness scores for nodes in a weighted network based on the distance_w-function.
closeness_w(mart, gconly=TRUE, precomp.dist=NULL, alpha=1)

# calculates betweenness scores for nodes in a weighted network based on the distance_w-function
betweenness_w(mart)

#Describing a species' degree as sum of its links
spplevel<-specieslevel(mart)

#Coping the results to my folder in local repository
write.table(spplevel, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/spplevel1.txt", sep="\t")

#nestedcontribution
#which computes the contribution of each species to the overall nestedness

linklevel(mart)

#Matasgordas
matas<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/matas_visit.csv",header=TRUE,check.names=FALSE,row.names=1)
matas

#Step 2. Checking row names
row.names(matas)
colnames(matas) <- c("deer","wildboar","rabbit","rat", "fox","badger", "unknow")
#Step 3. Using "bipartite" package 

#Step 3.1 Ploting the web
plotweb(matas)
plotweb(matas, col.low=c(gray(seq(0.1,1,length=39))), col.high=c("white","white","gray30","gray30","black","black","darkgray"), method="normal", 
        text.rot=90,low.lablength=10, high.lablength=8)