
#Opening a matrix file with weighted visits of frugivores in individual plants, 
# the "row.names=1" is necessary to run the model and to detect the "name of plants" 
# in the matrix. Note: visit frequency= animal-visi-days/total-survey days. 

#The first explored matrix was the visit frequency of frugivores at Martinazo 

mart<-read.csv("~/Desktop/Palmito/Postdoc-Interaction Networks/mart_visit.csv",header=TRUE,check.names=FALSE,row.names=1)
mart

#Checking row names
row.names(mart)

#Using "bipartite" package 
plotweb(mart)
visweb(mart)

#Describing a species' degree as sum of its links
spplevel<-specieslevel(mart)

#Coping the results to my folder in local repository
write.table(spplevel, "/Users/apple/Desktop/Palmito/Postdoc-Interaction Networks/Dispersal Networks/spplevel1.txt", sep="\t")

#nestedcontribution
#which computes the contribution of each species to the overall nestedness