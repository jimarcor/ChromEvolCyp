setwd("C:/Rprojects/ChromEvolCyp")

library(ape)
unprunedHinchRoal<-read.nexus("HinchliffRoalson2013.nex")
#write.table(nex$tip.label,file="tipHinchRoal.csv",row.names=F) #Save tiplabels as a xls in order to add chromosome data manually

unprunedSpalink<-read.nexus("Spalink2016.tree")
#write.table(nex$tip.label,file="tipSpalink.csv",row.names=F)


#AFTER ADDING MANUALLY CHROMOSOME NUMBER
#Importing txt with chromosome data
chromHinchRoal<-read.table("tipHinchRoal_chromnum.txt", header=F,sep=";") 
chromSpalink<-read.table("tipSpalink_chromnum.txt", header=F,sep=";")


#prune tree by matching tips (chromosomes count and tiplabel)
HinchChromtree<-drop.tip(phy=unprunedHinchRoal,setdiff(unprunedHinchRoal$tip.label,chromHinchRoal[,1]),subtree=F) #Pruning tips without chromosome data
HinchChromtree
plot.phylo(HinchChromtree, cex = 0.7,type="fan",no.margin=T)
write.tree(HinchChromtree, file="HinchChrom.tree")

#Add chromosome number on tips
row.names(chromHinchRoal) = paste(chromHinchRoal[,1], chromHinchRoal[,2], sep="_")
HinchChromtree$tip.label <- row.names(chromHinchRoal)[match(HinchChromtree$tip.label,chromHinchRoal[,1])]
plot.phylo(HinchChromtree, cex = 0.7,type="fan",no.margin=F, label.offset= 1)

#SAME FOR SPALINK
#prune tree by matching tips (chromosomes count and tiplabel)
SpalChromtree<-drop.tip(phy=unprunedSpalink,setdiff(unprunedSpalink$tip.label,chromSpalink[,1]),subtree=F)
SpalChromtree
plot.phylo(SpalChromtree, cex = 0.7,type="fan",no.margin=T)
write.tree(SpalChromtree, file="SpalChrom.tree")

#Add chromosome number on tips
row.names(chromSpalink) = paste(chromSpalink[,1], chromSpalink[,2], sep="_")
SpalChromtree$tip.label <- row.names(chromSpalink)[match(SpalChromtree$tip.label,chromSpalink[,1])]
plot.phylo(SpalChromtree, cex = 0.7,type="fan",no.margin=F, label.offset= 1)
