library(ape)
library(geiger)

cyperaceae<-read.nexus("cyperaceae.nex")

#Chromosome numbers file
chrom <- read.table("chrom.txt", header=F,sep=";") 

phy<-drop.tip(cyperaceae, setdiff(cyperaceae$tip.label, chrom[,1]))

#Add chromosome number on tips for plot
#row.names(chrom) = paste(chrom[,1], chrom[,2], sep="_")
#phy$tip.label <- row.names(chrom)[match(phy$tip.label,chrom[,1])]
#plot.phylo(phy, cex = 0.2,type="fan",no.margin=T, label.offset= 1)


#Name the nodes

Node4 <- node.leaves(phy, mrca(phy)["Bulbostylis_barbata", "Carex_lyngbyei"])
Node3 <- node.leaves(phy, mrca(phy)["Cyperus_laevigatus", "Cyperus_alopecuroides"])
Node2 <- node.leaves(phy, mrca(phy)["Carex_pseudorufa", "Carex_lyngbyei"])
Node1 <- node.leaves(phy, mrca(phy)["Bulbostylis_barbata", "Cyperus_alopecuroides"])
  

####################################
## Now we can create the subtrees ##
####################################

#Run this in order to write properly the txt files of each node
row.names(chrom) = paste(chrom[,1], chrom[,2], sep="\n")

#Null
write.tree(phy, file="cyperaceae/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(phy$tip.label,chrom[,1])]),
  file="cyperaceae/cyperaceae.txt")

#Node 1
Subtree.1 <- drop.tip(phy,setdiff(phy$tip.label,Node1),subtree=F)
plot.phylo(Subtree.1, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.1, file="cyperaceae1/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.1$tip.label,chrom[,1])]),
  file="cyperaceae1/cyperaceae.txt")

#1BG

Subtree.1BG <- drop.tip(phy,Node1,subtree=F)
plot.phylo(Subtree.1BG, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.1BG, file="cyperaceae1bg/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.1BG$tip.label,chrom[,1])]),
  file="cyperaceae1bg/cyperaceae.txt")

#Node 2
Subtree.2 <- drop.tip(phy,setdiff(phy$tip.label,Node2),subtree=F)
plot.phylo(Subtree.2, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.2, file="cyperaceae2/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.2$tip.label,chrom[,1])]),
  file="cyperaceae2/cyperaceae.txt")

#2BG

Subtree.2BG <- drop.tip(phy,Node2,subtree=F)
plot.phylo(Subtree.2BG, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.2BG, file="cyperaceae2bg/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.2BG$tip.label,chrom[,1])]),
  file="cyperaceae2bg/cyperaceae.txt")

#Node 3
Subtree.3 <- drop.tip(phy,setdiff(phy$tip.label,Node3),subtree=F)
plot.phylo(Subtree.3, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.3, file="cyperaceae3/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.3$tip.label,chrom[,1])]),
  file="cyperaceae3/cyperaceae.txt")

#3BG

Subtree.3BG <- drop.tip(phy,Node3,subtree=F)
plot.phylo(Subtree.3BG, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.3BG, file="cyperaceae3bg/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.3BG$tip.label,chrom[,1])]),
  file="cyperaceae3bg/cyperaceae.txt")

#Node 4
Subtree.4 <- drop.tip(phy,setdiff(phy$tip.label,Node4),subtree=F)
plot.phylo(Subtree.4, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.4, file="cyperaceae4/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.4$tip.label,chrom[,1])]),
  file="cyperaceae4/cyperaceae.txt")

#4BG

Subtree.4BG <- drop.tip(phy,Node4,subtree=F)
plot.phylo(Subtree.4BG, cex = 0.4,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.4BG, file="cyperaceae4bg/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.4BG$tip.label,chrom[,1])]),
  file="cyperaceae4bg/cyperaceae.txt")



# Now the more complex trees

#Background nodes 1 and 2

Subtree.12BG <- drop.tip(phy,c(Node1,Node2),subtree=F)
plot.phylo(Subtree.12BG, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.12BG, file="cyperaceae12bg/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.12BG$tip.label,chrom[,1])]),
  file="cyperaceae12bg/cyperaceae.txt")


#Node 1-3
Subtree.13 <- drop.tip(drop.tip(phy,setdiff(phy$tip.label,Node1),subtree=F), Node3)
plot.phylo(Subtree.13, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.13, file="cyperaceae13/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.13$tip.label,chrom[,1])]),
  file="cyperaceae13/cyperaceae.txt")


#Node 4-1
Subtree.41 <- drop.tip(drop.tip(phy,setdiff(phy$tip.label,Node4),subtree=F), Node1)
plot.phylo(Subtree.41, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.41, file="cyperaceae41/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.41$tip.label,chrom[,1])]),
  file="cyperaceae41/cyperaceae.txt")


#Background nodes 2 and 3
Subtree.23BG <- drop.tip(phy,c(Node2,Node3),subtree=F)
plot.phylo(Subtree.23BG, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.23BG, file="cyperaceae23bg/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.23BG$tip.label,chrom[,1])]),
  file="cyperaceae23bg/cyperaceae.txt")

#Node 4-2
Subtree.42 <- drop.tip(drop.tip(phy,setdiff(phy$tip.label,Node4),subtree=F), Node2)
plot.phylo(Subtree.42, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.42, file="cyperaceae42/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.42$tip.label,chrom[,1])]),
  file="cyperaceae42/cyperaceae.txt")


#Node 4-3
Subtree.43 <- drop.tip(drop.tip(phy,setdiff(phy$tip.label,Node4),subtree=F), Node3)
plot.phylo(Subtree.43, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.43, file="cyperaceae43/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.43$tip.label,chrom[,1])]),
  file="cyperaceae43/cyperaceae.txt")


#Node 4-2-1
Subtree.421 <- drop.tip(drop.tip(phy,setdiff(phy$tip.label,Node4),subtree=F), c(Node2,Node1))
plot.phylo(Subtree.421, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.421, file="cyperaceae421/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.421$tip.label,chrom[,1])]),
  file="cyperaceae421/cyperaceae.txt")

#Node 4-2-3
Subtree.423 <- drop.tip(drop.tip(phy,setdiff(phy$tip.label,Node4),subtree=F), c(Node2,Node3))
plot.phylo(Subtree.423, cex = 0.7,type="fan",no.margin=F, label.offset= 0.05)
write.tree(Subtree.423, file="cyperaceae423/cyperaceae.tree")
write(paste0(
  ">",
  row.names(chrom)[match(Subtree.423$tip.label,chrom[,1])]),
  file="cyperaceae423/cyperaceae.txt")

