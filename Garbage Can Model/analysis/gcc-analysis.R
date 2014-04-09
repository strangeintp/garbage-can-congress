## This is some network analysis of the matrix outputs from Vince's 
#  python simulation.
##

## Overhead
#  RStudio does this kind of stuff for me, but in case you use R from the 
#  command line, remember to set the current working directory
#setwd("~/Code/CSS739_COT/garbage-can-congress/")


## Libraries
#  I prefer iGraph for network analysis and viz. It seems to be faster than
#  the more commonly-used NetworkX package. Be aware, however, that igraph
#  objects are dense (almost mystical) things that are best dealt with only
#  using the igraph methods and no other way.
library("igraph", lib.loc="/usr/lib64/R/library")
library("ggplot2", lib.loc="/usr/lib64/R/library")


## Data
#  I converted the .xlsx files to .csv with OpenCalc and import them here first
#  as objects, then as graphs (via matrices, because dataframes don't become
#  graph matrices) 

df1_05 <- read.csv("Garbage Can Model/output/network_out_1_05.csv", 
                   header=F)
m1_05 <- as.matrix(df1_05)
g1_05 <- graph.adjacency(m1_05, 
                         mode="lower", 
                         weighted=TRUE, 
                         diag=FALSE)

df1_10 <- read.csv("Garbage Can Model/output/network_out_1_10.csv", 
                   header=F)
g1_10 <- graph.adjacency(as.matrix(df1_10), # a little more directly
                         mode="lower", 
                         weighted=TRUE, 
                         diag=FALSE)

df2_05 <- read.csv("Garbage Can Model/output/network_out_2_05.csv", 
                   header=F)
g2_05 <- graph.adjacency(as.matrix(df2_05), 
                         mode="lower", 
                         weighted=TRUE, 
                         diag=FALSE)

df2_10 <- read.csv("Garbage Can Model/output/network_out_2_10.csv", 
                   header=F)
g2_10 <- graph.adjacency(as.matrix(df2_10), 
                         mode="lower", 
                         weighted=TRUE, 
                         diag=FALSE)


## Exploration
#  Now that we have a few sets of data to look at, let's do some visual 
#  exploration to see what's interesting about the data. Start small...

# Of course, we know that every attribute is measured against every other 
# attribute, so the degree for every vector is 99.
degree(g1_05)
# and the hairball shows us what we're up against if we're comparing how
# things relate to one another.
plot(g1_05)
# Wow. That is incredibly uninformative. Ultimately, we'll be interested 
# in the before and after analyses; what the measures look like after each
# anealing process

g1_05  # Tells us about the graph: UNW means undirected, named, weighted

#  iGraph has a handy function that sums-up the edge weights of the edge 
#  connecting each vertex. We shouldn't have any loops and this is an 
#  undirected graph so there's no sense comparing in/out/all/total

gs1_05 <- graph.strength(g1_05)
head(gs1_05)
plot(gs1_05,ylab="strength", xlab="vertex")
# That's too scattered to make any sense.  Let's see what happens if we 
# reorder by strength values. Can we see that some vertices are stronger
# and some weaker? 
gsdf1_05 <- as.data.frame(graph.strength(g1_05))
colnames(gsdf1_05) <- "strength"
k1_05 <- order(gsdf1_05$strength)
ogs1_05 <- gsdf1_05[k1_05,]
plot(ogs1_05,ylab="strength", xlab="vertex")
# That's odd, isn't it? why do the values cluster like that, I wonder?
ogs1_05
# We consistently get a step of .8 over a range -7.8--5.8, it seems. Likely 
# an artifact of the number of edges?

# I think all this is leading toward a multiple-column chart with vertex
# labels as rows and columns for fit 'as introduced', 'after socialization',
# 'after committee' and 'after floor'. There are some balance points at which
# 50% or more of vertices need to be on the positive side of the vote.







