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

df <- read.csv("Garbage Can Model/output/network_out.csv", 
                   header=F)
df <- df[c(1:100)] # some recent change added a comma @EOL; this fixes
m <- as.matrix(df)
g <- graph.adjacency(m,
                     mode="directed", 
                     weighted=TRUE, 
                     diag=FALSE)

## Exploration
#  Now that we have a few sets of data to look at, let's do some visual 
#  exploration to see what's interesting about the data. Start small...

# Of course, we know that every attribute is measured against every other 
# attribute, so the degree for every vector is 99.
degree(g)
# and the hairball shows us what we're up against if we're comparing how
# things relate to one another.
plot(g)
# Wow. That is incredibly uninformative. Ultimately, we'll be interested 
# in the before and after analyses; what the measures look like after each
# anealing process

g  # Tells us about the graph: UNW means undirected, named, weighted

#  iGraph has a handy function that sums-up the edge weights of the edge 
#  connecting each vertex. We shouldn't have any loops and this is an 
#  undirected graph so there's no sense comparing in/out/all/total

gs <- graph.strength(g)
head(gs)
plot(gs,ylab="strength", xlab="vertex")
# That's too scattered to make any sense.  Let's see what happens if we 
# reorder by strength values. Can we see that some vertices are stronger
# and some weaker? 
gsdf <- as.data.frame(graph.strength(g))
colnames(gsdf) <- "strength"
k <- order(gsdf$strength)
ogs <- gsdf[k,]
plot(ogs, ylab="strength", xlab="vertex")
# That's odd, isn't it? why do the values cluster like that, I wonder?
ogs
# We consistently get a step of .8 over a range -7.8--5.8, it seems. Likely 
# an artifact of the number of edges?

# I think all this is leading toward a multiple-column chart with vertex
# labels as rows and columns for fit 'as introduced', 'after socialization',
# 'after committee' and 'after floor'. There are some balance points at which
# 50% or more of vertices need to be on the positive side of the vote.

g.filter.pos <- delete.edges(g, which(E(g)$weight < .8))
g.filter.neg <- delete.edges(g, which(E(g)$weight > .5))
g.filter.all <- g.filter.pos + g.filter.neg

gs <- graph.strength(g)
plot(gs,ylab="strength", xlab="vertex")

degree(g.filter.pos)
degree(g.filter.neg)
degree(g.filter.all)

plot.igraph(g.filter.pos,
            edge.width=1.5*(E(g.filter.pos)$weight),
            edge.color="blue",
            vertex.size=3,
            vertex.label.cex=.5,
            edge.arrow.size=.2,
            layout=layout.circle)

plot.igraph(g.filter.neg,
            edge.width=(E(g.filter.neg)$weight),
            edge.color="red",
            vertex.size=1,
            vertex.label.cex=.5)

## Let's see what's going on with edge weights.
library("Hmisc", lib.loc="/usr/lib64/R/library")
hist(E(g)$weight)
qplot(E(g)$weight)

describe(E(g)$weight)




