### This is the entry point for analaysis of the garbage-can model of 
#   congress. The model uses simulated annealing to alter bills proposing
#   policy issues by adding minor policy issues to make them fit better with
#   other members' policy issue agendas.
# 
#   Subordinate files (history.R and networks.R) examine the changes each bill
#   undergoes before the floor vote and the network structure between members
#   when the bill was introduced.  We were curious what difference the entry
#   point of a bill might make on whether it passes (structurlly).
#
#   This analysis parses the model output from several experiments so that we
#   can see the differences in parameters like party affiliation mixes and 
#   the number and criticality of issues
#
################################################################################

library(lattice)
library(reshape2)
library(ggplot2)
library(plyr)


## Data
#  Establish the data directory relative to the working directory, then id
#  some column names 
dataDir <- "Garbage Can Model/output/" 

# Main Experiment With Parties => me.party
me.party <- read.csv("Garbage Can Model/output/dated/GCC Main Experiment - National Priorities - 2014-05-04 154654_mod.csv",
               strip.white = TRUE,
               header=TRUE,
               sep=",",
               colClasses=c(timestamp="character",
                            job.ID="factor",
                            setUnaffiliatedFraction="factor",
                            setGreenFraction="factor",
                            setStatePriorities="factor",
                            setIdeologyIssues="factor"
                            )
               )
# Main Experiment, No Parties => me.np
me.np <- read.csv("Garbage Can Model/output/dated/GCC Main Experiment - National Priorities - No Parties 2014-05-03 175733_mod.csv",
                 strip.white = TRUE,
                 header=TRUE,
                 sep=",",
                 colClasses=c(timestamp="character",
                              setStatePriorities="factor",
                              setIdeologyIssues="factor"
                              )
                 )

me.np$job.ID <- me.np$job.ID + 24
me.np$job.ID <- as.factor(me.np$job.ID)
me.np$setGreenFraction <- "0"
me.np$setUnaffiliatedFraction <- "1"

me <- rbind(me.party, me.np)

summary(me)

bwplot(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

bwplot(laws.count ~ provisions | setUnaffiliatedFraction + setStatePriorities + setIdeologyIssues, me.np)

qqmath(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

densityplot(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

histogram(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

barchart(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

dotplot(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

stripplot(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

tmd(me$laws.count ~ me$provisions | me$setUnaffiliatedFraction + me$setGreenFraction + me$setStatePriorities + me$setIdeologyIssues)

png("Garbage Can Model/analysis/plots/votes_byJob_jitterQuints.png", width=800 , height=300)
tv <- ggplot(me, aes(reorder(job.ID, total.votes, FUN=mean), total.votes)) +  
  geom_boxplot(aes(reorder(job.ID, total.votes, FUN=mean), total.votes, alpha=0)) + 
  geom_jitter (alpha=I(1/5)) + 
  stat_summary(fun.data = "mean_se", colour = "red") + 
  theme(axis.title.y=element_text(angle=0)) +
  labs(title="Total Votes by Case: \nJitters & quintiles, sorted by mean values (red)") +
  xlab("Cases, by ID") + 
  ylab("Total \nVotes")
tv
ggsave(tv, file="Garbage Can Model/analysis/plots/votes_byJob_jitterQuints.png", width=9.5, height=5.0, dpi=600)
dev.off()

png("Garbage Can Model/analysis/plots/laws_byJob_jitterQuints.png", width=800 , height=300)
laws <- ggplot(me, aes(reorder(job.ID, laws.count, FUN=mean), laws.count)) +  
  geom_boxplot(aes(reorder(job.ID, laws.count, FUN=mean), laws.count, alpha=0)) + 
  geom_jitter (alpha=I(1/5)) + 
#  geom_smooth(aes(group = 1), method = "loess", se = F, colour = "blue") +
  stat_summary(fun.data = "mean_se", colour = "red") + 
  theme(axis.title.y=element_text(angle=0)) +
  labs(title="New Laws Passed by Case: \nJitters & quintiles, sorted by mean values (red)") +
  theme(legend.position = "none") +
  xlab("Cases by ID") + 
  ylab("New \nLaws")
laws
ggsave(laws, file="Garbage Can Model/analysis/plots/laws_byJob_jitterQuints.png", width=9.5, height=5.0, dpi=600)
dev.off()


lawsf1 <- ggplot(me, 
                 aes(reorder(job.ID, laws.count, FUN=median), 
                     laws.count)) + 
  geom_jitter(alpha=I(1/5)) + 
  stat_summary(fun.data = "mean_se", colour = "red") + 
  theme(axis.title.y=element_text(angle=0)) +
  facet_grid(. ~ setStatePriorities + setIdeologyIssues) +
  labs(title="Quantiles with Jittered Total Laws Passed by Job, Sorted by Median Values") +
  xlab("Jobs by ID, State Priorities (top facet) and Ideology Issues (bottom facet)") + 
  ylab("New \nLaws")
lawsf1
ggsave(lawsf1, file="laws_byJob_byStatePrior_byIdeology.png")


lawsf2 <- ggplot(me, 
                 aes(reorder(job.ID, laws.count, FUN=median), 
                     laws.count)) + 
  geom_jitter(alpha=I(1/5)) + 
  stat_summary(fun.data = "mean_se", colour = "red") + 
  theme(axis.title.y=element_text(angle=0)) +
  facet_grid(. ~ setUnaffiliatedFraction + setGreenFraction) +
  labs(title="Quantiles with Jittered Total Laws Passed by Job, Sorted by Median Values") +
  xlab("Jobs by ID, Unaffiliated Fraction (top facet) and Green Fraction (bottom facet)") + 
  ylab("New \nLaws")
lawsf2
ggsave(lawsf2, file="laws_byJob_byUnaffiliated_byGreen.png")

# A smaller verion of the main experiment dataset; 
# jobs 1, 3, 5, 9, 13, 14, 15, 16, 17, 21 have been removed.
nonzero.jobs <- as.factor(c(2, 4, 6, 7, 8, 10, 11, 12, 18, 19, 20, 22, 23, 24))
mes <- subset(me, 
              select=c(job.ID, 
                     provisions, 
                     satisfaction, 
                     total.votes, 
                     laws.count,
                     total.change,
                     setUnaffiliatedFraction,
                     setGreenFraction,
                     setStatePriorities,
                     setIdeologyIssues),
              subset=(job.ID %in% nonzero.jobs))

mes$job.f <- factor(mes$job.ID, levels=c("2", "4", "6", "7", "8", "10", "11",
                                         "12", "18", "19", "20", "22", "23",
                                         "24"))

fac_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="job.ID") { 
    value[value] <- paste("Case ", value)
  } 
  if (var=="setGreenFraction") {
    value[value=="0.5"] <- "50% "
    value[value=="0.75"]<- "75% "
    value[value=="1"]   <- "100%"
  }
  if (var=="setUnaffiliatedFraction") {
    value[value=="0.05"] <- "5%  "
    value[value=="0.5" ] <- "50% "
  } 
  if (var=="setStatePriorities") {
    value[value=="0"] <- "No, 0 "
    value[value=="5"]<-  "Yes, 5"
  }
  if (var=="setIdeologyIssues") {
    value[value=="0"] <- "No, 0 "
    value[value=="5"] <- "Yes, 5"
  }
  return(value)
}

facLabels <- data.frame(c("Case #", "State Iss.", "Ideology Iss.", "% Unaffiliated", "% Green"))
facLabels

png("Garbage Can Model/analysis/plots/votes_byJob_jitterQuints.png", width=800 , height=300)

prov.sat <- qplot(provisions, satisfaction, data=mes,
             geom="point", alpha = I(1/5), 
             main="Average Satisfaction by Number of Additional Provisions \nfor Select Jobs",
             xlab = "Number of Additional Provisions", 
             ylab = "Satisfaction") +
  geom_smooth(method = "lm", se = F) + 
#  scale_y_log10() +
  theme(axis.title.y=element_text(angle=0), axis.text.y=element_text(size=5.5)) +
  theme(axis.text.x=element_text(size=5.5)) +
  theme(strip.text.y=element_text(size=7, angle=0, hjust=0, vjust=0.9)) +
  facet_grid(. ~ job.f + 
               setStatePriorities + 
               setIdeologyIssues +
               setUnaffiliatedFraction +
               setGreenFraction#,
#               labeller=fac_labeller
             ) 
+ theme(strip.background = element_blank(), strip.text.x = element_blank())

prov.sat

prov.sat + annotation_custom(grob = tableGrob(head(iris[ ,1:3])),
                             xmin = 3, xmax = 6, ymin = 2, ymax = 8)

# ## Get the title style from the original plot
# g <- ggplotGrob(prov.sat)
# legend_style <- g$grobs[[73]]$gp
# 
# ## Add the second title and plot
# g2 <- gtable_add_grob(g, textGrob("Right", x=1, hjust=1, gp=legend_style),
#                       t=2, l=4, b=2, r=4, name="right-title")
# grid.draw(g2)



ggsave(prov.sat, file="main_sat_prov_byJob.png")

prov.votes <- qplot(provisions, total.votes, data=mes,
                  geom="point", alpha = I(1/4), 
                  main="Total Votes by Number of Additional Provisions: \nfor select cases",
                  xlab = "Number of Additional Provisions by (top-to-bottom): \nJob ID, # State Priorities, #Ideology Issues, % Unaffiliated, %Green Party", 
                  ylab = "Total \nVotes") +
  geom_smooth(method = "lm", se = T) +
  theme(axis.title.y=element_text(angle=0), 
        axis.text.y=element_text(size=5.5)) +
  theme(axis.text.x=element_text(size=5.5)) +
  facet_grid(. ~ job.f + 
               setStatePriorities + 
               setIdeologyIssues +
               setUnaffiliatedFraction +
               setGreenFraction#,
             #labeller=fac_labeller
               )
prov.votes

ggsave(prov.votes, file="main_votes_prov_byJob_byPrior_byIdeoIssues.png")


# How many bills became laws in this matrix?
prov.laws <- qplot(provisions, laws.count, data=mes,
                    geom="point", alpha = I(1/5), 
                    main="Laws Passed by Number of Additional Provisions \nfor Select Jobs",
                    xlab = "Number of Additional Provisions by by (top-to-bottom): \nJob ID, # State Priorities, #Ideology Issues, % Unaffiliated, %Green Party", 
                    ylab = "Laws \nPassed") +
  geom_smooth(method = "lm", se = T) + 
  theme(axis.title.y=element_text(angle=0), axis.text.y=element_text(size=5.5)) +
  theme(axis.text.x=element_text(size=5.5)) +
  theme(strip.text.y=element_text(size=7, angle=0, hjust=0, vjust=0.9)) +
  facet_grid(. ~ job.f + 
               setStatePriorities + 
               setIdeologyIssues +
               setUnaffiliatedFraction +
               setGreenFraction
             )  
prov.laws
ggsave(prov.laws, file="main_laws_prov_byJob.png")

###################################################
# 
# That's a lot of plotting for one paper/article, so I combine the plots
# into something a bit more dense.

# First, redo each of these narrow plots with no facet labels, titles/labels
# only where appropriate and with a shorter y axis.
require(grid)

vplayout <- function(x,y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
}

# Satisfaction by provision; to be combined
cSat <- qplot(provisions, satisfaction, data=mes,
                  geom="point", alpha = I(1/5), 
                  main="Average Satisfaction and New Laws \nby number of Additional Provisions - Select Cases\n", ylab = "Satisfaction") +
  geom_smooth(method = "lm", se = F) + 
  theme(axis.text=element_text(size=5.5)) +
  theme(axis.title.x=element_blank()) +
  facet_grid(. ~ job.f + 
               setStatePriorities + 
               setIdeologyIssues +
               setUnaffiliatedFraction +
               setGreenFraction,
               labeller=fac_labeller
             ) 

# New Laws by provision, to be combined
cLaws <- qplot(provisions, laws.count, data=mes,
               geom="point", 
               alpha = I(1/5), 
               ylab = "New Laws", 
               xlab = "Additional Povisions through simulated annealing") +
  geom_smooth(method = "lm", se = T) + 
  facet_grid(. ~ job.f + 
               setStatePriorities + 
               setIdeologyIssues +
               setUnaffiliatedFraction +
               setGreenFraction
    ) +
  theme(axis.text=element_text(size=5.5), 
        #axis.title.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()
        ) 
cLaws

# Votes by provision, to be combined

cVotes <- qplot(provisions, total.votes, data=mes,
                geom="point", alpha = I(1/5),
                ylab = "Total Votes", xlab = "Additional povisions through simulated annealing for each Case") +
  geom_smooth(method = "lm", se = T) + 
  facet_grid(. ~ job.f + 
               setStatePriorities + 
               setIdeologyIssues +
               setUnaffiliatedFraction +
               setGreenFraction
  ) +
  theme(axis.text=element_text(size=5.5), 
        strip.background = element_blank(),
        strip.text.x = element_blank()
  ) 

cVotes

# Combination of Satisfaction, Laws and Votes into a single viewport

pdf("combinedCases.pdf", width=8, height=8.5, pointsize=10)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))

print(cSat, vp = vplayout(1,1))
print(cLaws, vp =vplayout(2,1))
#print(cVotes, vp = vplayout(3,1))

#ggsave("combinedCases.png", width=8.5, height=11, dpi=600)
dev.off()






# So how much do these things matter anyway? 
mrm <- lm(mes$laws.count ~ mes$provisions * mes$total.votes * mes$satisfaction)
summary(mrm)
# Not such good predictors of the outcome, it seems (duh)

mrme <- lm(me$total.change ~ me$provisions )
             #mes$total.votes * 
             #mes$satisfaction * 
             #mes$setUnaffiliatedFraction * 
             #mes$setGreenFraction * 
             #mes$setStatePriorities )
             #mes$setIdeologyIssues) 

red.mrme <- step(mrme, direction="backward")
# based on the stepwise comparisson, I'm backing out the total.votes (probably
# autocorrelated with laws.count, satisfaction and setIdeologyIssues)
summary(red.mrme)
summary(mrme)

confint(mrme, level=0.99)

###############################################################################
# 
# We now have a theory that the total votes and laws passed depend on whether
# there are state priorities, ideology issues, the unaffiliated fraction and 
# the Green fraction. Let's try a regression model on those...
#
###############################################################################
lm4 <- lm(me$total.votes ~ me$setUnaffiliatedFraction * 
            me$setGreenFraction * me$setStatePriorities * me$setIdeologyIssues)
summary(lm4)

require(dplyr)
law.sum <- group_by(me, job.ID, setUnaffiliatedFraction, setGreenFraction,
                    setStatePriorities, setIdeologyIssues)
law.des <- summarise(law.sum)
law.des




## Citations for R and R packages used here
pks <- c("igraph", "network", "sna", "ergm", "qgraph", "reshape2", "ggplot2", 
         "plyr", "lattice")
lapply(pks, citation)


