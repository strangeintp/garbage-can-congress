'''
Created on May 3, 2014

@author: Vince (dev)
'''

from experiment import Experiment
from utility import getTimeStampString
import GarbageCanCongress as gcc

class GCC_Experiment(Experiment):

    def __init__(self):
        super(GCC_Experiment, self).__init__()        
        global timestamp
        gcc.timestamp = self.datetime
        self.state = None
        self.run_output = None
        gcc.setDebug(False)
        gcc.writeHistories(False)
        gcc.setVerbose(False)
        gcc.setSolutionBitLength(4)
        if gcc.DEBUG:
            self.directory = "../test_output/"
        self.max_proposals = 200

    def initiateSim(self):
        global timestamp
        gcc.timestamp = self.datetime
        self.state = gcc.State()
        self.proposals = 0

    def stopSim(self):
        condition = (self.proposals == self.max_proposals or len(gcc.State.open_issues)==0)
        if condition:
            self.run_output = self.state.closeout() 
        return condition

    def stepSim(self):
        self.state.step()
        self.proposals += 1
        
    def getProposals(self):
        return self.proposals

    def getLawCount(self):
        return self.run_output[0]

    def getProvisionCount(self):
        return self.run_output[1]
    
    def getTotalSatisfaction(self):
        return self.run_output[2]
    
    def getTotalChange(self):
        return self.run_output[3]
    
    def getTotalVotes(self):
        return self.run_output[4]

    def setupOutputs(self):
        self.addOutput(self.getProposals, "proposals", "%1.2f")
        self.addOutput(self.getLawCount, "laws count", "%1.2f")
        self.addOutput(self.getProvisionCount, "provisions", "%1.2f")
        self.addOutput(self.getTotalSatisfaction, "satisfaction", "%1.4f")
        self.addOutput(self.getTotalChange, "total change", "%1.4f")
        self.addOutput(self.getTotalVotes, "total votes", "%1.4f")

    def setupParameters(self):
        self.addParameter(gcc.setNumOfIssues, 75)
        self.addParameter(gcc.setNumOfRepresentatives, 100)
        self.addParameter(gcc.setSatisfactionThreshold, [0.675])
        self.addParameter(gcc.setUnaffiliatedFraction, [0.05, 0.25, 0.5, 0.75])
        self.addParameter(gcc.setGreenFraction, [0.5, 0.75, 0.9])
        self.addParameter(gcc.setIdeologyIssues, [10, 7, 5])

    def setupExperiment(self):
        self.Name = "GCC Main Experiment"
        self.comments = "Ideology Variations, 30 realizations per configuration "
        self.setupParameters()
        self.job_repetitions = 30

class GCC_Network_Experiment(GCC_Experiment):

    def __init__(self):
        super(GCC_Network_Experiment, self).__init__() 
        self.directory = "../output/networks/"
        gcc.setDebug(True, self.directory)
        gcc.writeHistories(True)
        self.max_proposals = 1  # histories not needed for this experiment

    def setupParameters(self):
        self.addParameter(gcc.setNumOfIssues, 75)
        self.addParameter(gcc.setNumOfRepresentatives, 100)
        self.addParameter(gcc.setSatisfactionThreshold, [0.675])
        self.addParameter(gcc.setUnaffiliatedFraction, [0.05, 0.5])
        self.addParameter(gcc.setGreenFraction, [0.5, 0.75, 1.0])
        self.addParameter(gcc.setStatePriorities, [0, 5])
        self.addParameter(gcc.setIdeologyIssues, [1, 5])

    def setupExperiment(self):
        self.Name = "GCC Network Experiment"
        self.comments = "Variations to look at networks under different conditions.\n"
        self.comments += "Since this is just looking at networks, no proposals are run."
        self.setupParameters()
        self.job_repetitions = 3
        
if __name__ == "__main__":
    #runOnce()
    #GCC_Experiment().run()
    GCC_Network_Experiment().run()
          