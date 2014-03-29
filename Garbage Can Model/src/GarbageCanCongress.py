'''
Created on Mar 26, 2014

@author: stran_000
'''
Delimiter = ','  # or \t

from random import choice
from random import random
from random import shuffle
from random import getrandbits

Num_of_Representatives = 100
def setNumOfRepresentatives(n = Num_of_Representatives):
    global Num_of_Representatives
    Num_of_Representatives = n
    return n

Num_of_Issues = 10
def setNumOfIssues(n = Num_of_Issues):
    global Num_of_Issues
    Num_of_Issues = n
    return n

Solution_Bit_Length = 3
def setSolutionBitLength(l = Solution_Bit_Length):
    global Solution_Bit_Length
    Solution_Bit_Length = l
    return l

Satisfaction_Threshold = 0.5  # threshold of satisfaction for voting aye on a bill
One_Degree_Threshold = 0.5
Issue_Similarity_Level = 2

def pdf(values):
    sum_values = sum(values)
    return [v/sum_values for v in values]

def cdf(values):
    distribution = []
    sum_values = sum(values)
    sum_cdf = 0
    for value in values:
        sum_cdf += value/sum_values
        distribution.append(sum_cdf)
    return distribution

def randomFromCDF(distribution):
    # choose an index from distribution given the CDF in distribution
    # note that values in distribution must be monotonically increasing,
    # and distributed in the range (0+,1]!
    r = random()
    index = 0
    while(r > distribution[index]):
        index += 1
    return index

def bitwiseJaccardIndex(a, b):
    c = a^b
    count = 0
    for i in range(Solution_Bit_Length):
        count += (c%2)
        c = c>>1
    return 1 - count/Solution_Bit_Length

def binaryTreeSimilarity(a, b):
    c = a^b
    sim_level = 1.0
    sim = 1.0
    i = 1
    while c%2 == 0 and i < Solution_Bit_Length:
        c = c>>1
        sim_level /= 2
        i += 1
    if c%2==1:
        sim -= sim_level
    return sim

def dumpNetwork(network):
    file = open("../output/network_out.csv", 'w')
    for i in range(Num_of_Representatives):
        #print(network[i])
        for j in range(Num_of_Representatives):
            file.write("%1.5f %s"%(network[i][j], Delimiter))
        file.write("\n")
    # TODO dump to file

class Legislator(object):

    @staticmethod
    def generateNetwork(legislators):
        matrix = [[0 for i in range(Num_of_Representatives)] for j in range(Num_of_Representatives)]
        for rep1 in legislators:
            i = legislators.index(rep1)
            for rep2 in legislators[i:]: # only cycle through upper triangular, since diagonnally symmetric
                j = legislators.index(rep2)
                if rep1==rep2:
                    rep1.links[rep2] = 1
                    matrix[i][i] = 1
                else :
                    link_strength = 0
                    for issue in State.issues:
                        # calculate issue position overlap (Jaccard Index)
                        overlap = binaryTreeSimilarity(rep1.positions[issue],rep2.positions[issue])
                        # weight this issue's contribution to link strength by similarity of priorities
                        link_strength += overlap #*rep1.priorities[issue]
                    # normalize to number of issues and scale between [-1,1],
                    # so that maximum link strength is 1 (complete position and priority agreement)
                    # and minimum link strength is -1 (priority agreement, but polar opposite positions)
                    link_strength = (link_strength/Num_of_Issues)*2 - 1
                    rep1.links[rep2] = link_strength
                    rep2.links[rep1] = link_strength
                    matrix[i][j] = link_strength
                    matrix[j][i] = link_strength
        return matrix

    @staticmethod
    def generatePrioritizedNetwork(legislators):
        matrix = [[0 for i in range(Num_of_Representatives)] for j in range(Num_of_Representatives)]
        for rep1 in legislators:
            i = legislators.index(rep1)
            for rep2 in legislators:
                j = legislators.index(rep2)
                if rep1==rep2:
                    rep1.links[rep2] = 1
                    matrix[i][i] = 1
                else:
                    rep1_issues = sorted(State.issues, key = lambda i: rep1.priorities[i], reverse=True)
                    link_strength = 0
                    sum_pri_diffs = 0
                    for issue in rep1_issues[0:Issue_Similarity_Level]:
                        similarity = binaryTreeSimilarity(rep1.positions[issue], rep2.positions[issue])
                        pri_diff = 1 - abs(rep1.priorities[issue] - rep2.priorities[issue])
                        link_strength += similarity*pri_diff
                        sum_pri_diffs += pri_diff
                    link_strength = (link_strength/sum_pri_diffs)*2 - 1
                    rep1.links[rep2] = link_strength
                    matrix[i][j] = link_strength
        return matrix


    def __init__(self, default_priorities=[], default_positions=[]):
        self.priorities = {}
        self.positions = {}
        self.links = {} # network link strengths to other legislators
        if default_priorities:
            priorities = default_priorities
        else:
            priorities = [1 for i in range(Num_of_Issues)]  #seed as uniform
        # allocate priorities by preferential attachment
        # generates a power law distribution of priorities
        # i.e., legislators have high priority on a few issues,
        # medium priority on some issues,
        # and low priority on most issues
        # TODO test the resulting distribution
        for i in range(Num_of_Issues**2):
            priorities[randomFromCDF(cdf(priorities))] += 1
        # normalize the priorities to sum = 1
        priorities = pdf(priorities)
        shuffle(priorities)
        #assign priorities and positions
        for i in State.issues:
            self.priorities[i] = priorities.pop(0)
            self.positions[i] = getrandbits(Solution_Bit_Length)

    def proposeBill(self):
        issue = choice(self.priorities.keys()) # TODO:  get highest priority issue
        bill = Bill(self, (issue, self.positions[issue]))

    def getSatisfactionWithBill(self, bill):
        s = 0
        sum_pris = 0
        for issue in bill.solutions.keys():
            # sum up Jaccard indeces for solutions vs positions, weighted by priorities
            s += binaryTreeSimilarity(self.position[issue], bill.solutions[issue])*self.priorities[issue]
            sum_pris += self.priorities[issue]
        s /= sum_pris # normalize to relevant priorities
        return s

class State(object):

    issues = []  # TODO:  make this more complicated?
    # e.g., issues could be related by Jaccard index
    laws = {}  # a dictionary of passed laws (solutions to issues), key: issue

    def __init__(self):
        # initialize lawmakers
        self.legislators = []
        State.issues = range(Num_of_Issues)
        default_priorities = [15-3*i for i in range(5)]
        default_priorities += [1 for i in range(5,Num_of_Issues)]
        print(default_priorities)
        for i in range(Num_of_Representatives):
            self.legislators.append(Legislator(default_priorities))
        network = Legislator.generatePrioritizedNetwork(self.legislators)
        dumpNetwork(network)

    def step(self):
        sponsor = choice(self.legislators)
        bill = sponsor.proposeBill()
        self.circulateDraft(bill)
        self.referToCommittee(bill)
        self.putToVote(bill)

    def circulateDraft(self, bill):
        # STUB
        cosponsors = []
        bill.anneal(cosponsors)

    def referToCommittee(self, bill):
        # STUB
        committee = None
        bill.anneal(committee)

    def putToVote(self, bill):
        votes = 0
        for rep in self.legislators:
            if rep.getSatisfactionWithBill(bill) > Satisfaction_Threshold:
                votes += 1
        if votes > 0.5*Num_of_Representatives:
            self.makeLaw(bill)

    def makeLaw(self, bill):
        pass # TODO - code stuff below
        # transfer bill's solutions to laws dictionary
        # delete issues from legislators' priorities and positions (issues don't resurface once law is passed)

class Bill(object):

    def __init__(self, sponsor, issue_proposal):
        (issue, proposal) = issue_proposal
        self.solutions = {}
        self.sponsor = sponsor
        self.solutions[issue] = proposal

    def anneal(self):
        # STUB
        pass

if __name__ == "__main__":
    setSolutionBitLength(4)
    setNumOfIssues(20)
    setNumOfRepresentatives(100)
    s = State()

