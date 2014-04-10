'''
Created on Mar 26, 2014

@author: stran_000
'''
Delimiter = ','  # or \t

from random import choice
from random import random
from random import shuffle
from random import getrandbits
from math import exp
from math import log

VERBOSE = True
def verbose(stuff):
    if VERBOSE:
        print(stuff)

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

Satisfaction_Threshold = 0.55  # threshold of satisfaction for voting aye on a bill
One_Degree_Threshold = 0.5
Issue_Similarity_Level = 3
PDFCM = 5 # Priority Depth For Committee Membership

# Annealer values
Min_Temp = 0.01
Max_Temp = 1
Min_Time = 1
Max_Time = 20
Time_Step = 2
k_B = -0.1/log(0.5)  # accept a decrease of 0.1 in satisfaction with a bill with probability 1/2 at temp=1.0

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
    sim_level = 0.5
    sim = 1.0
    i = 1
    while i <= Solution_Bit_Length:
        if c%2==1:
            sim -= sim_level
        c = c>>1
        sim_level /= 2
        i += 1
    
    return sim

def dumpNetwork(network):
    file = open("../output/network_out.csv", 'w')
    for i in range(Num_of_Representatives):
        #verbose(network[i])
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
        #issue = choice(list(self.priorities.keys())) # TODO:  get highest priority issue
        issues = sorted(list(State.issues), key = lambda issue : self.priorities[issue])
        issue = issues[-1]
        verbose("\nMain issue: %d" % issue)
        bill = Bill(self, (issue, self.positions[issue]))
        return bill
        
    def pickCoSponsors(self, bill=None):
        cosponsors = []
        for rep in self.links.keys():
            if self.links[rep] > 0.5:
                cosponsors.append(rep)
        return cosponsors
    
    def getSatisfactionWithBill(self, solutions):
        s = 0
        sum_pris = 0
        for issue in solutions.keys():
            # sum up Jaccard indeces for solutions vs positions, weighted by priorities
            s += binaryTreeSimilarity(self.positions[issue], solutions[issue])*self.priorities[issue]
            sum_pris += self.priorities[issue]
        s /= sum_pris # normalize to relevant priorities
        return s

class SmartLegislator(Legislator):
    '''
    Picks co-sponsors based on similarity of the bill main issue
    '''
    def pickCoSponsors(self, bill):
        '''
        Note that with an issue similarity depth of one for network formation, 
        and with only highest-priority issue bills being proposed,
        this method will not be any different than for the base class, since the same set of legislators
        will meet the criteria.
        '''
        cosponsors = []
        for rep in State.legislators:
            support = binaryTreeSimilarity(self.positions[bill.main_issue], rep.positions[bill.main_issue])
            if support > 0.5:
                cosponsors.append(rep)
        return cosponsors

class State(object):

    issues = []  
    laws = {}  # a dictionary of passed laws (solutions to issues), key: issue
    legislators = []

    def __init__(self):
        # initialize lawmakers
        State.legislators = []
        State.issues = range(Num_of_Issues)
        seed_priorities = [1 for i in range(Num_of_Issues)]#
        #seed_priorities = [1+i for i in range(Num_of_Issues)] #uncomment this for a skewed priority list
        #verbose(default_priorities)
        seed_positions = State.generatePartyPositions()
        for i in range(Num_of_Representatives):
            State.legislators.append(SmartLegislator(default_priorities=seed_priorities)) #, default_positions=seed_positions[i]))
        network = Legislator.generatePrioritizedNetwork(State.legislators)
        dumpNetwork(network)
        
    @staticmethod
    def generatePartyPositions():
        positions = []
        
        return positions

    def step(self):
        verbose("=================================   New Bill   =======================================================")
        sponsor = choice(State.legislators)
        bill = sponsor.proposeBill()
        verbose("Initial legislative body dissatisfaction: %1.4f" % bill.measureDisSatisfaction(reviewers=State.legislators))
        self.putToVote(bill)
        
        #circulate draft among cosposnors
        verbose("\nDraft review by Cosponsors:")
        cosponsors = sponsor.pickCoSponsors(bill)
        verbose("Number of cosponsors: %d" % len(cosponsors))
        self.circulateBill(bill, cosponsors)
        
        #circulate draft among committee
        verbose("\nCommittee revision:")
        committee = self.getCommitteeMembers(bill)
        verbose("Number of committee members: %d" % len(committee))
        self.circulateBill(bill, committee)
        
        # number of issues covered in bill's final form
        verbose("Number of issues addressed: %d" % len(bill.solutions.keys()))
        
        # put to vote
        verbose("\nFinal legislative body dissatisfaction: %1.4f" % bill.measureDisSatisfaction(reviewers=State.legislators))
        self.putToVote(bill)
        
    def getCommitteeMembers(self, bill):
        committee = []
        for rep in State.legislators:
            rep_prioritized_issues = sorted(State.issues, key = lambda issue : rep.priorities[issue])
            if bill.main_issue in rep_prioritized_issues[-PDFCM:-1]:
                committee.append(rep)
        return committee

    def circulateBill(self, bill, reviewers):
        bill.setReviewers(reviewers)
        verbose("Initial dissatisfaction: %1.4f"%bill.measureDisSatisfaction())
        revdash = bill.solutions.copy()
        (temps, times) = Annealer.configureLinearSchedule(Min_Temp, Max_Temp, Min_Time, Max_Time, Time_Step)
        revision = Annealer.anneal(revdash, bill.revise, bill.measureDisSatisfaction, k_B, temps, times)
        bill.solutions = revision.copy()
        verbose("Post-revision dissatisfaction: %1.4f"%bill.measureDisSatisfaction())

    def putToVote(self, bill):
        votes = 0
        for rep in State.legislators:
            if rep.getSatisfactionWithBill(bill.solutions) > Satisfaction_Threshold:
                votes += 1
        
        verbose("Number of votes: %d" % votes)
        if votes > 0.5*Num_of_Representatives:
            self.makeLaw(bill)

    def makeLaw(self, bill):
        # TODO - code stuff below
        # transfer bill's solutions to laws dictionary
        # delete issues from legislators' priorities and positions (issues don't resurface once law is passed)
        pass

class Bill(object):

    def __init__(self, sponsor, issue_proposal):
        (issue, proposal) = issue_proposal
        self.solutions = {}
        self.sponsor = sponsor
        self.solutions[issue] = proposal
        self.main_issue = issue
        self.reviewers = []
        
    def setReviewers(self, reviewers):
        self.reviewers = reviewers

    def revise(self, solutions):  # move bill to a neighbor in state-space
        revision = solutions.copy()
        issue = choice(State.issues)  # we could narrow this down so it only changes issues that are high-priority for reviewers
        # or pick one of the existing reviewers to choose an issue to revise according to reviewer's position
        bit = choice(range(Solution_Bit_Length))
        mask = 2**bit
        if issue in solutions.keys():  # if the issue is currently in the issues list,
            revision[issue] = revision[issue]^mask  # invert the bit in the current solution
        else:
            revision[issue] = mask # otherwise add it to the bill's issues list
        return revision
            
    def measureDisSatisfaction(self, solutions=None, reviewers=None):  # objective function
        if not reviewers:
            reviewers = self.reviewers
        if not solutions:
            solutions = self.solutions
        E = 0
        for reviewer in reviewers:
            E += reviewer.getSatisfactionWithBill(solutions)
        E /= len(reviewers)
        return -E
    
class Annealer(object):
    @staticmethod
    def anneal(initial_state, move_func, objective_func, k, schedule_temps, schedule_times):
        '''
        move_func := the function that moves the target from one point in the state-space to another
        objective_fun := the objective function that returns an energy E
        k := the constant that sets probability of acceptance for some energy E and some temperature T0
        schedule_temps := a list of temperatures to anneal at
        schedule_times := a list of durations to anneal at each temperature
        '''
        best_energy = objective_func(initial_state)
        best_state = initial_state.copy()
        prev_state = initial_state.copy()
        prev_energy = best_energy
        for step in range(len(schedule_temps)):
            temp = schedule_temps[step]
            time = schedule_times[step]
            for t in range(time):
                state = move_func(prev_state)
                energy = objective_func(state)
                delta_energy = energy-prev_energy
                # accept the new state if lower energy or probabilistically
                if energy < prev_energy:
                    prev_energy = energy
                    prev_state = state.copy()
                    if energy < best_energy:
                        best_energy = energy
                        best_state = state.copy()
                elif random() < exp(-delta_energy/(k*temp)):
                    prev_energy = energy
                    prev_state = state.copy()
        return best_state
    
    @staticmethod
    def configureLinearSchedule(min_temp, max_temp, min_time, max_time, time_step):
        times = list(range(max_time, min_time, -time_step))
        temp_step = (max_temp - min_temp)/len(times)
        temps = [min_temp+temp_step*i for i in range(len(times))]
        return (temps, times)
                    
if __name__ == "__main__":
    setSolutionBitLength(4)
    setNumOfIssues(30)
    setNumOfRepresentatives(100)
    s = State()
    for i in range(10):
        s.step()

