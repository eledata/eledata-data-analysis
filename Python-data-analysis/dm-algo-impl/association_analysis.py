#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月24日

@author: hmy
'''

"""
Description     : Simple Python implementation of the Apriori Algorithm
FP-Growth FP means frequent pattern
the FP-Growth algorithm needs: 
1. FP-tree (class treeNode)
2. header table (use dict)

"""
################################################################################
# Apriori Algo
################################################################################
import sys
from itertools import chain, combinations
from collections import defaultdict

class apriori(object):
    
    def __init__(self, filePath, minSup, minConf):
        self.filePath = filePath
        self.minSup = minSup
        self.minConf = minConf
        
    # Read data, Transaction 1, frozenset(['MBE', 'New York', 'ASIAN', '10002'])
    def _readDataFile(self):
        fileOpen = open(self.filePath, 'rU') # rU 自动探测尾行
        
        for line in fileOpen:         
            line = line.strip().rstrip(',')
            recordSet = frozenset(line.split(','))
            yield recordSet
    
    def _getItemSetTranscationList(self, recordSet):
        transcationList = []
        itemSet = set()
        for transcation in recordSet:
            transcationList.append(transcation)
            for item in transcation:
                itemSet.add(frozenset([item]))
        return itemSet, transcationList
    
    def _getItemWithMinSupport(self, itemSet, transcationList):
        localItemSet = defaultdict(int)
        _itemSet = set()
        
        for item in itemSet:
            for trans in transcationList:
                if item.issubset(trans):
                    localItemSet[item] += 1
        
        N = len(transcationList)
        for item, count in localItemSet.items():
            itemSup = float(count)/N
            if itemSup >= self.minSup:
                _itemSet.add(item)
        
        for key in _itemSet:
            if not localItemSet.has_key(key):
                del localItemSet[key]
        
        for key, value in localItemSet.items():
            localItemSet[key] = float(value)/N
                        
        return _itemSet, localItemSet

    def _joinSet(self, itemSet, k):
        joinSet = set()
        for i in itemSet:
            for j in itemSet:
                if len(i.union(j)) == k:
                    joinSet.add(i.union(j))
        return joinSet
    
    def _subSets(self,arr):
        return chain(*[combinations(arr, i + 1) for i, a in enumerate(arr)])

    def printResult(self, items, rules):
        for item, support in sorted(items, key=lambda (item, support): support):
            print "item: %s , %.3f" % (str(item), support)
        print "\n------------------------ RULES:"
        for rule, confidence in sorted(rules, key=lambda (rule, confidence): confidence):
            pre, post = rule
            print "Rule: %s ==> %s , %.3f" % (str(pre), str(post), confidence)
    
    def apriori(self):
        recordSet = self._readDataFile()
        itemSet, transcationList = self._getItemSetTranscationList(recordSet)
        largeSet = {}
        localFreqItemSet = {}
        localItemSet = {}
        localItemSet, localFreqItemSet = self._getItemWithMinSupport(itemSet, transcationList)
        
        k = 1
        while(localItemSet != set([])):
            largeSet[k] = localItemSet
            joinlocalItemSet = self._joinSet(localItemSet, k + 1)
            localItemSet, locfeqset = self._getItemWithMinSupport(joinlocalItemSet, transcationList)
            dictMerged = dict(localFreqItemSet,**locfeqset) # Dict copy, save freqitem and its' rate
            localFreqItemSet.clear()
            localFreqItemSet = dictMerged.copy()
            k = k + 1
        
        toRetItems = []
        for key, value in largeSet.items():
            toRetItems.extend([(tuple(item), localFreqItemSet[item]) for item in value])
     
        toRetRules = []
        for key, value in largeSet.items()[1:]:
            for item in value:
                _subsets = map(frozenset, [x for x in self._subSets(item)]) # Split into seg
                for element in _subsets:
                    remain = item.difference(element) # item - element
                    if len(remain) > 0:
                        confidence = localFreqItemSet[item]/localFreqItemSet[element]
                        if confidence >= self.minConf:
                             toRetRules.append(((tuple(element), tuple(remain)),confidence))
                              
        return toRetItems, toRetRules


# Test
file_path = 'C:/Users/hmy/Documents/GitHub/sk-learn/dm-algo-impl/dataset/INTEGRATED-DATASET.csv'
ap = apriori(file_path,0.15, 0.2)
reitem, rerule = ap.apriori()
# ap.printResult(reitem, rerule)


################################################################################
# FPGrowth Algo
################################################################################

class treeNode(object):
    
    def __init__(self, nameValue, numOccur, parentNode):
        self.name = nameValue
        self.count = numOccur
        self.parentNode = parentNode
        self.childNode = {}
        self.nodeLink = None

    def _inc(self, numOccur):
        self.count += numOccur
        
    def _disp(self, ind = 1):
        print ' '*ind, self.name, ' ', self.count
        for value in self.childNode.values():
            value._disp(ind + 1)

def loadSimpDat():
    simpDat = [['r', 'z', 'h', 'j', 'p'],
               ['z', 'y', 'x', 'w', 'v', 'u', 't', 's'],
               ['z'],
               ['r', 'x', 'n', 'o', 's'],
               ['y', 'r', 'x', 'z', 'q', 't', 'p'],
               ['y', 'z', 'x', 'e', 'q', 's', 't', 'm']]
    return simpDat

def createInitDataSet(dataSet):
    dict = {}
    for trans in dataSet:
        dict[frozenset(trans)] = 1
    return dict

def updateHeader(nodeInNext, targetNode):   #this version does not use recursion
    while (nodeInNext.nodeLink != None):    #Do not use recursion to traverse a linked list!
        nodeInNext = nodeInNext.nodeLink
    nodeInNext.nodeLink = targetNode
    
def updateTree(item, parentTree, headNodeTable, count):
    if item[0] in parentTree.childNode:
        parentTree.childNode[item[0]]._inc(count)
    else:
        parentTree.childNode[item[0]] = treeNode(item[0], count, parentTree)
        if headNodeTable[item[0]][1] is None:
            headNodeTable[item[0]][1] = parentTree.childNode[item[0]]
        else:
            updateHeader(headNodeTable[item[0]][1], parentTree.childNode[item[0]])
    if len(item) > 1:
        updateTree(item[1::], parentTree.childNode[item[0]], headNodeTable, count)
    
def createTree(dataSet, minSup = 1):
    headNodeTable = {}
    
    # item freq number
    for trans in dataSet:
        for item in trans:
            headNodeTable[item] = headNodeTable.get(item, 0) + 1
    
    # del <　minSup item
    for key, value in headNodeTable.items():
        if value < minSup:
            del headNodeTable[key]
    
    freqSet = set(headNodeTable.keys())
    if len(freqSet) == 0: return None, None # Tree, head node table
    
    # Add point to the same node in FP tree.
    for k in headNodeTable:
        headNodeTable[k] = [headNodeTable[k], None]
    
    # Create Root Node for Tree
    retTree = treeNode('Null Set', 1, None)
    
    # 2nd scan dataSet for create new transaction.
    for trans in dataSet:
        local = {}
        for item in trans:
            if item in freqSet:
                local[item] = headNodeTable[item][0]
        count = 1
        # Create FP tree for new Trans.
        if len(local) > 0:
            # sort by freq value
            orderedItems = [v[0] for v in sorted(local.items(), key=lambda p: p[1], reverse=True)]
            updateTree(orderedItems, retTree, headNodeTable, count)
    return retTree, headNodeTable

simpleData = loadSimpDat()
dataSet = createInitDataSet(simpleData)
tree, head = createTree(dataSet, 3)
tree._disp()











