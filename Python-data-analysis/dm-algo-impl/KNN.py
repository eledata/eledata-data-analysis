#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月27日
 
@author: HUANGMA
'''
import numpy as np
import os
from collections import Counter
 
# Build KD tree, and search KD tree. From <统计学习方法>
T = [[2, 3], [5, 4], [9, 6], [4, 7], [8, 1], [7, 2]]
 
class node(object):
    def __init__(self, point):
        self.left = None
        self.right = None
        self.parent = None
        self.point = point
        pass
 
    def setLeft(self, left):
        if left == None : pass
        left.parent = self
        self.left = left
         
    def setRight(self, right):
        if right == None: pass
        right.parent = self
        self.right = right
         
         
def median(lst):
    m = len(lst)/2
    return lst[m], m
 
def buildKDTree(dataSet, d):
    dataSet = sorted(dataSet, key = lambda x : x[d]) # 挑选哪一列的数据要排序
    point, m = median(dataSet)
    tree = node(point)
 
    del dataSet[m] # 生成树节点之后，就从数据集里面删掉
    if m > 0: tree.setLeft(buildKDTree(dataSet[:m], not d))
    if len(dataSet) > 1: tree.setRight(buildKDTree(dataSet[m:], not d))
    return  tree
 
def distance(a, b):
    # 欧式距离
    print a, b
    euclideanDist = ((a[0] - b[0])**2 + (a[1] - b[1])**2)**0.5       
    return euclideanDist
         
def searchKDTree(tree, target, d):
    if target[d] < tree.point[d]:
        if tree.left != None:
            return searchKDTree(tree.left, target, not d)
    else:
        if tree.right != None:
            return searchKDTree(tree.right, target, not d)
     
    def updateBest(t, best):
        if t == None: return
        dist = distance(t.point, target)
        if dist < best[1]:
            best[1] = dist
            best[0] = t.point
             
    best = [tree.point, 100000.0]
    while(tree.parent != None):
        updateBest(tree.parent.left, best)
        updateBest(tree.parent.right, best)
        tree = tree.parent
         
    return best[0]
 
# Test
#kd_tree = buildKDTree(T, 0)
#searchKDTree(kd_tree, [9,4], 0)
 
 
################################################################################
# ML Action KNN example
################################################################################
 
filePath = 'C:/Users/hmy/Documents/GitHub/sk-learn/dm-algo-impl/dataset/KNN/datingTestSet2.txt'
def readDataFromFile(filePath):
    fr = open(filePath, 'rU')
    lines = fr.readlines()
    lineNum = len(lines)
    dataMat = np.zeros((lineNum, 3))
    label = []
     
    index = 0
    for line in lines:
        list = line.strip().split('\t')
        dataMat[index, :] = list[0:3]
        label.append(int(list[-1]))
        index += 1
    return dataMat, label
 
# Data Normalization, dismiss the gap with value impact
def dataNorm(dataSet):
    dim = len(dataSet[0])
    for i in range(dim):
        max = np.max(dataSet[:,i])
        min = np.min(dataSet[:,i])
        featureLen = len(dataSet[:,i])
        dataSet[:,i] = (dataSet[:,i] - min)/(max - min)
     
    return dataSet
         
         
def knn(int_x, dataSet, label, k):
    dist = {}
    dim = len(dataSet[:,0])
    
    for i in range(dim):
        distance = (np.sum((dataSet[i] - int_x)**2))**0.5 # 欧氏距离
        dist[i] = distance

    top_k = {}
    labellst = []
    for i in range(k):
        minKey = min(dist.items(), key=lambda x: x[1])[0] #取dist最小的前k个样本
        minItem = min(dist.items(), key=lambda x: x[1])[1]
        top_k[minKey] = minItem
        del dist[minKey]
    
    for k, v in top_k.items():
        labellst.append(label[k])

    result = max(Counter(labellst).items(), key=lambda x: x[1])[0] # 取出类别频率最高的label
    return result    
    
# Test
def testKNN():
    dataMat, label = readDataFromFile(filePath)
    dataMat = dataNorm(dataMat)
    print label
    dataMat_Test = dataMat
    dim = len(dataMat[:,0])
    for i in range(5):
        print "The test result: %d, actual result: %d" % (knn(dataMat_Test[i], dataMat, label, 5),label[i])
    
#     count = 0
#     for i in range(dim):
#         if knn(dataMat_Test[i], dataMat, label, 5) == label[i]:
#             count += 1
#             
#     acc = float(count)/dim
# 
#     print acc

# testKNN()

################################################################################
# Hand Write Example
################################################################################
filePath_Train = 'C:/Users/hmy/Documents/GitHub/sk-learn/dm-algo-impl/dataset/KNN/digits/trainingDigits/'
filePath_Test = 'C:/Users/hmy/Documents/GitHub/sk-learn/dm-algo-impl/dataset/KNN/digits/testDigits/'

def dataReadFromDigitFiles(path):
    fr = open(path, 'r')
    readLines = fr.readlines()
    digitData = np.zeros((1,1024))
    
    count = 0
    for line in readLines:
        for i in range(32):
            digitData[0, count + i] = int(line[i])
        count += 32
        
    return digitData


# filePath_Train_name = filePath_Train + '0_0.txt'
# print dataReadFromDigitFiles(filePath_Train_name)

def handWriteKNN():
    trainFileNameLst = os.listdir(filePath_Train)
    testFileNameLst = os.listdir(filePath_Test)
    trainDataSet = []
    testDataSet = []
    
    # Read Train Data from folder
    for fileName in trainFileNameLst:
        filePath = filePath_Train + fileName
        trainDataSet.append(dataReadFromDigitFiles(filePath)[0])
    
    # Read Test Data from folder
    for fileName in testFileNameLst:
        filePath = filePath_Test + fileName
        testDataSet.append(dataReadFromDigitFiles(filePath)[0])
     
    trainDataMat = (np.array(trainDataSet))
    testDataMat = (np.array(testDataSet))

    dim = len(testDataMat[:,0])
    
    for i in range(5):
        print "The test result: %s, actual result: %s" %(knn(testDataMat[i], trainDataMat, trainFileNameLst, 5).split('_')[0] ,testFileNameLst[i].split('_')[0])
    
    count = 0
    for i in range(dim):
        if knn(testDataMat[i], trainDataMat, trainFileNameLst, 5).split('_')[0] == testFileNameLst[i].split('_')[0]:
            count += 1
             
    acc = float(count)/dim
 
    print acc
    
    
handWriteKNN()
    
    
    
    
    
    
    
    
    