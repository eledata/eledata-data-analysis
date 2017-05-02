#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月21日

@author: hmy
'''

import numpy as np
import math

def calcShannonEnt(dataset):
    """
        计算训练数据集中的Y随机变量的香农熵
    """
    numentries = len(dataset)
    shannonent = 0.0
    prob = 0.0
    entries = np.unique(dataset)
    
    for i in range(len(entries)):
        prob = float(np.sum(np.equal(entries[i],dataset)))/(numentries)
        shannonent -= prob * math.log(prob,2)
        
    return shannonent


def calcConditionalEntropy(feature_x, y):
    '''
        条件熵
    '''
    ce = 0.0
    prob = 0.0
    feature_x_shape = feature_x.shape[0] # Features number
    ce_res = []
    
    for i in range(feature_x_shape):
        ft = feature_x[i] # Select one feature data
        features = np.unique(ft)
        numfeatures = len(ft)
        for i in range(len(features)):
            subdataset = y[np.equal(ft, features[i])] # Extract the y entries from features.
            prob = float(len(subdataset))/float(numfeatures)
            ce += prob*calcShannonEnt(subdataset)
        ce_res.append(ce)
        ce = 0.0
    return ce_res


def calcInformationGain(feature_x, y):
    """
        计算信息增益
    """
    baseent = calcShannonEnt(y)
    newent = calcConditionalEntropy(feature_x, y)
    infogain = []
    for i in range(len(newent)):
        infogain.append(baseent - newent[i])
    return infogain

def calcInformationGainRate(feature_x, y):
    """
        计算信息增益比
    """
    baseent = calcShannonEnt(y)
    newgain = calcInformationGain(feature_x, y)
    infogainrate = []
    for i in range(len(newgain)):
        infogainrate.append(newgain[i]/baseent)
    return infogainrate

def chooseBestFeatureToSplitByID3(feature_x, y):
    """
        选择最好的数据集划分方式
    """
    infogain = calcInformationGain(feature_x, y) # Info gain
    index_best_infogain = infogain.index(max(infogain))
    return index_best_infogain


def chooseBestFeatureToSplitByC45(feature_x, y):
    """
        选择最好的数据集划分方式
    """
    rate_infogain = calcInformationGainRate(feature_x, y) # Infogain rate
    index_best_infogain = rate_infogain.index(max(rate_infogain))
    return index_best_infogain


def majorityCnt(classList):
    """
        返回出现次数最多的分类名称
    :param classList: 类列表
    :return: 出现次数最多的类名称
    """
    classCount = {}  # 这是一个字典
    for vote in classList:
        if vote not in classCount.keys(): classCount[vote] = 0
        classCount[vote] += 1
    sortedClassCount = sorted(classCount.iteritems(), key=operator.itemgetter(1), reverse=True)
    return sortedClassCount[0][0]


def createTree(dataSet, labels, chooseBestFeatureToSplitFunc=chooseBestFeatureToSplitByID3):
    """
        创建决策树
    """
    classList = [example[-1] for example in dataSet]  # 类别列表
    if classList.count(classList[0]) == len(classList):
        return classList[0]  # 当类别完全相同则停止继续划分
    if len(dataSet[0]) == 1:  # 当只有一个特征的时候，遍历完所有实例返回出现次数最多的类别
        return majorityCnt(classList)
    bestFeat = chooseBestFeatureToSplitFunc(dataSet)
    bestFeatLabel = labels[bestFeat]
    myTree = {bestFeatLabel: {}}
    del (labels[bestFeat])
    featValues = [example[bestFeat] for example in dataSet]
    uniqueVals = set(featValues)
    for value in uniqueVals:
        subLabels = labels[:]  # 复制操作
        myTree[bestFeatLabel][value] = createTree(splitDataSet(dataSet, bestFeat, value), subLabels)
    return myTree


def majorityCnt(ft_x):
    """
        返回出现次数最多的分类名称
    """
    feat = np.unique(ft_x)
    tmp = 0
    re_ft = None
    for ft in range(len(feat)):
        freq = np.sum(np.equal(ft_x,feat[ft]))
        if freq > tmp:
            tmp = freq
            re_ft = feat[ft]
            
    return re_ft
    
def createTree(feature_x, y, chooseBestFeatureToSplitFunc=chooseBestFeatureToSplitByID3):
    """
        创建决策树
    """
    
    # 当类别完全相同则停止继续划分, 1,1,1,1,1,1,1
    if feature_x.shape == 1 and np.sum(np.unique(feature_x[0])) == 1:
        return np.unique(feature_x[0]) 
    
    # 当只有一个特征的时候，遍历完所有实例返回出现次数最多的类别
    if feature_x.shape[0] == 1:
        return majorityCnt(feature_x)
    
    bestfeat = chooseBestFeatureToSplitFunc(feature_x,y)
    print bestfeat # Lables 这边还有点问题要解决。。。。
    myTree = {bestfeat: {}}
    uniquevals = np.unique(feature_x[bestfeat])

    for value in uniquevals:
        myTree[bestfeat][value] = createTree(np.delete(feature_x, bestfeat, axis = 0), y)
    return myTree

