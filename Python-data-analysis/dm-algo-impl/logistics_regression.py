#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月26日

@author: HUANGMA
'''

import matplotlib.pyplot as plt
import numpy as np

filePath = 'C:/Users/hmy/Documents/GitHub/sk-learn/dm-algo-impl/dataset/testSet.txt'


def loadDataSet():
    dataMat = []
    labelMat = []
    fo = open(filePath)
    
    for line in fo.readlines():
        lineArr = line.strip().split()
        dataMat.append([1.0, float(lineArr[0]), float(lineArr[1])]) #X0设为1.0，构成拓充后的输入向量
        labelMat.append(int(lineArr[2]))
    
    return dataMat, labelMat
        
def plotBestFit(weights):
    """
        画出数据集和逻辑斯谛最佳回归直线
    :param weights:
    """
    # load the data on the plot.
    dataMat,labelMat = loadDataSet()
    dataArr = np.array(dataMat)
    n = np.shape(dataArr)[0]
    xcord1 = []; ycord1 = []
    xcord2 = []; ycord2 = []
    
    # Print the x1 and x2 on the plot by using label.
    for i in range(n):
        if int(labelMat[i]) == 1:
            xcord1.append(dataArr[i,1]); ycord1.append(dataArr[i,2])
        else:
            xcord2.append(dataArr[i,1]); ycord2.append(dataArr[i,2])
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.scatter(xcord1, ycord1, s=30, c='red', marker='s')
    ax.scatter(xcord2, ycord2, s=30, c='green')
    
    if weights is not None:
        
        x = np.arange(-3.0, 3.0, 0.1)
        y = (-weights[0]- weights[1]*x)/weights[2]   #令w0*x0 + w1*x1 + w2*x2 = 0，其中x0=1，解出x1和x2的关系, z = w0*x0 + w1*x1 + w2*x2.......
        ax.plot(x, y)                        #一个作为X一个作为Y，画出直线

    plt.xlabel('X1'); plt.ylabel('X2');
    plt.show()


def sigmod(x):
    return 1/(1 + np.exp(-x))
    
# 梯度上升法
def gradAscent(data, label):
    data, label = loadDataSet()
    dataMat = np.mat(data)
    labelMat = np.mat(label).T
    
    m, n = np.shape(dataMat)
    weight = np.ones((n, 1)) # (1,1,1)T
    cycle = 500
    alpha = 0.001
    
    for k in range(cycle):
        h = sigmod(dataMat * weight) # 100*1, dataMat:100*3, weight: 3*1
        error = labelMat - h # 100*1
        weight = weight + alpha * dataMat.T * error # Mat transfer..
    
    return np.array(weight)

# 随机梯度上升法, 每次仅用一个样本点的误差值来更新weight
def randomGradAscent(data, label):
    data, label = loadDataSet()
    dataMat = np.mat(data)
    labelMat = np.mat(label).T
    
    m, n = np.shape(dataMat)
    weight = np.ones((n, 1)) # (1,1,1)T
    alpha = 0.001
    
    for i in range(m):
        h = sigmod(np.sum(dataMat[i] * weight)) # value
        error = labelMat[i] - h
        weight = weight + alpha * dataMat[i].T * error
    
    return np.array(weight)


# 改进随机梯度上升法, 每次仅用一个样本点的误差值来更新weight， 改进random Index和Alpha的值
def advanceRandomGradAscent(data, label, numItem = 150):
    data, label = loadDataSet()
    dataMat = np.mat(data)
    labelMat = np.mat(label).T
    
    m, n = np.shape(dataMat)
    weight = np.ones((n, 1)) # (1,1,1)T
    alpha = 0.001
    
    for i in range(numItem):
        dataIndex = range(m)
        for j in range(m):
            alpha = (4/(i + j + 1.0)) + 0.01 # Change Alpha every time.
            random_index = int(np.random.uniform(0, len(dataIndex))) # use real random
            h = sigmod(np.sum(dataMat[random_index] * weight)) # value
            error = labelMat[random_index] - h
            weight = weight + alpha * dataMat[random_index].T * error
            del dataIndex[random_index]
            
    return np.array(weight)

data, label = loadDataSet()
weight = advanceRandomGradAscent(data, label)
plotBestFit(weight)