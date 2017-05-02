#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2017年4月16日

@author: hmy
'''

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sbn





# Data Explore 
# 读取训练文件csv
df_train = pd.read_csv('C:/Users/hmy/Documents/GitHub/eledata-data-analysis/Python-data-analysis/kaggle-projects/House Price/train.csv')
# 数据各列名称
print '训练数据各列名称: '
print df_train.columns

# 房价的数据描述
print '房价的数据描述:'
print df_train['SalePrice'].describe()

# 房价数据分布情况
print '偏度系数: %f'%df_train['SalePrice'].skew()
print '峰度系数: %f'%df_train['SalePrice'].kurt()
sbn.distplot(df_train['SalePrice'])

# 相关矩阵和协方差矩阵
# 1, 算相关矩阵
corrmat = df_train.corr()
#corrmat.to_csv('C:/Users/hmy/Documents/GitHub/eledata-data-analysis/Python-data-analysis/kaggle-projects/House Price/corrmat.csv')
print corrmat['SalePrice']



# 2, 设置画布,画图
f, ax = plt.subplots(figsize=(12, 9))
sbn.heatmap(corrmat, vmax=.8, square=True)
# f.tight_layout()



# 与房价有明显相关关系的列，进行画图比较
# 1. GarageCars&GarageArea
data_gcp = pd.concat([df_train['GarageCars'],df_train['SalePrice']], axis = 1)
f, ax = plt.subplots(figsize=(12, 9))
sbn.boxplot(x="GarageCars", y="SalePrice", data=data_gcp, palette="PRGn")
sbn.despine(offset=10, trim=True)

data_gap = pd.concat([df_train['GarageArea'],df_train['SalePrice']], axis = 1)
f, ax = plt.subplots(figsize=(12, 9))
sbn.boxplot(x="GarageArea", y="SalePrice", data=data_gap, palette="PRGn")
sbn.despine(offset=10, trim=True)

# 2. OverallQual
data_oqp = pd.concat([df_train['OverallQual'],df_train['SalePrice']], axis = 1)
f, ax = plt.subplots(figsize=(12, 9))
sbn.boxplot(x="OverallQual", y="SalePrice", data=data_oqp, palette="PRGn")
sbn.despine(offset=10, trim=True)

# 3. YearBuilt
data_ybp = pd.concat([df_train['YearBuilt'],df_train['SalePrice']], axis = 1)
f, ax = plt.subplots(figsize=(12, 9))
sbn.boxplot(x="YearBuilt", y="SalePrice", data=data_ybp, palette="PRGn")
sbn.despine(offset=10, trim=True)

# 4. YearRemodAdd
data_yrp = pd.concat([df_train['YearRemodAdd'],df_train['SalePrice']], axis = 1)
f, ax = plt.subplots(figsize=(12, 9))
sbn.boxplot(x="YearRemodAdd", y="SalePrice", data=data_yrp, palette="PRGn")
sbn.despine(offset=10, trim=True)

# 5. TotalBsmtSF
data_tbp = pd.concat([df_train['TotalBsmtSF'],df_train['SalePrice']], axis = 1)
data_tbp.plot.scatter(x="TotalBsmtSF", y="SalePrice", ylim=(0,800000));


plt.show()











