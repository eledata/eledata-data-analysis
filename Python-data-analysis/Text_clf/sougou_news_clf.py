# -*- coding: utf-8 -*-
"""
Created on Mon May 08 15:59:02 2017

@author: HUANGMA
"""

###############################################################################
# 1.数据预处理和特征提取（文档重现）。
# 2.特征选择。
# 3.分类。
# 4.评估比较不同的分类器。
# 5.根据给定标准选择最好的分类器（标准如分类准确性、F 值、 精确率或召回率）
###############################################################################


import os
from itertools import chain
from sklearn import naive_bayes
from bs4 import BeautifulSoup
import pandas as pd
import jieba
import chardet
import codecs

path = os.getcwd()
datapath = path + '\\SogouCS\\'
stopwordpath = path + '\\Res\\stop_word.txt'

categories = ['auto',  
 'business',  
 'it',  
 'health',  
 'sports',
 'travel',
 'learning',
 'career',
 'cul',
 'mil',
 'news',
 'house',
 'yule',
 'women',
 'media',
 'gongyi'
 ];
stopword = []
stopwordfile = open(stopwordpath,'r')
stline = stopwordfile.readlines()
for tw in stline:
    tw = tw.replace('\n','')
    stopword.append(tw)

###############################################################################
# 网页分类目标：训练分类模型，对网页信息进行分类。

def fileprocess(path):
    filelist = os.listdir(path)
    filepaths = []
    newfiledata = []
    for fl in filelist:
        filepaths.append(path + fl)
    
    for filepath in filepaths:
        filedata = open(filepath)
        line = filedata.readlines()
        i = 0
        htmlraw = []
        
        # 取出每页数据
        while i <= len(line) - 6:
            htmlraw.append(line[i:i+5])
            i = i + 6
        # 对网页数据加工转换
        # 1. 网页内容类别
        # 2. 网页内容的分词信息
        # 3. 网页网址信息
        for hr in htmlraw:
            wordlist = []
            fileprocessdata = []
            soup = BeautifulSoup('\n'.join(hr)) # 对网页信息重新整形，便于信息的抽取
            # 抽取网页信息
            url = ''.join(soup.url.string)
            docno = ''.join(soup.docno.string)
            title = ''.join(soup.contenttitle.string)
            content = ''.join(soup.content.string)
            
            # 添加类别
            contentype = url.replace('http://','').split('/')[0].split('.')[0]
            # 网页内容分词进行预清理，清掉一些干扰性的词语
            for seg in jieba.cut(content):
                seg = seg.encode('utf8')
                if seg not in stopword:
                    if seg != ' ':
                        wordlist.append(seg)
            fileprocessdata.append(url)
            fileprocessdata.append(contentype)
            fileprocessdata.append(docno)
            fileprocessdata.append(title)
            fileprocessdata.append(wordlist)
            newfiledata.append(fileprocessdata)
        filedata.close()
    return newfiledata


consolidatedata = fileprocess(datapath)

# tf-idf 筛选
def tfidfprocess(rawfiledata):
    # 创建分类的映射表，利用字典结构来实现
    dict = {}
    for cate in categories:
        dict[cate] = []
    
    for rfd in rawfiledata:
        if rfd[1] in categories:
            dict[rfd[1]].append(rfd[4])
    
    # 合并链表
    for cate in categories:
        dict[cate] = list(chain(*dict[cate]))
    return dict



result = tfidfprocess(consolidatedata)











