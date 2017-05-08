#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月8日

@author: hmy
'''

import os
from bs4 import BeautifulSoup as bfs
import jieba
import os
import sys
from sklearn import feature_extraction
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.feature_extraction.text import CountVectorizer

path = 'D:/Sogou WebLog/SogouCA/'

def consolidate_file(path):
    link = []
    file_list = os.listdir(path)
    
    for fl in file_list:
        link.append(path + fl) 
    
    new_file_name = 'SogouCA'
    consolidate_file = open(path+new_file_name, 'w')
    
    for link_item in link:
        tmp = open(link_item,'r')
        for eachline in tmp:
            str_convert = ''.join(eachline)
            str_convert.replace(' ', '    ')
            consolidate_file.write(str_convert)
                
    tmp.close()
    consolidate_file.close()
    
def split_file_part(path, file_name):
    file = open(path+file_name, 'r')
    start = '<doc>'
    end = '</doc>\n'
    content = []
    tmp = ""
    for eachline in file:
        tmp = tmp + (''.join(eachline))
        if eachline == end:
            content.append(tmp)
            tmp = ""
    return content

# split_file_part(path,"test.txt")

def bfs_process(path, file_name):
    split_file = split_file_part(path, file_name)
    content = []

    for part in split_file:
        soup = bfs(part)
        cnt = ''.join(soup.content.string)
        content.append(cnt)
    
    return content

def sk_jieba_process(content):
    corpus = []
    
    for cnt in content:
        seg_list = jieba.cut(cnt) 
        corpus.append("  ".join(seg_list))
    
    vectorizer = CountVectorizer()
    transformer = TfidfTransformer()
    tfidf = transformer.fit_transform(vectorizer.fit_transform(corpus)) #第一个fit_transform是计算tf-idf，第二个fit_transform是将文本转为词频矩阵
    word = vectorizer.get_feature_names()#获取词袋模型中的所有词语
    weight = tfidf.toarray()#将tf-idf矩阵抽取出来，元素a[i][j]表示j词在i类文本中的tf-idf权重
    for i in range(len(weight)):#打印每类文本的tf-idf词语权重，第一个for遍历所有文本，第二个for便利某一类文本下的词语权重
        print u"-------这里输出第",i,u"类文本的词语tf-idf权重------"
        for j in range(len(word)):
            print word[j],weight[i][j]    
    

# bfs_process(path,"test.txt")
# bfs_process(path,"test.txt")
sk_jieba_process(bfs_process(path,"test.txt"))  
    
# soup = bfs(split_file_part(path,"test.txt")[0])
# print soup
# print soup.docno.string
# print soup.url.string
# print soup.contenttitle.string
# print soup.content.string




