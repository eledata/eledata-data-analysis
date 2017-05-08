# -*- coding: utf-8 -*-
"""
Created on Mon May 08 15:59:02 2017

@author: HUANGMA
"""

import os
import sys
from sklearn import naive_bayes
from bs4 import BeautifulSoup
import pandas as pd
import jieba
import chardet

path = os.getcwd()
datapath = path + '\\SogouCS\\'

reload(sys)
sys.setdefaultencoding('utf8')

link = []
webdetails = []
filelist = os.listdir(datapath)


for fl in filelist:
    link.append(datapath + fl) 

for link_item in link:
    tmp = open(link_item)
    line = tmp.read()
    i = 0
    while i <= len(line) - 6:
        webdetails.append(line[i:i+5])
        i = i + 6
    print line
        


        
        
tmp.close()

