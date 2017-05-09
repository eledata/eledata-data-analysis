# -*- coding: utf-8 -*-
"""
Created on Mon May 08 15:59:02 2017

@author: HUANGMA
"""

import os
from sklearn import naive_bayes
from bs4 import BeautifulSoup
import pandas as pd
import jieba
import jieba.analyse
import chardet

path = os.getcwd()
datapath = path + '\\SogouCS\\'

link = []
webdetails = []
filelist = os.listdir(datapath)


for fl in filelist:
    link.append(datapath + fl) 

df = pd.DataFrame(columns=['url', 
                           'docno', 
                           'title', 
                           'content_key_01',
                           'content_key_02',
                           'content_key_03',
                           'content_key_04',
                           'content_key_05',
                           'content_key_06',
                           'content_key_07',
                           'content_key_08',
                           'content_key_09',
                           'content_key_10',
                           'content_key_11',
                           'content_key_12',
                           'content_key_13',
                           'content_key_14',
                           'content_key_15',
                           'content_key_16',
                           'content_key_17',
                           'content_key_18',
                           'content_key_19',
                           'content_key_20'
                           ])
for link_item in link:
    tmp = open(link_item)
    line = tmp.readlines()
    i = 0

    while i <= len(line) - 6:
        webdetails.append(line[i:i+5])
        i = i + 6

    for wd in webdetails:
        soup = BeautifulSoup('\n'.join(wd))
        print soup.prettify()
        url = ''.join(soup.url.string)
        docno = ''.join(soup.docno.string)
        title = ''.join(soup.contenttitle.string)
        content = ''.join(soup.content.string)
        
#        print url
#        print docno
#        print title
#        print content

        tags = jieba.analyse.extract_tags(content)
        if len(tags) <= 20:
            tmptags = ['' for n in range(20)]
            for i in xrange(len(tags)):
                tmptags[i] = tags[i]
            tags = tmptags
#        print '|'.join(tags)
#        print len(tags[19])
        convertresult = []
        convertresult.append(url)
        convertresult.append(docno)
        convertresult.append(title)
        for i in xrange(len(tags)):
            convertresult.append(tags[i])
        tmpdf = pd.DataFrame(convertresult)
        df.append(tmpdf.T)
        print tmpdf.T
    print df
    tmp.close()

df.to_csv(datapath + 'result.csv')
