#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月8日

@author: hmy
'''

import os

path = 'D:/Sogou WebLog/SogouQ/'
link = []
file_list = os.listdir(path)

for fl in file_list:
    link.append(path + fl) 

new_file_name = 'sougou_q'
consolidate_file = open(path+new_file_name, 'w')

for link_item in link:
    tmp = open(link_item,'r')
    for eachline in tmp:
        str_convert = ''.join(eachline)
        str_convert.replace(' ', '    ')
        consolidate_file.write(str_convert)
            
tmp.close()
consolidate_file.close()
