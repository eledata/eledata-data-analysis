#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年11月25日

@author: hmy
'''

import json

path = 'C:/Users/hmy/Documents/GitHub/eledata-data-analysis/Python-data-analysis/dataset/ch02/usagov_bitly_data2012-03-16-1331923249.txt'

# Function Area
def count(timezone):
    dict = {}
    for tz in timezone:
        if tz in dict:
            dict[tz] += 1
        else:
            dict[tz] = 1
    return dict

# Data analysis area
records = [json.loads(line) for line in open(path)]

tz = [res['tz'] for res in records if 'tz' in res]

counts = count(tz)
counts
# print ';'.join("%s = %s "%(k,v) for k, v in counts.items())
