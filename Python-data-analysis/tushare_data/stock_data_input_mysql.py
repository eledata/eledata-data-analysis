#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on Nov 23, 2016

@author: hmy
'''

import tushare as ts

stock_all = ts.get_today_all()
print stock_all[0]