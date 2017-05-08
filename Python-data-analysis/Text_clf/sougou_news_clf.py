# -*- coding: utf-8 -*-
"""
Created on Mon May 08 15:59:02 2017

@author: HUANGMA
"""

import jieba

seg_list = jieba.cut("我来到北京清华大学", cut_all=True)
print("Full Mode: " + "/ ".join(seg_list))  # 全模式