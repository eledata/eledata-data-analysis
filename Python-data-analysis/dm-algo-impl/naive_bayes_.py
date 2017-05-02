#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月20日

@author: hmy
'''

import numpy as np

class MultinomialNB(object):
    
    def __init__(self, alpha = 1.0, fit_prior = True, class_prior = None):
        self.alpha = alpha
        self.fit_prior = fit_prior
        self.class_prior = class_prior
        self.classes = None
        self.conditional_prob = None
        
    def _calc_feature_prob(self, feature):
        values = np.unique(feature)
        
    