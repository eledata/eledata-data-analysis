#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年12月21日

@author: HUANGMA
'''

import numpy as np

class MultinomialNB(object):
    
    '''
    step 1: P(X=xi|Y=Ck), P(Y=Ck)
    step 2: f(x)=argmax[P(Y=Ck)IIP(X=xi|Y=Ck)
    step 3: Predict x -> y...
    '''
    
    def __init__(self, alpha = 1, fit_prior = True, class_prior = None):
        self.alpha = alpha
        self.fit_prior = fit_prior
        self.class_prior = class_prior
        self.classes = None
        self.conditional_prob = None
    
    # Calc feature prob, P(X=xi|Y=Ck)    
    def _calc_feature_prob(self, feature):
        values = np.unique(feature)
        tot_num = len(feature)
        value_prob = {}
        for value in values:
            value_prob[value] = (np.sum(np.equal(feature, value)))/(tot_num + len(values)*self.alpha)
        return value_prob
    
    # Sample X --> y
    def fit(self, X, y):
        self.classes = np.unique(y)
        
        # calc class prior prob
        if self.class_prior is None:
            class_num = len(self.classes)
            if not self.fit_prior:
                self.class_prior = [1.0/class_num for _ in range(class_num)]
            else:
                self.class_prior = []
                y_num = float(len(y)) # Y length
                for cls in self.classes:
                    cls_num = np.sum(np.equal(y,cls)) # Y=Ck 
                    self.class_prior.append((cls_num + self.alpha*class_num)/(y_num + self.alpha*class_num))
        
        # calc conditoin prob
        self.conditional_prob = {}
        for cls in self.classes:
            self.conditional_prob[cls] = {}
            for i in range(len(X[0])):
                feature = X[np.equal(cls, y)][:,i] # Split out the suitable feature data to load in calc function.
                self.conditional_prob[cls][i] = self._calc_feature_prob(feature)
                
        return self
    
    def _get_xj_prob(self, values_prob, target_value):
        return values_prob[target_value]
    
    # Predict a single value, x11, x21 --> Y1, f(x)=argmax[P(Y=Ck)IIP(X=xi|Y=Ck)
    def _predict_single_sv(self, x):
        label = -1
        argmax = 0
        
        for cls_index in range(len(self.classes)):
            cur_class_prior = self.class_prior[cls_index]
            cur_conditional_prob = 1.0
            feature_prob = self.conditional_prob[self.classes[cls_index]]
            
            j = 0
            # IIP(X=xi|Y=Ck) Dict struct operation
            for key in feature_prob.keys():
                print feature_prob[key]
                cur_conditional_prob *= self._get_xj_prob(feature_prob[key], x[j])
                j += 1
            
            # argmax[P(Y=Ck)IIP(X=xi|Y=Ck)
            if cur_conditional_prob*cur_class_prior > argmax:
                argmax = cur_conditional_prob*cur_class_prior
                label = self.classes[cls_index]
            
            return label

    # predict 
    def predict(self, X):
        
        if X.ndim == 1:
            return self._predict_single_sv(X)
        else:
            labels = []
            for i in range(X.shape[0]):
                label = self._predict_single_sv(X[i])
                labels.append(label)
        return labels
    

class GaussianNB(MultinomialNB):
    
    # Calc feature prob, Gaussian
    def _calc_feature_prob(self, feature):
        mu = np.mean(feature)
        sigma = np.std(feature)
        return (mu, sigma)

    def _prob_gaussian(self,mu,sigma,x):
        return (1.0/(sigma*np.sqrt(2*np.pi))*np.exp(-(x - mu)**2/(2*sigma**2)))
    
    def _get_xj_prob(self, mu_sigma, target_value):
        return self._prob_gaussian(mu_sigma[0],mu_sigma[1],target_value)


# Test
X = np.array([
                      [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3],
                      [4,5,5,4,4,4,5,5,6,6,6,5,5,6,6]
             ])
X = X.T
y = np.array([-1,-1,1,1,-1,-1,-1,1,1,1,1,1,1,1,-1])

nb = MultinomialNB(alpha=1.0,fit_prior=True)
ngb = GaussianNB(alpha=1.0,fit_prior=True)
nb.fit(X,y)
print nb.predict(np.array([2,4]))
ngb.fit(X,y)
print ngb.predict(np.array([2,4]))
