# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 14:30:24 2019

@author: grant
"""

import pandas as pd
import numpy as np


def ingest():
    train1 = pd.read_csv('MLProject_train.csv')
    test = pd.read_csv('MLProject_valid.csv')
    
    #Missing observations in the training dataset will be replaced by the median. Missing observations in the test dataset will be dropped
    train1 = train1.dropna(axis=0)
    train1['missing_flag'] = train1['L6'].isna().astype(int)
    train1 = train1.fillna(train1.median())
    test['missing_flag'] = 0
    
    ###splitting categorical variable into dummies
    for df in [train1,test]:
        df['Z2'] = df['Z2'].astype(str)
        df = pd.get_dummies(df)
        
    ###splitting training dataset into training and validation
    np.random.seed(0)
    rand = np.random.rand(len(train1)) < .7
    train = train1[rand]
    valid = train1[~rand]
    
    
    ###splitting training, validation, and test datasts into features and targets
    train_features = train.drop(['target1','target2'],axis=1)
    valid_features = valid.drop(['target1','target2'],axis=1)
    test_features = test.drop(['target1','target2'],axis=1)
    feature_list = list(train_features.columns)
    
    #train_features = np.array(train_features)
    #valid_features = np.array(valid_features)
    
    train_target1 = train['target1']
    valid_target1 = valid['target1']
    test_target1 = test['target1']
    
    train_target2 = train['target2']
    valid_target2 = valid['target2']
    test_target2 = test['target2']
    
    return feature_list, train_features, train_target1, train_target2, valid_features, valid_target1, valid_target2, test_features, test_target1, test_target2


