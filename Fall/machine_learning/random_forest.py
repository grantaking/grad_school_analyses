# -*- coding: utf-8 -*-
"""
Created on Thu Nov  7 23:46:29 2019

@author: grant
"""

import ingest_explore as ie
import numpy as np

feature_list, train_features, train_target1, train_target2, valid_features, valid_target1, valid_target2, test_features, test_target1, test_target2 = ie.ingest()


#importing modules for modeling
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix


###creating generalizable function
def rand_forest_tune(n_estimators, max_depth, t_x, t_y, v_x, v_y):
    rf = RandomForestClassifier(n_estimators = n_estimators, random_state = 123, n_jobs = -1, max_depth = max_depth)
    rf.fit(t_x, t_y)
    prob = rf.predict_proba(v_x)[:,1]
    pred = (prob >= np.percentile(prob,85)).astype('int')
    cm = confusion_matrix(v_y,pred)
    misclass = (cm[1,0] + cm[0,1]) / sum(sum(cm))

    #calculating precision and lift
    prec = cm[1,1] / sum(cm[:,1])
    lift = prec / np.mean(v_y)
    
    return n_estimators, max_depth, misclass, prec, lift


##using function to predict target 1
model_metrics1 = []
for i in range(100,110,10):
    for j in range(30,38,2):
        estimator,depth,misclass,prec,lift = rand_forest_tune(i,j,train_features,train_target1,valid_features,valid_target1)
        model_metrics1.append([estimator,depth,misclass,prec,lift])

model_metrics1 = np.array(model_metrics1)
print(model_metrics1)


##using function to predict target 2
model_metrics2 = []
for i in range(170,180,10):
    for j in range(30,38,2):
        estimator,depth,misclass,prec,lift = rand_forest_tune(i,j,train_features,train_target2,valid_features,valid_target2)
        model_metrics2.append([estimator,depth,misclass,prec,lift])

model_metrics2 = np.array(model_metrics2)
print(model_metrics2)
print('\007')


train_features.append(valid_features)
train_target1.append(valid_target1)
train_target2.append(valid_target2)

estimator1, depth1, misclass1, prec1, lift1 = rand_forest_tune(100,28,train_features,train_target1,test_features,test_target1)

estimator2, depth2, misclass2, prec2, lift2 = rand_forest_tune(170,34,train_features,train_target2,test_features,test_target2)
