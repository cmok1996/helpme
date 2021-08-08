# -*- coding: utf-8 -*-
"""
Created on Mon May 31 12:18:33 2021

This script does the imputation for testdata.csv for Question 2

@author: cmok1
"""
import pickle
import joblib
import pandas as pd


#Load pipelined model (numerical transformer : median imputer + standard scaling; categorical transformer : mode imputer + one-hot encoding)
impute_model = joblib.load('impute_model.pkl')

#Load column names for processing data to be fed into model
with open('column_dict.pickle', 'rb') as handle:
    col_dict = pickle.load(handle)
    
cols = col_dict['columns']

#Load dataset
test_data = pd.read_csv('testdata.csv')
X_impute = test_data[cols]#ensure it has 255 columns to be fed inside the model as inputs (does not include totshipping.rep)
imputed_totshopping = impute_model.predict(X_impute)
test_data['totshopping.rep'] = imputed_totshopping
test_data.to_csv('imputed_testdata.csv', index=False)