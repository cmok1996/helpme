# -*- coding: utf-8 -*-
"""
Created on Sun May 30 20:11:47 2021

This script helps to build the imputation model for Question 2.
Since grid search is being performed to train and validate the models, the script might take a long time to run especially for Random Forest

@author: cmok1
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.compose import make_column_transformer
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.impute import SimpleImputer
import statsmodels.api as sm
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import cross_val_score
from sklearn.metrics import mean_squared_error
import joblib
import pickle

#Load data
spend_df = pd.read_csv('spenddata.csv')

#Standard checks
spend_df.describe()
spend_df.info()
spend_df.drop('Unnamed: 0', axis=1, inplace=True) #Drop index column

#Identify proportion of missing variables
missing_proportion = spend_df.isna().sum(axis=0) / len(spend_df)
plt.hist(missing_proportion)

#Drop columns with more than 50% missing value
num_columns = (missing_proportion > 0.5).sum()
print(f"There are {num_columns} columns with more than 50% missing value")
missing_columns = missing_proportion.loc[missing_proportion > 0.5].index
spend_df.drop(missing_columns, axis=1, inplace=True)

#Identify categorical and numerical variables
##Columns with less than 5 unique values are considered categorical
categorical = [x for x in spend_df.columns if len(spend_df[x].unique()) < 5]
categorical_index = [i for i,x in enumerate(spend_df.columns) if len(spend_df[x].unique()) < 5]
numerical = [x for x in spend_df.columns if len(spend_df[x].unique()) >= 5]
numerical.remove('totshopping.rep') #0 missing value for totshopping.rep anyway
    
#Change categorical variables to category type
for i in categorical:
    spend_df[i] = spend_df[i].astype("category")
    
#Train test split
X = spend_df.drop(['totshopping.rep'], axis=1) 
y = spend_df['totshopping.rep'].values
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

# =====================Feature Engineering using pipelines ==============================

#Feature engineering -> Scale numerical variables + One-hot encoding for categorical
numeric_imputer = SimpleImputer(strategy='median')
cat_imputer = SimpleImputer(strategy='most_frequent')
scaler = StandardScaler()
one_hot = OneHotEncoder(handle_unknown = 'ignore')

numeric_transformer = make_pipeline((numeric_imputer),(scaler))
categorical_transformer = make_pipeline( (cat_imputer),(one_hot))
col_transformer = make_column_transformer(
    (numeric_transformer, numerical),
    (categorical_transformer, categorical), 
    remainder='passthrough',
)

X_tr = col_transformer.fit_transform(X_train)
X_val = col_transformer.transform(X_test)

#OLS summary table
X_sm = sm.add_constant(X_tr)
model = sm.OLS(y_train,X_sm)
print("OLS Model output for exploratory purposes")
model.fit().summary() #the one-hot encoded categorical variables are significant

# ====================Model Building==================================================
#OLS
lr = LinearRegression()
pipe_ols = make_pipeline(col_transformer, lr)
pipe_ols.fit(X_train, y_train)
#pipe_ols.score(X_test, y_test)
ols_rmse = np.mean(cross_val_score(pipe_ols,X_train,y_train, scoring = 'neg_root_mean_squared_error', cv= 3))
print(f"CV RMSE for OLS : {-ols_rmse}") #RMSE = 1809.343

#Ridge regression
ridge = Ridge()
pipe_ridge = make_pipeline(col_transformer, ridge)
params = {'ridge__alpha' : np.array([0.1, 0.5, 1.0, 5.0]), 'ridge__random_state' : [42]}
grid_search_ridge = GridSearchCV(pipe_ridge, param_grid=params, cv=3, scoring= 'neg_root_mean_squared_error', refit=True, verbose=2)
grid_search_ridge.fit(X_train, y_train)
print("Ridge Regression statistics")
print(f"Best root mean squared error is {-grid_search_ridge.best_score_}") #emse = 0.09507
print(f"Best alpha : {grid_search_ridge.best_params_['ridge__alpha']}") #alpha = 0.1

#Lasso regression
lasso = Lasso()
pipe_lasso = make_pipeline(col_transformer, lasso)
params = {'lasso__alpha' : np.array([0.1, 0.5, 1.0, 5.0]), 'lasso__random_state' : [42]}
grid_search_lasso = GridSearchCV(pipe_lasso, param_grid=params, cv=3, scoring= 'neg_root_mean_squared_error', refit=True, verbose=2)
grid_search_lasso.fit(X_train, y_train)
print("Lasso Regression statistics")
print(f"Best root mean squared error is {-grid_search_lasso.best_score_}") #rmse = 0.1829
print(f"Best alpha : {grid_search_lasso.best_params_['lasso__alpha']}") #alpha = 0.1

#Elastic Net regression
en = ElasticNet()
pipe_en = make_pipeline(col_transformer, en)
params = {'elasticnet__alpha' : np.array([0.1,0.5, 1.0, 5.0]), 'elasticnet__random_state' : [42], 'elasticnet__l1_ratio' : [0.1, 0.5, 0.8] }
grid_search_en = GridSearchCV(pipe_en, param_grid=params, cv=3, scoring= 'neg_root_mean_squared_error', refit=True, verbose=2)
grid_search_en.fit(X_train, y_train)
print("ElasticNet Regression statistics")
print(f"Best root mean squared error is {-grid_search_en.best_score_}") #rmse = 35.3
print(f"Best alpha : {grid_search_en.best_params_['elasticnet__alpha']}") #alpha = 0.1
print(f"Best l1 ratio : {grid_search_en.best_params_['elasticnet__l1_ratio']}") #l1 = 0.8

#RandomForest Regression
rf = RandomForestRegressor()
pipe_rf = make_pipeline(col_transformer, rf)
params = {'randomforestregressor__max_depth' : np.array([5, 10,50,100]), 'randomforestregressor__max_features' : ['auto', 'sqrt', 'log2'], 'randomforestregressor__n_estimators' : [50,100,200], 'randomforestregressor__random_state' : [42]}

grid_search_rf = GridSearchCV(pipe_rf, param_grid=params, cv=3, scoring= 'neg_root_mean_squared_error', refit=True, verbose=2)
#Takes about 30minutes to run
grid_search_rf.fit(X_train, y_train)
print("Random Forest Regression statistics")
print(f"Best negative root mean squared error is {-grid_search_rf.best_score_}") #rmse = 15.9
print("best parameters")
grid_search_rf.best_params_

#Final fit and comptue test score using best Ridge model
pred = grid_search_ridge.predict(X_test)
rmse_test = np.sqrt(mean_squared_error(pred,y_test))
print(f"RMSE on test set using Ridge Model : {rmse_test}")

#Save model
joblib.dump(grid_search_ridge, 'impute_model.pkl')

#Save columns for faster selection of columns to be fed into the model
cols = X_test.columns

column_dicts = {"columns" : cols,
                "numerical_columns" : numerical,
                "categorical_columns" : categorical}

with open('column_dict.pickle', 'wb') as handle:
    pickle.dump(column_dicts, handle, protocol=pickle.HIGHEST_PROTOCOL)
    
        
