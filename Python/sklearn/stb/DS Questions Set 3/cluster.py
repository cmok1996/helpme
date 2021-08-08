# -*- coding: utf-8 -*-
"""
Created on Sun May 30 17:47:38 2021

This script helps to answer Question 1. 
Building model block takes approx 20mins to run. Advised to skip this block when running the codes

@author: cmok1
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
#from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.compose import make_column_transformer
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.decomposition import PCA
#from sklearn.cluster import KMeans, MiniBatchKMeans
from kmodes.kprototypes import KPrototypes

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

#Impute strategy - Fill null with mode for categorical, median for numerical
for i in categorical:
    spend_df[i] = spend_df[i].fillna(value = spend_df[i].mode().item())
for j in numerical:
    spend_df[j].fillna(value=spend_df[j].median(), inplace=True)

#Ensure there are no missing values

if len(spend_df[spend_df.isna().any(axis=1)]) == 0:
    print("Identified categorical features are imputed with the mode while numerical features are imputed with median")
else:
    print("Incomplete imputation")
    
#Change categorical variables to category type
for i in categorical:
    spend_df[i] = spend_df[i].astype("category")

#Feature engineering -> Scale numerical variables + One-hot encoding for categorical
scaler = StandardScaler()
numeric_transformer = make_pipeline(scaler)
#one_hot = OneHotEncoder()
#categorical_transformer = make_pipeline(one_hot)
col_transformer = make_column_transformer(
    (numeric_transformer, numerical),
    #(categorical_transformer, categorical),
    remainder='passthrough',
)

X = col_transformer.fit_transform(spend_df)

# =========Building model==========================================


#Plot elbow plot to find number of clusters using euclidean distance for numeric, takes about 20minutes to run
#Uncomment if want to run

# ========================Uncomment block to run grid search==========================
# K = range(1,30)
# wss = []
# #To speed up computation, sample 500 observations
# #Sample 500 from length of X
# random_index = np.random.choice(len(X), 500, replace=False)
# X_fit = X[random_index]
# for k in K:
#     print(k)
#     
#     k_proto = KPrototypes(n_clusters = k, verbose = 2, max_iter = 20, random_state = 42)
#     clusters = k_proto.fit(X_fit, categorical = categorical_index)
#     wss.append(clusters.cost_)
#     
# fig = plt.figure()
# plt.plot(K, np.array(wss))
# plt.xlabel('Number of clusters')
# plt.ylabel('Inertia')
# plt.title('Scree plot')
# plt.show()
# =============================================================================

# Fitting best kmeans model, takes about 15mins to run
k_proto = KPrototypes(n_clusters = 5, verbose = 2, max_iter = 20, random_state = 42)
clusters = k_proto.fit_predict(X, categorical = categorical_index)

spend_df['labels']= np.array(clusters)
grouped_df = spend_df.groupby('labels').mean() #Inspect dataframe to see average value for cluster

#Visualizing clusters
pca = PCA(n_components=2)

scaler = StandardScaler()
numeric_transformer = make_pipeline(scaler)
one_hot = OneHotEncoder()
categorical_transformer = make_pipeline(one_hot)
col_transformer = make_column_transformer(
    (numeric_transformer, numerical),
    (categorical_transformer, categorical),
    remainder='passthrough',
)

X_ = col_transformer.fit_transform(spend_df)

X_2d = pca.fit_transform(X_)
print(pca.explained_variance_ratio_) #2 principle components only account for 11% of total variation

df_plot = pd.DataFrame({"x" : X_2d[:,0 ], "y" : X_2d[:, 1], "label" :  np.array(clusters)})
sns.scatterplot(data=df_plot, x="x", y="y", hue="label")






