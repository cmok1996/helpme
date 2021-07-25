import pandas as pd
import numpy as np

def prepare_dataset():
    """
    load in case study file and creates randomly generated fields for region, keyword category and access type
    """
    df = pd.read_excel("case_study.xlsx", sheet_name = "KPI data", usecols = "A:E")
    df['date'] = pd.to_datetime(df['date'])
    df_new = pd.DataFrame()
    regions = ['Singapore', 'Taiwan', 'Malaysia', 'Thailand', 'Indonesia', 'Phillipines', 'Brazil', 'Mexico'] #8
    categories = ['Clothing & Accessories', 'Electronics', 'Home & Living', 'Food & Beverages', 'Hobbie & Books', 'Health & Wellness', 'Sports & Outdoors', 'Travel', 'Pet', 'Others']  #10
    accesses = ['Web', 'Mobile'] #2

    for d in range(len(df)):

        random_weights = list(np.random.dirichlet(np.ones(160),size=1)[0])
        for region in regions:
            for category in categories:
                for access in accesses:
                    weight = random_weights.pop(0)
                    date = df.loc[d, "date"]
                    total_users = np.ceil(df.loc[d, "total_users"] * weight)
                    impression = np.ceil(df.loc[d, "impression"] * weight)
                    click = np.ceil(df.loc[d, "click"] * weight)
                    temp_df = pd.DataFrame({"date" : [date], "total_users" : [total_users], "impression" : [impression], "click" : [click], "region" : [region], "categories" : [category], "access" : [access]})
                    df_new = df_new.append(temp_df, ignore_index=True)
    
    #Create CTR field
    df_new['ctr'] = df_new['click'] / df_new['impression']
    #df_new.to_csv('rgn_sample_report.csv', index=False)
    return df_new

def filter_df(df_, date = None, category = None, region = None):
    """
    Filter based on dropdown selection of Date, Category and Region
    """
    df = df_.copy()
    df = df.sort_values('date', ascending=False).reset_index(drop=True)
    if not date:
        date = df.loc[0, 'date']
    if region and category:
        df = df.loc[(df['date'] == date) & (df['categories'] == category) & (df['region'] == region)]
        #print('case 1')
    elif not region and not category:
        df = df.loc[(df['date'] == date)]        
        #print('case 2')
    elif not category:
        df = df.loc[(df['date'] == date) & (df['region'] == region)]
        #print('case 3')
    elif not region:
        df = df.loc[(df['date'] == date) & (df['categories'] == category)]
        #print('case 4')


    return df

def filter_df_month(df_, month = None, category = None, region = None):
    """
    From selected date, extract the month and filter as before
    """
    df = df_.copy()
    df = df.sort_values('date', ascending=False).reset_index(drop=True)
    if not month:
        month = df.loc[0, 'month']
    if region and category:
        df = df.loc[(df['month'] == month) & (df['categories'] == category) & (df['region'] == region)]
        #print('case 1')
    elif not region and not category:
        df = df.loc[(df['month'] == month)]        
        #print('case 2')
    elif not category:
        df = df.loc[(df['month'] == month) & (df['region'] == region)]
        #print('case 3')
    elif not region:
        df = df.loc[(df['month'] == month) & (df['categories'] == category)]
        #print('case 4')


    return df
