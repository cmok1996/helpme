# -*- coding: utf-8 -*-
"""
Created on Sat Apr 17 16:47:53 2021

@author: cmok1
"""
import pandas as pd
import numpy as np
from utils import prepare_dataset, filter_df, filter_df_month

import chart_studio.plotly as py
import cufflinks as cf
import seaborn as sns
import plotly.express as px
import plotly.graph_objects as go
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import dash_bootstrap_components as dbc
from plotly.subplots import make_subplots

from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot
init_notebook_mode(connected=True)
cf.go_offline()

app = dash.Dash(__name__, external_stylesheets = [dbc.themes.SOLAR])

#Load data

#df_new = prepare_dataset()
df_new = pd.read_csv("rgn_sample_report.csv")
df_new['date'] = pd.to_datetime(df_new['date'])

#Feature extraction

df_new['dow'] = df_new['date'].dt.dayofweek
dayofweek_map = {0:'Monday', 1:'Tuesday', 2:'Wednesday', 3:'Thursday', 4:'Friday', 5:'Saturday', 6:'Sunday'}
df_new['dow'] = df_new['dow'].map(dayofweek_map)
df_new['week'] = df_new['date'].apply(lambda x : str(x.year) + '-' + str(x.week))
df_new['month'] = df_new['date'].apply(lambda x : str(x.year) + '-' + str(x.month))
df_new['date'] = df_new['date'].dt.date

#Get latest date
latest_date = df_new.sort_values('date', ascending=False).reset_index(drop=True).loc[0,'date']
latest_week = df_new.sort_values('date', ascending=False).reset_index(drop=True).loc[0, 'week']
latest_month = df_new.sort_values('date', ascending=False).reset_index(drop=True).loc[0, 'month']

#App layout
app.layout = dbc.Container([
    dbc.Row([
        dbc.Col(html.H1("Search Investigation Dashboard", className = "text-center text-white"),
        width = 12)
    ]), 

    dbc.Row([
        dbc.Col([
            dcc.Dropdown(id = 'slct_date', value=str(latest_date), multi=False,
            options = [{'label' : x, 'value' : x} for x in sorted(df_new['date'].unique())], placeholder = 'Select date'),
            #dcc.Graph(id = 'line_graph', figure = {})
        ], width = 4),
        dbc.Col([
            dcc.Dropdown(id = 'slct_region', value=None, multi=False,
            options = [{'label' : x, 'value' : x} for x in sorted(df_new['region'].unique())], placeholder = 'Select Region'),
        ], width = 4),
        dbc.Col([
            dcc.Dropdown(id = 'slct_product', value=None, multi=False,
            options = [{'label' : x, 'value' : x} for x in sorted(df_new['categories'].unique())], placeholder = 'Select Product Category'),
        ], width = 4)
    ]),

    html.Br(),

    dbc.Row([

        dbc.Col([
            html.H6(children = 'Metrics for selected date', className = "text-center text-white",
            style = {'fontSize' : 22}),
            dcc.Graph(id = 'num_users', config = {'displayModeBar':False}, style = {'margin-top' : '12px'}),
            dcc.Graph(id = 'num_impression', config = {'displayModeBar':False}, style = {'margin-top' : '12px'}),
            dcc.Graph(id = 'num_clicks', config = {'displayModeBar':False}, style = {'margin-top' : '12px'}),
            dcc.Graph(id = 'CTR', config = {'displayModeBar':False}, style = {'margin-top' : '12px'}),
            dcc.Graph(id = 'adj_CTR', config = {'displayModeBar':False}, style = {'margin-top' : '12px'}),

        ], width = 4),

        dbc.Col([
            html.H6(children = 'Per-user statistics', className = "text-center text-white",
            style = {'fontSize' : 22}),
            dcc.Graph(id = 'user_stats', figure={}, style = {'margin-top' : '20px', 'pad' : '0px'}),

        ], width = 8)
  
    ]), 

    
    html.Br(),

    dbc.Row([

        dbc.Col([
            html.H6(children = 'Top category clicks by region',
            className = "text-center text-white",
            style = {'fontSize' : 22}),
            dcc.Graph(id = 'regional_searches', style = {'margin-top' : '20px'}, figure = {}),
        ], width = 8),

        dbc.Col([
            html.H6(children = 'Users by access type',
            className = "text-center text-white",
            style = {'fontSize' : 22}),
            dcc.Graph(id = 'access', style = {'margin-top' : '20px'}, figure = {}),
        ], width = 4)
    ]),
    html.Br(),

    dbc.Row([

        dbc.Col([
            html.H6(children = 'Overall KPI plot', className = "text-center text-white",
            style = {'fontSize' : 22}),
            dcc.Graph(id = 'overall_kpi', style = {'margin-top' : '20px'}, figure = {}),
        ])
    ])


])

#Callbacks to allow for interactivity based on dropdown selection
@app.callback(
    Output('num_users', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_users(date, region, category):
    date = pd.Timestamp(date)
    df = filter_df(df_new, date, category, region)
    users = int(df['total_users'].sum())
    #print(users, date, region, category)
    #Plotly graph
    dic = {
        'data'  : [go.Indicator(mode = "number",value = users, number = {'font' : {'size' : 28}}, domain = {'y' : [0,1]},  gauge = {
        'axis': {'visible': False}})],
        'layout' : go.Layout(title = {'text' : '<b>Users<b>', 'y': 1, 'x' : 0.5, 'xanchor' : 'center', 'yanchor' : 'top' }, font=dict(color='black', size=10), height=60, paper_bgcolor = '#DADDD8')
    }
    
    return dic

@app.callback(
    Output('num_impression', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_impression(date, region, category):
    date = pd.Timestamp(date)
    df = filter_df(df_new, date, category, region)
    impression = int(df['impression'].sum())
    #print(users, date, region, category)
    #Plotly graph
    dic = {
        'data'  : [go.Indicator(mode = "number",value = impression, number = {'font' : {'size' : 28}}, domain = {'y' : [0,1]},  gauge = {
        'axis': {'visible': False}})],
        'layout' : go.Layout(title = {'text' : '<b>Impressions<b>', 'y': 1, 'x' : 0.5, 'xanchor' : 'center', 'yanchor' : 'top' }, font=dict(color='black', size=10), height=60, paper_bgcolor = '#DADDD8')
    }
    
    return dic

@app.callback(
    Output('num_clicks', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_clicks(date, region, category):
    date = pd.Timestamp(date)
    df = filter_df(df_new, date, category, region)
    clicks = int(df['click'].sum())
    #print(users, date, region, category)
    #Plotly graph
    dic = {
        'data'  : [go.Indicator(mode = "number",value = clicks, number = {'font' : {'size' : 28}}, domain = {'y' : [0,1]},  gauge = {
        'axis': {'visible': False}})],
        'layout' : go.Layout(title = {'text' : '<b>Clicks<b>', 'y': 1, 'x' : 0.5, 'xanchor' : 'center', 'yanchor' : 'top' }, font=dict(color='black', size=10), height=60, paper_bgcolor = '#DADDD8')
    }
    
    return dic

@app.callback(
    Output('CTR', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_ctr(date, region, category):
    date = pd.Timestamp(date)
    df = filter_df(df_new, date, category, region)
    df = df.groupby('date')['click', 'impression'].sum().reset_index()
    df['ctr'] = df['click'] / df['impression']
    ctr = round(df['ctr'].mean(),4)
    #print(users, date, region, category)
    #Plotly graph
    dic = {
        'data'  : [go.Indicator(mode = "number",value = ctr, number = {'font' : {'size' : 28}}, domain = {'y' : [0,1]})],
        'layout' : go.Layout(title = {'text' : '<b>CTR<b>', 'y': 1, 'x' : 0.5, 'xanchor' : 'center', 'yanchor' : 'top' }, font=dict(color='black', size=10), height=60, paper_bgcolor = '#DADDD8')
    }
    
    return dic

@app.callback(
    Output('adj_CTR', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_adj_ctr(date, region, category):
    date = pd.Timestamp(date)
    month = str(date.year) + '-' + str(date.month)
    df = filter_df_month(df_new, month, category, region)
    df = df.groupby('date').sum().reset_index()
    df['ctr'] = df['click'] / df['impression']
    df['ctr_impadj'] = df['ctr'] * df['impression']/df.impression.mean()
    ctr_adj = round(df['ctr_impadj'].mean(),4)
    #print(ctr_adj)
    #print(users, date, region, category)
    #Plotly graph
    dic = {
        'data'  : [go.Indicator(mode = "number",value = ctr_adj, number = {'font' : {'size' : 28}}, domain = {'y' : [0,1]})],
        'layout' : go.Layout(title = {'text' : '<b>Impression Adjusted CTR<b>', 'y': 1, 'x' : 0.5, 'xanchor' : 'center', 'yanchor' : 'top' }, font=dict(color='black', size=10), height=60, paper_bgcolor = '#DADDD8')
    }
    
    return dic

@app.callback(
    Output('user_stats', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_user_graph(date, region, category):
    date = pd.Timestamp(date)
    month = str(date.year) + '-' + str(date.month)
    df = filter_df_month(df_new, month, category, region)
    
    #print(len(df))
    df = df.groupby(['date'])['impression', 'total_users', 'click'].sum().reset_index()
    #print(df.impression.sum()/df.total_users.sum())
    df['impression_per_user'] = df['impression'] / df['total_users']
    df['click_per_user'] = df['click'] / df['total_users']
    fig = make_subplots(specs=[[{"secondary_y": True}]])
    fig.update_layout(paper_bgcolor = '#DADDD8')
    
    fig.add_trace(go.Scatter( x=df['date'], y=df['impression_per_user'], mode = 'lines', name="Impression per user"), secondary_y=False)
    fig.add_trace(go.Scatter( x=df['date'], y=df['click_per_user'], mode = 'lines', name="Click per user", line = {'color' : 'green'}), secondary_y=True)
    fig.update_yaxes(title_text="<b>Impression per user</b>", secondary_y=False)
    fig.update_yaxes(title_text="<b>Click per user</b>", secondary_y=True)

    #mean1 = go.Scatter(x = [df.index.min(),df.index.max()], y = [df.ctr_impadj.mean(), df.ctr_impadj.mean()], mode = 'lines', line={'dash': 'dash', 'color':'firebrick'}, name="Mean of Adjusted CTR")
    #fig.add_trace(mean1)
    fig.update_layout(
    xaxis_title = '<b>Date<b>',  showlegend=True,
            height = 320, margin=dict(t=2), yaxis = {'showgrid' : False})
    
    return fig

@app.callback(
    Output('overall_kpi', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_kpi(date, region, category):
    date = pd.Timestamp(date)
    month = str(date.year) + '-' + str(date.month)
    df = filter_df_month(df_new, month, category, region)
    df = df.groupby('date').sum().reset_index()
    df['ctr'] = df['click'] / df['impression']
    df['ctr_impadj'] = df['ctr'] * df['impression']/df.impression.mean() 

    #Make subplots
    fig = make_subplots(rows=2, cols=2, x_title = "<b>Date<b>", y_title = "<b>Frequency<b>", subplot_titles=("<b>Total Users<b>", "<b>Impression<b>", "<b>Clicks<b>", "<b>CTR (Impression Adjusted in Red)<b>"))
    fig.update_layout(title = "<b>Monthly KPI<b>",height = 500, showlegend=False, paper_bgcolor = '#DADDD8', titlefont = {'size':28})

    fig1 = go.Scatter(x=df.date, y=df.total_users, mode='lines', line={'color' : 'blue'})
    mean1 = go.Scatter(x = [df.date.min(),df.date.max()], y = [df.total_users.mean(), df.total_users.mean()], mode = 'lines', line={'dash': 'dash', 'color':'firebrick'})
    fig.add_trace(fig1, row=1, col=1)
    fig.add_trace(mean1, row=1, col=1)


    fig2 = go.Scatter(x=df.date, y=df.impression, mode='lines', line={'color':'blue'})
    mean2 = go.Scatter(x = [df.date.min(),df.date.max()], y = [df.impression.mean(), df.impression.mean()], mode = 'lines', line={'dash': 'dash', 'color':'firebrick'})

    fig.add_trace(fig2, row=1, col=2)
    fig.add_trace(mean2, row=1, col=2)

    fig3 = go.Scatter(x=df.date, y=df.click, mode='lines', line={'color':'blue'})
    mean3 = go.Scatter(x = [df.date.min(),df.date.max()], y = [df.click.mean(), df.click.mean()], mode = 'lines', line={'dash': 'dash', 'color':'firebrick'})

    fig.add_trace(fig3, row=2, col=1)
    fig.add_trace(mean3, row=2, col=1)

    fig4 = go.Scatter(x=df.date, y=df.ctr, mode='lines', line={'color':'blue'})
    mean4 = go.Scatter(x = [df.date.min(),df.date.max()], y = [df.ctr.mean(), df.ctr.mean()], mode = 'lines', line={'dash': 'dash', 'color':'firebrick'})

    fig.add_trace(fig4, row=2, col=2)
    fig.add_trace(go.Scatter(x=df.date, y=df.ctr_impadj, mode='lines', line={'color':'red'}), row=2, col=2)
    fig.add_trace(mean4, row=2, col=2) 

    
    return fig

@app.callback(
    Output('regional_searches', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_category_graph(date, region, category):
    date = pd.Timestamp(date)
    #month = str(date.year) + '-' + str(date.month)
    df = filter_df(df_new, date, category, region)
    df = df.groupby(['categories', 'region']).click.sum().sort_values(ascending=True).reset_index()

    #Sort the df based on its sum and get the index
    _ = df.groupby(['categories']).click.sum().sort_values(ascending=True)

    #Get sorted df
    df.sort_values('categories', inplace=True)
    df.set_index('categories', inplace=True)
    df = df.loc[_.index]
    df.reset_index(inplace=True)

    fig = px.bar(df, x = 'click', y='categories', color = 'region', orientation='h', labels = {'category' : 'Category', 'click' : 'Clicks'})

    fig.update_layout(
    xaxis_title = '<b>Count<b>', yaxis_title='',   showlegend=True,
            height = 320, margin=dict(t=2), yaxis = {'showgrid' : False,}, paper_bgcolor = '#DADDD8')   

    
    return fig

@app.callback(
    Output('access', 'figure'),
    [Input('slct_date', 'value'), Input('slct_region','value'), Input('slct_product' , 'value')]
)

def update_pie_chart(date, region, category):
    date = pd.Timestamp(date)
    #month = str(date.year) + '-' + str(date.month)
    df = filter_df(df_new, date, category, region)
    fig = px.pie(df, values='total_users', names='access')
    fig.update_layout(showlegend=True,
            height = 320, margin=dict(t=2), yaxis = {'showgrid' : False,}, paper_bgcolor = '#DADDD8')
    return fig


if __name__=="__main__":
    app.run_server(debug=True,  use_reloader=True)