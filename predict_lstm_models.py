#!/usr/bin/python

import math
import sys, os, os.path, fnmatch
import numpy as np
from numpy import concatenate
from matplotlib import pyplot
import pandas as pd
from pandas import read_csv
from pandas import DataFrame
from pandas import concat
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_squared_error
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Dropout
from numpy import array
from numpy import argmax
from keras.utils import to_categorical
from keras.models import model_from_json
from sklearn.externals import joblib
import csv


# convert series to supervised learning
def series_to_supervised(data, n_in=1, n_out=1, lag=1, dropnan=True):
    n_vars = 1 if type(data) is list else data.shape[1]
    df = DataFrame(data)
    cols, names = list(), list()
    # input sequence (t-n, ... t-1)
    for i in range(n_in, 0, -1):
        cols.append(df.shift(i+lag))
        names += [('var%d(t-%d)' % (j+1, i+lag-1)) for j in range(n_vars)]
    # forecast sequence (t, t+1, ... t+n)
    for i in range(0, n_out):
        cols.append(df.shift(-i))
        if i == 0:
            names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
        else:
            names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
    # put it all together
    agg = concat(cols, axis=1)
    agg.columns = names
    # drop rows with NaN values
    if dropnan:
        agg.dropna(inplace=True)
    return agg

# Predict something with our learned model and scaler
def predict(model, scaler, test_X, test_y, n_in, n_features, subtract=0):
    yhat_raw = model.predict(test_X)
    if n_in > 1:
        test_X2 = test_X.reshape((test_X.shape[0], n_in*n_features))
    else:
        test_X2 = test_X.reshape((test_X.shape[0], test_X.shape[2]))

    # invert scaling for forecast
    inv_test_X2 = scaler.inverse_transform(test_X2)

    yhat = np.argmax(yhat_raw, axis=1)

    rmse = math.sqrt(mean_squared_error(test_y, yhat))
    print('Test RMSE: %.9f' % rmse)

    yhat = yhat - subtract
    return (yhat_raw, yhat)


# x should be test_X2 when passed in
# price_col needs to be the column the price is in in x. I slapped a bunch of variables onto the front
# so it's not 0 anymore

def plot_predictions(x, test_y, yhat_raw, yhat, plot_filename_graphs, plot_filename_correlations, price_col, label_min, label_max):

    graph_i=6
    pyplot.figure(figsize=(6,12))
    pyplot.suptitle(plot_filename_graphs[5:-4])
    pyplot.subplots_adjust(hspace=1)
    pyplot.subplot(graph_i,1,1)

    pyplot.plot(x[:,price_col]) # Hacked a little bit
    pyplot.title("Price")
    pyplot.subplot(graph_i,1,2)
    pyplot.plot(test_y)
    pyplot.title("Label")
    pyplot.subplot(graph_i,1,3)
    pyplot.plot(yhat)
    pyplot.title("Argmax")
    
    #pp=np.average(yhat_raw, axis=1, weights=range(label_max))
    pp = np.sum(yhat_raw[:,3:], axis=1)
    #pp[pp>0.004]=0.004
    pyplot.subplot(graph_i,1,4)
    pyplot.plot(pp)
    pyplot.title("Average")
    pyplot.subplot(graph_i,1,5)
    pyplot.plot(yhat_raw)
    pyplot.title("All predictions")
    pyplot.savefig(plot_filename_graphs)
    pyplot.close()

    pyplot.figure()
    pyplot.suptitle(plot_filename_correlations[5:-4])
    pyplot.subplot(2,1,1)
    pyplot.plot(yhat, test_y, 'ro', alpha=0.007)
    pyplot.title("Argmax")
    pyplot.subplot(2,1,2)
    pyplot.plot(pp, test_y, 'ro', alpha=0.007)
    pyplot.title("Average")
    pyplot.savefig(plot_filename_correlations)
    pyplot.close()


def run_on_test_data(wfilename, scaler, model, n_in=1, n_out=0, lag=0):
    # Load the dataset
    dataset = read_csv(wfilename, header=0)
    values = dataset.values[:,:-1]

    n_col=values.shape[1]
    reframed = series_to_supervised(values, n_in, n_out, lag)
    if reframed.shape[0]==0:
        print("No non-NA data found for this coin. Skipping")
        null = np.zeros((1,1))
        return (null, null, null, null)

    nn = reframed.columns

    # normalize features
    reframed = DataFrame(scaler.transform(reframed))
    reframed.columns=nn

    test_y = dataset.values[n_in:,-1]
    test_X = reframed.values

    if n_in > 1:
        test_X = test_X.reshape((test_X.shape[0], n_in, n_col))
    else:
        test_X = test_X.reshape((test_X.shape[0], 1, test_X.shape[1]))

    yhat_raw = model.predict(test_X)
    yhat = np.argmax(yhat_raw, axis=1)

    # invert scaling for forecast
    #inv_test_X2 = scaler.inverse_transform(test_X2)
    rmse = math.sqrt(mean_squared_error(test_y, yhat))
    print('Test RMSE: %.9f' % rmse)

    return (test_X, test_y, yhat_raw, yhat)

def buy(test_X, yhat_raw, price_col, scaler, method_params={}):

    # Methods:
    # avg_thresh = Threshold on the average, method_params={thresh: <threshold>}
    # avg_diff_thresh = Threshold on the diff of the average, method_params={thresh: <threshold>}
    # ...
    # Amount of time above a certain level
    # EWMA?
    # Different kind of weighting (something like clarity of confidence, or all probs above a label > X)

    method = method_params["method"]

    buy_idx = np.zeros((0,0))
    if method == "avg_thresh":
        pp = np.average(yhat_raw, axis=1, weights=range(method_params["label_max"]))
        buy_idx = np.argwhere(pp > method_params["thresh"])[:,0]
    if method == "pct_gt":
        label_gt = method_params["label_gt"]
        pp = np.sum(yhat_raw[:,label_gt:], axis=1)
        buy_idx = np.argwhere(pp > method_params["thresh"])[:,0]

    if len(buy_idx):
        if test_X.shape[1]-buy_idx[-1] < 30: # It said buy in the last 30 min
            return True
        else:
            return False
    else:
        return False

# TODO: Replace this with a read straight from MySQL
model_path = "models/"
data_path = "test_predict/"
filename = "_predict.csv"
data_files = os.listdir(data_path)
data_files = [d for d in data_files if fnmatch.fnmatch(d, "*.csv")]
timesteps=1
price_col=8

coins_to_buy = []

for fname in data_files:
    
    uscore = fname.find("_")
    coin = fname[:uscore]
    print("Predicting " + coin)
    json_file = model_path+coin+'_model.json'

    weights_file = model_path+coin+'_model.h5'
    scaler_file = model_path+coin+"_scaler.save"
    if not os.path.isfile(json_file):
        print("Couldn't find model file for " + coin)
        continue
    if not os.path.isfile(weights_file):
        print("Couldn't find weights file for " + coin)
        continue
    if not os.path.isfile(scaler_file):
        print("Couldn't find scaler file for " + coin)
        continue

    jsonf = open(json_file, 'r')
    loaded_model_json = jsonf.read()
    jsonf.close()
    model = model_from_json(loaded_model_json)
    # load weights into new model
    model.load_weights(weights_file)
    print("Loaded model from disk for " + coin)
    scaler = joblib.load(scaler_file) 
    
    (test_X2, test_y2, yhat_raw2, yhat2) = run_on_test_data(data_path+fname, scaler, model, n_in=timesteps)
    if (test_X2.shape[0]==1):
        print("No testing data found for this coin. Skipping.")
        continue
    
    if timesteps > 1:
        x2 = test_X2.reshape((test_X2.shape[0], timesteps, n_col))
    else:
        x2 = test_X2.reshape((test_X2.shape[0], test_X2.shape[2]))

    method_params={"method":"pct_gt", "thresh":0.6, "label_gt":3}

    should_buy = buy(x2, yhat_raw2, price_col, scaler, method_params=method_params)
    if should_buy:
        coins_to_buy.append(coin.upper())

f = csv.writer(open("coins.to.buy.csv", "w"))
for c in coins_to_buy:
    f.writerow([c])
