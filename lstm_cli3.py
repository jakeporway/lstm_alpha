#!/usr/bin/python

import math
import sys
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

# Load a CSV of training and validation data. The predictor must be the last column
def load_training_set(coin, extension, label_min, label_max, n_in=1, n_out=0, lag=0, train_pct=0.8):
    # Load the dataset
    dataset = read_csv(coin+extension, header=0)
    values = dataset.values[:,:-1]

    n_col = values.shape[1]
    # frame as supervised learning
    reframed = series_to_supervised(values, n_in, n_out, lag)
    if reframed.shape[0]==0:
        print("No non-NA data found for this coin. Skipping")
        null = np.zeros((1,1))
        return (null, null, null, null, null)


    nn = reframed.columns

    # normalize features
    scaler = MinMaxScaler(feature_range=(0, 1))
    reframed = DataFrame(scaler.fit_transform(reframed))
    reframed.columns=nn

    label = dataset.values[n_in:,-1]

    # split into train and test sets
    values = reframed.values
    n_train_hours = int(math.floor(values.shape[0]*train_pct))
    train_X = values[:n_train_hours, :]
    test_X = values[n_train_hours:, :]

    # split into input and outputs
    train_y = label[:n_train_hours]
    test_y = label[n_train_hours:]

    label_diff = label_max-label_min
    if label_min < 0:
        train_y = train_y + int(label_diff/2.0)
        test_y = test_y + int(label_diff/2.0)

    train_y[train_y > label_max-1] = label_max-1
    test_y[test_y > label_max-1] = label_max-1

    train_y = to_categorical(train_y, label_max)
    test_y = to_categorical(test_y, label_max)

    # reshape input to be 3D [samples, timesteps, features]

    if n_in > 1:
        train_X = train_X.reshape((train_X.shape[0], n_in, n_col))
        test_X = test_X.reshape((test_X.shape[0], n_in, n_col))
    else:
        train_X = train_X.reshape((train_X.shape[0], 1, train_X.shape[1]))
        test_X = test_X.reshape((test_X.shape[0], 1, test_X.shape[1]))

    return (train_X, train_y, test_X, test_y, scaler, n_col)

def train_model(train_X, train_y, test_X, test_y, epochs, batch_size, plot_file, lstm_layers, dense_layer, dropout=0.4):

    print("Creating NN with " + str(len(lstm_layers)) + " layers: " + str(lstm_layers) + " and dense layer of " + str(dense_layer) + ".")

    # design network
    model = Sequential()

    for i in range(len(lstm_layers)-1):
        if i == 0:
            model.add(LSTM(lstm_layers[i], return_sequences=True, input_shape=(train_X.shape[1], train_X.shape[2])))
        else:
            model.add(LSTM(lstm_layers[i], return_sequences=True))
    model.add(LSTM(lstm_layers[-1]))
    model.add(Dropout(dropout))
    model.add(Dense(dense_layer, activation='softmax'))
    model.compile(loss='categorical_crossentropy', optimizer='adam')
    # fit network
    history = model.fit(train_X, train_y, epochs=epochs, batch_size=batch_size, validation_data=(test_X, test_y), verbose=2, shuffle=False)

    # plot history
    pyplot.figure()
    pyplot.plot(history.history['loss'], label='train')
    pyplot.plot(history.history['val_loss'], label='test')
    pyplot.legend()
    pyplot.savefig(plot_file)
    pyplot.close()

    return (history, model)

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



def run_on_test_data(coin, wfilename, scaler, model, n_in=1, n_out=0, lag=0):
    # Load the dataset
    dataset = read_csv(coin+wfilename, header=0)
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


# Todo: Instead of using % above thresh, instead do something like average (maybe?)
def compute_accuracy(predictor, test_y, nbreaks, min_label=3):
    # for nbreaks thresholds across the predictor variables, compute % above 3 and # above 3
    mn = min(predictor)
    mx = max(predictor)
    index = np.linspace(mn, mx, nbreaks)
    results = np.zeros((nbreaks,5))
    cnt = 0
    i = mn
    vals_above = test_y[predictor >= i]
    good_vals_above = vals_above[vals_above >= min_label]
    nvals_below = len(predictor)-len(good_vals_above)
    num_good = len(good_vals_above)
    pct_good = num_good/float(len(vals_above))
    baseline_good_pct = pct_good
    for i in index:
        vals_above = test_y[predictor >= i]
        good_vals_above = vals_above[vals_above >= min_label]
        nvals_below = len(predictor)-len(good_vals_above)
        num_good = len(good_vals_above)
        pct_good = num_good/float(len(vals_above))
        improve = pct_good - baseline_good_pct
        results[cnt,:] = (i, pct_good, improve, num_good, pct_good*pct_good*num_good)
        cnt=cnt+1

    return (results, baseline_good_pct)

def test_accuracy(predictor, test_y, threshold, min_label=3):
    vals_above = test_y[predictor >= threshold]
    good_vals_above = vals_above[vals_above >= min_label]
    nvals_below = len(predictor)-len(good_vals_above)
    num_good = len(good_vals_above)
    if (len(vals_above) <= 0):
        pct_good=1
    else:
        pct_good = num_good/float(len(vals_above))
    return (num_good, pct_good)


def buy(test_X, yhat_raw, price_col, scaler, filename, strategy, method_params={}):

    # Methods:
    # avg_thresh = Threshold on the average, method_params={thresh: <threshold>}
    # avg_diff_thresh = Threshold on the diff of the average, method_params={thresh: <threshold>}
    # ...
    # Amount of time above a certain level
    # EWMA?
    # Different kind of weighting (something like clarity of confidence, or all probs above a label > X)

    inv_X = scaler.inverse_transform(test_X)
    
    method = method_params["method"]
    pct_gain = strategy["pct_gain"]
    pct_loss = strategy["pct_loss"]
    days_to_hold = strategy["days_to_hold"]

    buy_idx = np.zeros((0,0))
    if method == "avg_thresh":
        pp = np.average(yhat_raw, axis=1, weights=range(method_params["label_max"]))
        buy_idx = np.argwhere(pp > method_params["thresh"])[:,0]
    if method == "pct_gt":
        label_gt = method_params["label_gt"]
        pp = np.sum(yhat_raw[:,label_gt:], axis=1)
        buy_idx = np.argwhere(pp > method_params["thresh"])[:,0]

    n = yhat_raw.shape[0]
    sell_idx = np.zeros(len(buy_idx))
    pct_diff = np.zeros(len(buy_idx))

    for i in range(len(buy_idx)):
        #print i
        bb = int(buy_idx[i])
        mn = min(days_to_hold*24*60, n-bb-1)
        buy_price = inv_X[int(buy_idx[i]), price_col]
        high_target = buy_price*(1+pct_gain)
        low_target = buy_price*(1-pct_loss)

        hit_high = np.argwhere(inv_X[(bb+1):(bb+mn),price_col] >= high_target)+bb+1
        hit_low = np.argwhere(inv_X[(bb+1):(bb+mn),price_col] <= low_target)+bb+1

        hh = hl = 0
        if len(hit_high):
            hh = hit_high[0]
        if (len(hit_low)):
            hl = hit_low[0]

        if hh and hl:
            if hh < hl:
                sell_idx[i] = hh
            else:
                sell_idx[i] = hl
        elif hh:
            sell_idx[i] = hh
        elif hl:
            sell_idx[i] = hl
        else:
            sell_idx[i] = bb+mn

        sell_price = inv_X[int(sell_idx[i]), price_col]
        if buy_price == 0:
            pct_diff[i] = 0
        else:
            pct_diff[i] = (sell_price-buy_price)/(buy_price)


    pyplot.figure()
    pyplot.subplot(4, 1, 1)
    pyplot.plot(range(inv_X.shape[0]),inv_X[:,price_col])
    pyplot.title("Price")
    pyplot.subplot(4, 1, 2)
    pyplot.plot(range(inv_X.shape[0]),pp)
    pyplot.title("Average")
    pyplot.subplot(4, 1, 3)
    bbidx = np.zeros(inv_X.shape[0])
    if buy_idx.shape[0]:
        bbidx[buy_idx] = 1
    pyplot.plot(range(inv_X.shape[0]), bbidx)
    pyplot.title("Buys")
    pyplot.subplot(4, 1, 4)
    ppidx = np.zeros(inv_X.shape[0])
    if buy_idx.shape[0]:
        ppidx[buy_idx]=pct_diff
    pyplot.plot(range(inv_X.shape[0]), ppidx)
    pyplot.title("Pct Gain")
    pyplot.savefig(filename)
    pyplot.close()

    return (buy_idx, sell_idx, pct_diff)


#coins = ["2GIVE", "ABY", "ADA", "ADT", "AEON", "AMP", "ANT", "ARDR", "ARK", "AUR", "BAT", "BAY", "BCY", "BITB", "BLITZ", "BLK", "BLOCK", "BNT", "BRK", "BRX", "BSD", "BTG", "BURST", "BYC", "CANN", "CFI", "CLAM", "CLOAK", "COVAL", "CRW", "CURE", "CVC", "DASH", "DCR", "DCT", "DGB", "DMD", "DNT", "DOGE", "DOPE", "DTB", "DYN", "EBST", "EDG", "EFL", "EGC", "EMC", "EMC2", "ENRG", "ERC", "ETC", "EXCL", "EXP", "FCT", "FLDC", "FLO", "FTC", "FUN", "GAM", "GAME", "GBG", "GBYTE", "GCR", "GEO", "GLD", "GNO", "GNT", "GOLOS", "GRC", "GRS", "GUP", "HKG", "HMQ", "INCNT", "INFX", "IOC", "ION", "IOP", "KMD", "KORE", "LBC", "LGD", "LMC", "LSK", "LUN", "MAID", "MANA"]
#coins = ["ETC", "BNT", "FLDC", "ARDR", "2GIVE", "EGC", "BRX", "BTG", "ARK", "AUR", "EBST", "EGC"]
coins = ["ANT", "BSD", "BNT", "ARDR", "2GIVE", "ABY", "BRX", "BTG", "ARK", "AUR","AEON", "CLAM"]
#coins = ["AMP", "ANT", "ARDR", "ARK", "AUR"]
#coins = ["ANT", "ARDR", "BRK", "DOGE"] # T1 > T2

training_filename = "_training_through_feb.csv"
test1_filename = "_mar_apr.csv"
test2_filename = "_apr_may.csv"
lstm_layers = [40,40]
epochs=50
price_col=8
batch_size=1800
label_min=0
label_max=15
timesteps=1
do_training=True
cutoff = 0.95

strategy = {}
strategy["pct_gain"]=0.15
strategy["pct_loss"]=100
strategy["days_to_hold"]=4
results_file="results_sample.csv"

f = csv.writer(open(results_file, "w"))
f.writerow(["coin", "threshold", "sg_training", "nb_training", "sg_test1", "nb_test1", "sg_test2", "nb_test2"])

for coin in coins:
    
    csvrow = []
    csvrow.append(coin)

    print("\n-=-=-=-=- " + coin + " -=-=-=-=-")
    if coin == "" or len(sys.argv) > 1:
        coin = str(sys.argv[1])

    
    print("-- Loading data")
    (train_X, train_y, test_X, test_y, scaler, n_col) = load_training_set(coin, training_filename, label_min=label_min, label_max=int(math.fabs(label_min)+label_max), n_in=timesteps)

    if (train_X.shape[0]==1):
        print("No training data found. Skipping")
        continue

    if do_training:
        print("-- Training model")
        (history, model) = train_model(train_X, train_y, test_X, test_y, epochs, batch_size, "figs/"+coin+"_0_train_test_error.png", lstm_layers, dense_layer=int(math.fabs(label_min)+label_max))

        # Test saving the model
        model_json = model.to_json()
        with open(coin+"_model.json", "w") as json_file:
            json_file.write(model_json)
        # serialize weights to HDF5
        model.save_weights(coin+"_model.h5")
        print("Saved model to disk")

        scaler_filename = coin+"_scaler.save"
        joblib.dump(scaler, scaler_filename)
    else:
        json_file = open(coin+'_model.json', 'r')
        loaded_model_json = json_file.read()
        json_file.close()
        loaded_model = model_from_json(loaded_model_json)
        # load weights into new model
        loaded_model.load_weights(coin+"_model.h5")
        print("Loaded model from disk")
        scaler_filename = coin+"_scaler.save"
        scaler = joblib.load(scaler_filename) 

    # Compress test_y back into a single number (undo one-hot)
    test_y = np.argmax(test_y, axis=1)

    print("-- Predicting")
    (yhat_raw, yhat) = predict(model, scaler, test_X, test_y, timesteps, n_col)

    if timesteps > 1:
        x2 = test_X.reshape((test_X.shape[0], timesteps, n_col))
    else:
        x2 = test_X.reshape((test_X.shape[0], test_X.shape[2]))

    print("-- Plotting")
    plot_predictions(x2, test_y, yhat_raw, yhat, "figs/"+coin+"_1_validation_graphs.png", "figs/"+coin+"_2_validation_correlations.png", price_col=price_col, label_min=label_min, label_max=label_max)

    (results, baseline) = compute_accuracy(np.average(yhat_raw, axis=1, weights=range(label_min, label_max)), test_y, 100)

    best_improve = np.argmax(results[:,2])
    best_num = np.argmax(results[:,4])

    # There are a couple of ways to choose the best threshold. Right now
    # I'm trying to find definitive thresholds, so I'm just going to go
    # with the biggest number of points above a threshold of 95% accuracy.
    # If there are none, we'll not predict this coin
    
    best = np.argwhere(results[:,1]>cutoff)
    classification_threshold = 0
    if (len(best)==0):
        best_row = results[np.argmax(results[:,1]),:]
        print("No threshold yieled more than " + str(cutoff))
        best_row[0]=100000
        print("Selecting " + str(best_row[0]) + " with accuracy " + str(best_row[1]) + " instead, an improvement of " + str(best_row[2]))
    else:
        best_row=results[best[np.argmax(results[best,3])],:]
        best_row=best_row[0,:]
        print("Selecting " + str(best_row[0]) + " with accuracy " + str(best_row[1]) + ", an improvement of " + str(best_row[2]))
    final_threshold = best_row[0]

    #f.write(coin + "\n")
    #f.write("threshold: " + str(best_row[0]) + ", " + str(best_row[1]) + ", " + str(best_row[2]) + ", " + str(best_row[3]) + "\n")
    csvrow.append(best_row[0])

    
    #method_params={"method":"avg_thresh","thresh":best_row[0], "label_max":label_max}
    method_params={"method":"pct_gt", "thresh":0.6, "label_gt":3}

    if timesteps > 1:
        x2 = test_X.reshape((test_X.shape[0], timesteps, n_col))
    else:
        x2 = test_X.reshape((test_X.shape[0], test_X.shape[2]))


    (bi, si, pg) = buy(x2, yhat_raw, price_col, scaler, "figs/"+coin+"_2_validation_buys.png", strategy, method_params=method_params)

    print("Sum gain: [" + str(sum(pg)) + "] Num buys: " + str(len(pg)))
    #f.write("Sum gain: [" + str(sum(pg)) + "] Num buys: " + str(len(pg))+"\n")
    csvrow.extend([sum(pg), len(pg)])

    print("-- Running on test 1 data")
    (test_X2, test_y2, yhat_raw2, yhat2) = run_on_test_data(coin, test1_filename, scaler, model, n_in=timesteps)
    if (test_X2.shape[0]==1):
        print("No testing data found for this coin. Skipping.")
    #    continue


    if timesteps > 1:
        x2 = test_X2.reshape((test_X2.shape[0], timesteps, n_col))
    else:
        x2 = test_X2.reshape((test_X2.shape[0], test_X2.shape[2]))

    print("-- Plotting")
    plot_predictions(x2, test_y2, yhat_raw2, yhat2, "figs/"+coin+"_3_test1_graphs.png", "figs/"+coin+"_4_test1_correlations.png", price_col=price_col, label_min=label_min, label_max=label_max)
    #(num_good, pct_good) = test_accuracy(np.average(yhat_raw2, axis=1, weights=range(label_min, label_max)), test_y2, final_threshold, 3)
    #print("++ Test set 1: pct good: " + str(pct_good) + " Num good: " + str(num_good))
    #f.write("++ Test set 1:  pct good: " + str(pct_good) + "  num good: " + str(num_good) + "\n\n")

    (bi, si, pg) = buy(x2, yhat_raw2, price_col, scaler, "figs/"+coin+"_4_test1_buys.png", strategy, method_params=method_params)
    print("Sum gain: [" + str(sum(pg)) + "] Num buys: " + str(len(pg)))
    #f.write("Sum gain: [" + str(sum(pg)) + "] Num buys: " + str(len(pg))+"\n")
    csvrow.extend([sum(pg), len(pg)])

    print("-- Running on test 2 data")
    (test_X3, test_y3, yhat_raw3, yhat3) = run_on_test_data(coin, test2_filename, scaler, model, n_in=timesteps)
    if (test_X3.shape[0]==1):
        print("No testing data found for this coin. Skipping.")
    #   continue

    if timesteps > 1:
        x3 = test_X3.reshape((test_X3.shape[0], timesteps, n_col))
    else:
        x3 = test_X3.reshape((test_X3.shape[0], test_X3.shape[2]))


    print("-- Plotting")
    plot_predictions(x3, test_y3, yhat_raw3, yhat3, "figs/"+coin+"_5_test2_graphs.png", "figs/"+coin+"_6_test2_correlations.png", price_col=2, label_min=label_min, label_max=label_max)
    #(num_good, pct_good) = test_accuracy(np.average(yhat_raw2, axis=1, weights=range(label_min, label_max)), test_y2, final_threshold, 3)
    #print("++ Test set 2: pct good: " + str(pct_good) + " Num good: " + str(num_good))
    #f.write("++ Test set 2:  pct good: " + str(pct_good) + "  num good: " + str(num_good) + "\n\n")
    
    (bi, si, pg) = buy(x3, yhat_raw3, price_col, scaler, "figs/"+coin+"_6_test2_buys.png", strategy, method_params=method_params)
    print("Sum gain: [" + str(sum(pg)) + "] Num buys: " + str(len(pg)))
    #f.write("Sum gain: [" + str(sum(pg)) + "] Num buys: " + str(len(pg))+"\n")
    csvrow.extend([sum(pg), len(pg)])
    f.writerow(csvrow)

    print("-- Done.")

del f


#9745630508
