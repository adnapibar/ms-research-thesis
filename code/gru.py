# An LSTM network for short term traffic prediction
from sklearn.metrics import mean_absolute_error, mean_squared_error

import time
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from keras.layers.core import Dense, Activation, Dropout
from keras.layers.recurrent import GRU
from keras.models import Sequential

# Set a random seed to reproduce the results
np.random.seed(1234)

# Load the volume data
volume_data = pd.read_csv('../data/volume_data.csv', header=None)
scaler = None
all_hfs = pd.read_csv('../data/hf_list.csv')

def find_index_hf(hf_no):
    return all_hfs[all_hfs['GID'] == hf_no].index.tolist()[0]


maxs = []
means = []


# Return a training and test data for a site.
def train_test_traffic_data(sitenum, sequence_length=100):
    site_traffic = volume_data[find_index_hf(sitenum)]
    mean_t = int(site_traffic.mean())
    site_traffic = site_traffic.replace(0, mean_t).values
    means.append(site_traffic.mean())
    maxs.append(site_traffic.max())
    site_traffic = (site_traffic - site_traffic.mean()) / site_traffic.max()
    result = []
    for index in range(len(site_traffic) - sequence_length):
        result.append(site_traffic[index: index + sequence_length])
    result = np.array(result)
    row = round(0.9 * result.shape[0])
    train = result[:row, :]
    np.random.shuffle(train)
    X_train = train[:, :-1]
    y_train = train[:, -1]
    X_test = result[row:, :-1]
    y_test = result[row:, -1]

    X_train = np.reshape(X_train, (X_train.shape[0], X_train.shape[1], 1))
    X_test = np.reshape(X_test, (X_test.shape[0], X_test.shape[1], 1))

    return [X_train, y_train, X_test, y_test]


def build_model():
    mdl = Sequential()
    # a network with 1-dimensional input,
    # two hidden layers of sizes 50 and 100
    # and eventually a 1-dimensional output layer
    layers = [1, 50, 100, 1]

    # We also add 20% Dropout in this layer.
    mdl.add(GRU(
        input_dim=layers[0],
        output_dim=layers[1],
        return_sequences=True))
    mdl.add(Dropout(0.2))

    # 2nd hidden layer
    mdl.add(GRU(
        layers[2],
        return_sequences=False))
    mdl.add(Dropout(0.2))

    # last layer we use is a Dense layer ( = feedforward).
    # Since we are doing a regression, its activation is linear
    mdl.add(Dense(
        output_dim=layers[3]))
    mdl.add(Activation("linear"))

    start = time.time()
    mdl.compile(loss="mse", optimizer="rmsprop")
    print("Compilation Time : ", time.time() - start)
    return mdl


def run_network(mdl=None, data=None):
    global_start_time = time.time()
    epochs = 5
    sequence_length = 50

    if data is None:
        print('Loading data... ')
        X_train, y_train, X_test, y_test = train_test_traffic_data(15773, sequence_length)
    else:
        X_train, y_train, X_test, y_test = data

    print('\nData Loaded. Compiling...\n')

    if mdl is None:
        mdl = build_model()

    try:
        mdl.fit(X_train, y_train, batch_size=512,
                nb_epoch=epochs, validation_split=0.05)
        predicted_trffic = mdl.predict(X_test)
        predicted_trffic = np.reshape(predicted_trffic, (predicted_trffic.size,))
    except KeyboardInterrupt:
        print('Training duration (s) : ', time.time() - global_start_time)
        return mdl, y_test, 0

    print('Training duration (s) : ', time.time() - global_start_time)
    y_test = (y_test * maxs[0]) + means[0]
    predicted_trffic = (predicted_trffic * maxs[0]) + means[0]
    return mdl, y_test, predicted_trffic


def plot_predictions(y_test, predicted):
    x = np.arange(400)
    plt.plot(x, y_test[:400], label='Actual')
    plt.plot(x, predicted[:400], label='Predicted')
    plt.legend(loc=2)
    plt.savefig('../latex-thesis/Figures/lstm-single1.pdf')


def mean_absolute_percentage_error(y_true, y_pred):
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100


def print_scores(actual, predictions):
    print("MAE=", mean_absolute_error(actual, predictions))
    print("MSE=", mean_squared_error(actual, predictions))
    print("MAPE=", mean_absolute_percentage_error(actual, predictions))


if __name__ == '__main__':
    print('Starting the network...')
    model, y_test, predicted = run_network()
    print('Plotting test vs prediction...')
    plot_predictions(y_test, predicted)
    print('Error metrics...')
    print_scores(y_test, predicted)
    print('Done!')
