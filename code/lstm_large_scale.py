import time
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from keras.layers.core import Dense, Activation, Dropout
from keras.layers.recurrent import LSTM
from keras.models import Sequential
from sklearn.metrics import mean_absolute_error, mean_squared_error


# Set a random seed to reproduce the results
np.random.seed(1234)

# Load the volume data
# Brunswick Road - 6336, 6338; 16572
# Flemington Road - 3393, 3394, 3396, 3398; 13632, 13634, 13635, 13637
# Elizabeth Street -
# Victorial Parade - 3592, 3593, 3594, 3595, 3596; 13829, 13831, 13832, 13835
# Hoddle Street - 268, 272, 273, 274, 275, 277, 278; 6278, 10522, 10523, 10525, 10526, 10527,
# 10528, 16515, 16517
# Nicholson Street - 5539, 5540, 5541, 5543, 5544, 5545, 9615; 15773, 15774, 15775, 15777, 15778,
#  19854, 19855
# Royal Parade - 6782, 6783, 6784, 6786; 17022, 17023, 17025
# Lygon Street - 9424, 9425, 9426, 9427, 9429; 19659, 19660, 19661, 19662, 19663

volume_data = pd.read_csv('../data/volume_data.csv', header=None)

all_hfs = pd.read_csv('../data/hf_list.csv')

used_hfs = [6336, 6338, 16572, 3393, 3394, 3396, 3398, 13632, 13634, 13635, 13637,
            3592, 3593, 3594, 3595, 3596, 13829, 13831, 13832, 13835, 268, 272, 273,
            274, 275, 277, 278, 6278, 10522, 10523, 10525, 10526, 10527, 10528, 16515, 16517,
            5539, 5540, 5541, 5543, 5544, 5545, 9615, 15773, 15774, 15775, 15777, 15778,
            19854, 19855, 6782, 6783, 6784, 6786, 17022, 17023, 17025, 9424, 9425, 9426, 9427,
            9429, 19659, 19660, 19661, 19662, 19663]


def find_index_hf(hf_no):
    return all_hfs[all_hfs['GID'] == hf_no].index.tolist()[0]


means = []
maxs = []


# Return a training and test data for a site.
def train_test_traffic_data(sequence_length=50):
    sample_size = volume_data.shape[0]
    hfs = [find_index_hf(hf_no) for hf_no in used_hfs]

    result = []
    # Create data
    for j in hfs:
        road_vol = volume_data[j]
        road_vol = road_vol.replace(0, int(road_vol.mean())).values
        means.append(road_vol.mean())
        maxs.append(road_vol.max())
        road_vol = (road_vol - road_vol.mean()) / road_vol.max()
        temp = []
        for i in range(sample_size - sequence_length):
            temp.append(road_vol[i: i + sequence_length])
        result.append(temp)

    result = np.dstack(result)

    row = round(0.9 * result.shape[0])
    train = result[:row, :, :]
    np.random.shuffle(train)
    X_train = train[:, :-1, :]
    y_train = train[:, -1, :]
    X_test = result[row:, :-1, :]
    y_test = result[row:, -1, :]

    return [X_train, y_train, X_test, y_test]


def build_model():
    mdl = Sequential()
    io_dim = volume_data.shape[1]
    # a network with 1-dimensional input,
    # three hidden layers of sizes 50, 100 and 100
    # and eventually a 1-dimensional output layer
    layers = [io_dim, 50, 100, 100, io_dim]

    # We also add 20% Dropout in this layer.
    mdl.add(LSTM(
        input_dim=layers[0],
        output_dim=layers[1],
        return_sequences=True))
    mdl.add(Dropout(0.2))

    # 2nd hidden layer
    mdl.add(LSTM(
        layers[2],
        return_sequences=True))
    mdl.add(Dropout(0.2))

    # 3rd hidden layer
    mdl.add(LSTM(
        layers[3],
        return_sequences=False))
    mdl.add(Dropout(0.2))

    # last layer we use is a Dense layer ( = feedforward).
    # Since we are doing a regression, its activation is linear
    mdl.add(Dense(
        output_dim=layers[4]))
    mdl.add(Activation("linear"))

    start = time.time()
    mdl.compile(loss="mse", optimizer="rmsprop")
    print("Compilation Time : ", time.time() - start)
    return mdl


def run_network(mdl=None, data=None):
    global_start_time = time.time()
    epochs = 10
    sequence_length = 100

    if data is None:
        print('Loading data... ')
        X_train, y_train, X_test, y_test = train_test_traffic_data(sequence_length)
    else:
        X_train, y_train, X_test, y_test = data

    print('\nData Loaded. Compiling...\n')

    if mdl is None:
        mdl = build_model()

    try:
        mdl.fit(X_train, y_train, batch_size=512,
                nb_epoch=epochs, validation_split=0.05)
        predicted_trffic = mdl.predict(X_test)
    except KeyboardInterrupt:
        print('Training duration (s) : ', time.time() - global_start_time)
        return mdl, y_test, 0

    print('Training duration (s) : ', time.time() - global_start_time)

    return mdl, y_test, predicted_trffic


def plot_predictions(y_test, predicted):
    x = np.arange(300)
    y_test_tp = np.transpose(y_test)
    pred_tp = np.transpose(predicted)
    for i in range(2):
        actual = y_test_tp[i][:300]
        predictions = pred_tp[i][:300]
        actual = (actual * maxs[i]) + means[i]
        predictions = (predictions * maxs[i]) + means[i]
        plt.plot(x,actual, label='Actual')
        plt.plot(x,predictions, label='Predicted')
        plt.legend(loc=2)
        plt.savefig('../latex-thesis/Figures/lstm'+ str(i) + '.pdf')
        plt.close()


def mean_absolute_percentage_error(y_true, y_pred):
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100


def print_scores(y_test, predicted):
    y_test_tp = np.transpose(y_test)
    pred_tp = np.transpose(predicted)
    mae = []
    mse = []
    mape = []
    for i in range(2):
        actual = y_test_tp[i]
        predictions = pred_tp[i]
        actual = (actual * maxs[i]) + means[i]
        predictions = (predictions * maxs[i]) + means[i]
        mae.append(mean_absolute_error(actual, predictions))
        mse.append(mean_squared_error(actual, predictions))
        mape.append(mean_absolute_percentage_error(actual, predictions))
    print("MAE=", np.array(mae).mean())
    print("MSE=", np.array(mse).mean())
    print("MAPE=", np.array(mape).mean())


if __name__ == '__main__':
    print('Starting the network...')
    model, y_test, predicted = run_network()
    print('Plotting test vs prediction...')
    plot_predictions(y_test, predicted)
    print('Metrics...')
    print_scores(y_test, predicted)
    print('Done!')
