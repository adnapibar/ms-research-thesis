import time
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from keras.layers.core import Dense, Activation, Dropout
from keras.layers.recurrent import LSTM
from keras.models import Sequential

# Set a random seed to reproduce the results
np.random.seed(1234)

# Load the volume data
volume_data = pd.read_csv('../data/volume_data.csv', header=None)


# Return a training and test data for a site.
def train_test_traffic_data(sequence_length=50):
    sample_size = volume_data.shape[0]
    roads_size = volume_data.shape[1]

    result = []
    # Create data
    for j in range(roads_size):
        road_vol = volume_data[j]
        road_vol = road_vol.replace(0, int(road_vol.mean())).values
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
    epochs = 5
    sequence_length = 50

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
    x = np.arange(500)
    y_test_tp = np.transpose(y_test)
    pred_tp = np.transpose(predicted)
    for i in range(y_test_tp.shape[0]):
        plt.plot(x, y_test_tp[i][:300], label='Actual')
        plt.plot(x, pred_tp[i][:300], label='Predicted')
        plt.legend(loc=2)
        plt.savefig('../latex-thesis/Figures/lstm'+ str(i) + '.pdf')
        plt.close()


if __name__ == '__main__':
    print('Starting the network...')
    model, y_test, predicted = run_network()
    print('Plotting test vs prediction...')
    plot_predictions(y_test, predicted)
    print('Done!')
