# A K-Nearest Neighbour implementation for short term traffic
# prediction.

from sklearn.neighbors import KNeighborsRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error
# Set a random seed to reproduce the results
import time
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

np.random.seed(1234)


# Load the volume data
volume_data = pd.read_csv('../data/volume_data.csv', header=None)
all_hfs = pd.read_csv('../data/hf_list.csv')


def find_index_hf(hf_no):
    return all_hfs[all_hfs['GID'] == hf_no].index.tolist()[0]


# Return a training and test data for a site.
def train_test_traffic_data(sitenum, sequence_length=50):
    site_traffic = volume_data[find_index_hf(sitenum)]
    mean_t = int(site_traffic.mean())
    site_traffic = site_traffic.replace(0, mean_t).values
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

    return [X_train, y_train, X_test, y_test]


def run_network(mdl=None, data=None):
    global_start_time = time.time()
    sequence_length = 10

    if data is None:
        print('Loading data... ')
        X_train, y_train, X_test, y_test = train_test_traffic_data(15773, sequence_length)
    else:
        X_train, y_train, X_test, y_test = data

    print('\nData Loaded...\n')

    if mdl is None:
        mdl = KNeighborsRegressor(5, weights='distance')

    try:
        mdl.fit(X_train, y_train)
        predicted_trffic = mdl.predict(X_test)
    except KeyboardInterrupt:
        print('Training duration (s) : ', time.time() - global_start_time)
        return mdl, y_test, 0

    print('Training duration (s) : ', time.time() - global_start_time)

    return mdl, y_test, predicted_trffic


def plot_predictions(y_test, predicted):
    x = np.arange(300)
    plt.plot(x, y_test[:300], label='Actual')
    plt.plot(x, predicted[:300], label='Predicted')
    plt.legend(loc=2)
    plt.savefig('../latex-thesis/Figures/knn.pdf')


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
