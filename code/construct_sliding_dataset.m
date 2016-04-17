load VolumeData_small.mat
noDays = size(data,2);
noSensors = size(data,1);
noObs = size(data,3);

%% ---- configure options here -----

winWidth = 6;  % Sliding window width
slideStep = 1;  % Slide step for the window (this is not the response size)
trainPercent = 0.7; % The percentage of the data used for training and parameter tuning
quantilePercent = 0.85; % The threshold for splitting into high/low traffic volume

%% ---- generate (nodays * 96) x noSensors ----
% e.g. concatenate data for one sensor for all time,
% columns = timeseries samples, rows are sensors / features

totalObs = noDays * noObs;

allSensors = zeros(totalObs, noSensors);
for i = 1:noSensors
    d=squeeze(data(i,:,:))';
    allSensors(:,i) = d(:);
end

% make vectors unit length
%allSensors = normc(allSensors); 

clear data HF_list;

%% split into train / validation

testSize = max(winWidth + 1, floor((1 - trainPercent) * totalObs));
trainSize = totalObs - testSize;

trainSet = allSensors(1:trainSize,:);
testSet = allSensors(trainSize+1:totalObs,:);

%clear allSensors;

%% ---- reshape dataset into synchronised sliding windows ----
% for all sensors and target value for each time step
% gather all data into trainining and testing datasets

numStepsTrn = (trainSize - winWidth) / slideStep; % Number of windows train
numStepsTst = (testSize - winWidth) / slideStep; % Number of windows test

% preallocate
trnDat = zeros(numStepsTrn, noSensors*(winWidth-1));
trnLbl = zeros(numStepsTrn, noSensors);
tmp = zeros(winWidth-1, noSensors);

tic;
for i = 1:numStepsTrn % sliding window index through all time series
    for j = 1:noSensors % gather data for all sensors into one row
        tmp(:,j) = trainSet(i:i+winWidth-2,j);
        % each row is a new time step. the values in the rows are the
        % outputs for each sensor, corresponding to the td / (winWidth-1)
        trnLbl(i,j) = trainSet(i+winWidth-1,j);
    end
    % this contains the values of the input sliding window
    % for each sensor, concatenated into a single vector
    % every winWidth-1, a new sensor reading begins
    trnDat(i,:) = reshape(tmp, 1, noSensors*(winWidth-1));
end
toc;
disp('Finished processing train data');

clear trainSet; 

%% same for test data

tstDat = zeros(numStepsTst, noSensors*(winWidth-1));
tstLbl = zeros(numStepsTst, noSensors);
tmp = zeros(winWidth-1, noSensors);

tic;
for i = 1:numStepsTst % sliding window index through all time series
    for j = 1:noSensors % gather data for all sensors into one row
        tmp(:,j) = testSet(i:i+winWidth-2,j);
        % each row is a new time step. the values in the rows are the
        % outputs for each sensor, corresponding to the td / (winWidth-1)
        tstLbl(i,j) = testSet(winWidth,j);
    end
    % this contains the values of the input sliding window
    % for each sensor, concatenated into a single vector
    % every winWidth-1, a new sensor reading begins
    tstDat(i,:) = reshape(tmp, 1, noSensors*(winWidth-1));
end
toc;
disp('Finished processing test data');

%% ----- generate class labels  -----

trnLblBin = zeros(numStepsTrn, noSensors);
tstLblBin = zeros(numStepsTst, noSensors);

for i = 1:noSensors
    threshold = quantile(allSensors(:,i), quantilePercent);
    trnLblBin(:,i) = (trnLbl(:,i) > threshold);
    tstLblBin(:,i) = (tstLbl(:,i) > threshold);
end

% total high volume, percentage:
percentHighTrn = sum(sum(trnLblBin))/numel(trnLblBin)
percentHighTst = sum(sum(tstLblBin))/numel(tstLblBin)

clear testSet i j tmp allSensors;

%%

% save to disk with -v7.3 (HDFS) so we can load things partially with matfile command
save(strcat('traffic_data/VolumeData_small_Window_',num2str(winWidth),'.mat'), '-v7.3'); 

%% test for this script
% data = [1:19 ; (1:19)*10]
% construct_sliding_dataset
% trnDat
% trnLbl
% tstDat
% tstLbl
