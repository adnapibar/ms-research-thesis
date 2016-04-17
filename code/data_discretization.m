%data discretization
clear;clc;
load('VolumeData_small.mat');

%%%%concatenating the time series:
%vdata=data(:,:)   %%%INCORRECT!!!
%plot(vdata(199,[1:96]))

%%%%Correct way to stack data
vdata=zeros(1084,2033*96);
for i=1:1084
   d=squeeze(data(i,:,:))';
   vdata(i,:)=d(:);    
end
%plot(vdata(199,[1:96]))  %Note the difference


%%%%Data discretization: 85% low, 15% high
ddata=vdata;
for i=1:1084
    i
    ddata(i,:)=vdata(i,:)>quantile(vdata(i,:),0.85);
end
hist(ddata(10,:))


%%%%Making training data, sliding window size 1:  use x(t) to predict
%%%%x(t+1)
train=vdata(:,1:end-1);
C=ddata(:,2:end);


%%%%Splitting data into train and test randomly
n=size(C,2);
ntrain=round(n*0.7);  %70% train - 30% test
tmp=randperm(n);
train_instances=tmp(1:ntrain);
test_instances=tmp(ntrain+1:end);

train_data=train(:,train_instances)';
train_label=C(:,train_instances)';

test_data=train(:,test_instances)';
test_label=C(:,test_instances)';

hist(test_label(:,1))

save discretized_data.mat train_data train_label test_data test_label train_instances test_instances

%%
%%%%Train a simple decision tree for sensor #ID
%sensorID=10;
%tc = fitctree(train_data,train_label(:,sensorID));
%pred=predict(tc,test_data);
%acc=sum(pred==test_label(:,1))/length(test_label(:,sensorID))
%rand_pred=zeros(1,length(test_data);
%rand_acc=sum(rand_pred==test_label(:,1))/length(test_label(:,sensorID))
% 0.8705 : no good!

%%%%traindata a simple decision tree for sensor #ID
sensorID=10;
tc = fitglm(train_data(:,sensorID), train_label(:,sensorID), 'Distribution','binomial', 'Link','logit');
pred=predict(tc,test_data(:,sensorID));

%%

acc=sum((pred>0.5)==test_label(:,1))/length(test_label(:,sensorID))
%
rand_pred=zeros(length(test_data),1);
rand_acc=sum(rand_pred==test_label(:,sensorID))/length(test_label(:,sensorID))
% 0.8705 : no good!

%
[tp,fp,~,au] = perfcurve(test_label(:,1)', pred, 1);

[c1,cm1] = confusion(test_label(:,1)', rand_pred');

[c2,cm2] = confusion(test_label(:,1)', pred');


