# Sherlock Code for SVM with radial kernel.

# Data cleaning and pre-processing
data.train = read.csv('loan_train.csv')
data.train = data.train[complete.cases(data.train),]

library(caret)
library(glmnet)
# Convert data frame to numeric matrix
data.train.numeric = data.matrix(data.train)
# Compute the correlation of variables
corr = cor(data.train.numeric)
# Purge the variables that are not correlated to anything (including the response)
corr_max = apply(corr, 1, function(v) max(abs(v[-which.max(v)])))
variable.index = 1:ncol(data.train)
data.train = data.train[,-variable.index[corr_max<0.1]]
# Find the heavily correlated variables
corr_v = findCorrelation(corr, cutoff = .9)
# Purge the correlated variables
data.train = data.train[,-corr_v]
# Convert the response into a factor
data.train$default = as.factor(data.train$default)

# Apply 10-fold cross validation
train.control = trainControl(method = 'repeatedcv', number = 10,
                             repeats = 3)
# Train the model via SVM (radial kernel)
library(kernlab)
preProc = preProcess(data.train, method = c('center', 'scale'))
data.train.normal = predict(preProc, data.train)
svmGrid1 = expand.grid(degree = 2, scale = 1, C = cost)
model.svm1 = train(default ~ ., data = data.train.normal,
                   method = 'svmRadialCost', trControl = train.control,
                   tuneGrid = svmGrid1)
