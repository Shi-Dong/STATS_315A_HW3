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
# Train the model via SVM (linear kernel)
library(e1071)
data.train.normal = preProcess(data.train, method = c('center', 'scale'))
data.train.normal$default = data.train$default
svmGrid0 = expand.grid(cost = 10^seq(svmGridStart , svmGridEnd, length = 5))
model.svm0 = train(default ~ ., data = data.train,
                   method = 'svmLinear2', trControl = train.control,
                   tuneGrid = svmGrid0)
