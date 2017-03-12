# Data cleaning and pre-processing
dataFileName <- ifelse(file.exists('loan_train.csv'), 
                       'loan_train.csv',
                       'STATS_315A_HW3/loan_train.csv')
data.train <- read.csv(dataFileName)

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

# Create centered and scaled data
data.train.normal <- data.train

# Convert factors in data.train.normal to numeric
ind <- sapply(data.train.normal, is.factor)
data.train.normal[ind] <- lapply(data.train.normal[ind], 
                                 function(x) as.numeric(x)*1.0)
preProc.train = preProcess(data.train.normal[,-1], method = c('center', 'scale'))
data.train.normal[,-1] = predict(preProc.train, data.train.normal[,-1])
data.train.normal$default <- data.train$default
