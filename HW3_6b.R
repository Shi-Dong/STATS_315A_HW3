# Classification after variable selection

# Data cleaning and pre-processing
data.train = read.csv('STATS_315A_HW3/loan_train.csv')
data.train = data.train[complete.cases(data.train),]

library(caret)
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
### Train the model via SVM (linear kernel) to select variables
library(e1071)
model.svm0 = train(default ~ ., data = data.train,
                   method = 'svmLinear2', trControl = train.control)
var.imp <- varImp(model.svm0)
var.imp <- var.imp$importance[order(var.imp$importance$X0, decreasing = T),]
library(kernlab)
for (M in 10:23){
    var.selected <- which(names(data.train) %in% row.names(var.imp)[1:M])
    data.train.selected <- data.train[,c(1,var.selected)]
    
    ### Train the model via SVM (polynomial kernel)
    # preProc = preProcess(data.train.selected, method = c('center', 'scale'))
    # data.train.normal = predict(preProc, data.train.selected)
    svmGrid1 = expand.grid(degree = 2, scale = 1, C = 0.01)
    model.svm1 = train(default ~ ., data = data.train.selected,
                       method = 'svmPoly', trControl = train.control,
                       tuneGrid = svmGrid1)
    print(paste0('Number of included variables: ', as.character(M)))
    print(model.svm1)
}


