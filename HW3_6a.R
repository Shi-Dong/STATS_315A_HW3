# This script trains all models with pre-determined parameters.
# Every model is fed with data.train.normal

# Self-defined metric function
optThreshold <- function(data, lev = NULL, model = NULL){
    min.error <- 1
    opt.threshold <- 0
    for (t in seq(0, 1, by = 0.01)){
        pred <- matrix(0, ncol = 1, nrow = nrow(data))
        pred[data$X1 > t] <- 1
        e <- sum(abs(pred - as.numeric(data$obs) + 1))/nrow(data)
        if (e < min.error){
            min.error <- e
            opt.threshold <- t
        }
    }
    c('Max_Accuracy' = 1-min.error, 'Opt_Threshold' = opt.threshold)
}


# Apply 10-fold cross validation
train.control = trainControl(method = 'repeatedcv', number = 10,
                             repeats = 3, classProbs = T,
                             summaryFunction = optThreshold,
                             # down-sampling the data
                             sampling = 'up')
# Relabel the data
levels(data.train.normal$default) <- c('X0', 'X1')

### Train the model via glmnet
glmGrid = expand.grid(alpha = 1, lambda = 10^seq(-7, -1, by = 1))
model.glmnet = train(default ~ ., data = data.train.normal,
                     method = 'glmnet', trControl = train.control,
                     tuneGrid = glmGrid, 
                     metric = 'Max_Accuracy')
print('glmnet training complete!')

### Train the model via SVM (linear kernel)
library(e1071)
svmGrid0 = expand.grid(cost = 1)
model.svm0 = train(default ~ ., data = data.train.normal,
                   method = 'svmLinear2', trControl = train.control,
                   tuneGrid = svmGrid0, metric = 'Max_Accuracy')
print('linear SVM training complete!')

### Train the model via SVM (radial kernel)
library(kernlab)
svmGrid1 = expand.grid(C = 9)
set.seed(825)
model.svm1 = train(default ~ ., data = data.train.normal,
                   method = 'svmRadialCost', trControl = train.control,
                   tuneGrid = svmGrid1, metric = 'Max_Accuracy')
print('radial SVM training complete!')

### Train the model via k-NN
knnGrid = expand.grid(k = 5)
model.knn  = train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status,
                   data = data.train.normal,
                   method = 'knn', trControl = train.control,
                   tuneGrid = knnGrid, metric = 'Max_Accuracy')
print('k-NN training complete!')

### Train the model via QDA
set.seed(825)
model.qda <- train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status, 
                   data = data.train.normal,
                   method = 'qda', trControl = train.control, metric = 'Max_Accuracy')
print('QDA training complete!')

### Train the model via GAM with smoothing splines
library(gam)
gamGrid <- expand.grid(df = 1)
model.gam <- train(default ~ ., data = data.train.normal,
                   method = 'gamSpline', trControl = train.control,
                   tuneGrid = gamGrid, metric = 'Max_Accuracy')
print('GAM with smoothing splines training complete!')


### Train the model via SVM (2nd degree polynomial kernel)
library(kernlab)
svmGrid2 = expand.grid(degree = 2, scale = 1, C = 77.4)
model.svm2 = train(default ~ recover + coll_fee + interest + quality + int_rec + req,
                   data = data.train.normal,
                   method = 'svmPoly', trControl = train.control,
                   tuneGrid = svmGrid2, metric = 'Max_Accuracy')
print('2nd degree polynomial SVM training complete!')

### Train the model via SVM (3rd degree polynomial kernel)
library(kernlab)
svmGrid3 = expand.grid(degree = 3, scale = 1, C = 3.59)
model.svm3 = train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status,
                   data = data.train.normal,
                   method = 'svmPoly', trControl = train.control,
                   tuneGrid = svmGrid3, metric = 'Max_Accuracy')
print('3rd degree polynomial SVM training complete!')