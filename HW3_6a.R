# This script trains all models with pre-determined parameters.
# Every model is fed with data.train.normal

# Apply 10-fold cross validation
train.control = trainControl(method = 'repeatedcv', number = 10,
                             repeats = 3)

### Train the model via glmnet
glmGrid = expand.grid(alpha = 1, lambda = 0.0003268627)
model.glmnet = train(default ~ ., data = data.train.normal,
                     method = 'glmnet', trControl = train.control,
                     tuneGrid = glmGrid)
print('glmnet training complete!')

### Train the model via SVM (linear kernel)
library(e1071)
svmGrid0 = expand.grid(cost = 1)
model.svm0 = train(default ~ ., data = data.train.normal,
                   method = 'svmLinear2', trControl = train.control,
                   tuneGrid = svmGrid0)
print('linear SVM training complete!')

### Train the model via SVM (radial kernel)
library(kernlab)
svmGrid1 = expand.grid(C = 9)
model.svm1 = train(default ~ ., data = data.train.normal,
                   method = 'svmRadialCost', trControl = train.control,
                   tuneGrid = svmGrid1)
print('radial SVM training complete!')

### Train the model via SVM (polynomial kernel)
library(kernlab)
svmGrid2 = expand.grid(degree = 2, scale = 1, C = 100)
model.svm2 = train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status,
                   data = data.train.normal,
                   method = 'svmPoly', trControl = train.control,
                   tuneGrid = svmGrid2)
print('polynomial SVM training complete!')

### Train the model via k-NN
knnGrid = expand.grid(k = 5)
model.knn  = train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status,
                   data = data.train.normal,
                   method = 'knn', trControl = train.control,
                   tuneGrid = knnGrid)
print('k-NN training complete!')

### Train the model via QDA
model.qda <- train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status, 
                   data = data.train.normal,
                   method = 'qda', trControl = train.control)
print('QDA training complete!')

### Train the model via GAM with smoothing splines
library(gam)
gamGrid <- expand.grid(df = 1)
model.gam <- train(default ~ ., data = data.train.normal,
                   method = 'gamSpline', trControl = train.control,
                   tuneGrid = gamGrid)
print('GAM with smoothing splines training complete!')
