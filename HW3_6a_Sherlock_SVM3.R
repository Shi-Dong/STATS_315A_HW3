# Submitting SVM3 model to Sherlock to find the optimal parameters
# Polynomial SVM with 3rd degree polynomial.
train.control = trainControl(method = 'repeatedcv', number = 10,
                             repeats = 3, classProbs = T)
levels(data.train.normal$default) <- c('X0','X1')
library(kernlab)
svmGrid3 = expand.grid(degree = 3, scale = 1, C = cost)
model.svm3.sweep = train(default ~ recover + coll_fee + out_prncp + fees_rec + 
                             last_payment + prin_rec,
                         data = data.train.normal,
                         method = 'svmPoly', trControl = train.control,
                         tuneGrid = svmGrid3, metric = 'ROC')