# Submitting SVM2 model to Sherlock to find the optimal parameters
train.control = trainControl(method = 'repeatedcv', number = 10,
                             repeats = 3, classProbs = T)
library(kernlab)
svmGrid2 = expand.grid(degree = 2, scale = 1, C = cost)
model.svm2.sweep = train(default ~ recover + coll_fee + out_prncp + fees_rec + 
                                   last_payment + prin_rec,
                         data = data.train.normal,
                         method = 'svmPoly', trControl = train.control,
                         tuneGrid = svmGrid2, metric = 'ROC')