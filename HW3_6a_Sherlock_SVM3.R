# Submitting SVM3 model to Sherlock to find the optimal parameters
# Polynomial SVM with 3rd degree polynomial.
train.control = trainControl(method = 'repeatedcv', number = 10,
                             repeats = 3)
library(kernlab)
svmGrid3 = expand.grid(degree = 3, scale = 1, C = cost)
model.svm3.sweep = train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status,
                         data = data.train.normal,
                         method = 'svmPoly', trControl = train.control,
                         tuneGrid = svmGrid3)