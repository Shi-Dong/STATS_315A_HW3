# This function computes the default rate or training error rate on data.
# set = c('train', 'test') selects between training data and test data
#   if training data, the function returns training error rate.
#   if test data, the function returns default rate.
testModel <- function(model, tuneProb = TRUE, set = 'test'){
    if (tuneProb) tune.prob <- tuneProb(model)
    
    if (set=='test'){
        data.test <- read.csv('STATS_315A_HW3/loan_testx.csv')
        data.test <- data.test[complete.cases(data.test),]
        ind <- sapply(data.test, is.factor)
        data.test[ind] <- lapply(data.test[ind], function(x) as.numeric(x)*1.0)
        data.test.normal <- predict(preProc.train, data.test)
    }
    else{
        data.test <- data.train.normal
        data.test.normal <- data.train.normal
    }
    pred <- predict(model, newdata = data.test.normal, type = 'prob')
    pred.class <- matrix(0, ncol = 1, nrow = nrow(data.test))
    if (tuneProb) pred.class[pred[,2] > tune.prob$p] <- 1
    else pred.class[pred[,2] > 0.5] <- 1
    
    if (set=='test') sum(as.numeric(pred.class))/length(pred.class)
    else sum(abs(pred.class - as.numeric(data.test.normal$default) + 1))/length(pred.class)
}
