model <- model.svm2
tune.prob <- tuneProb(model)

data.test <- read.csv('STATS_315A_HW3/loan_testx.csv')
data.test <- data.test[complete.cases(data.test),]
ind <- sapply(data.test, is.factor)
data.test[ind] <- lapply(data.test[ind], function(x) as.numeric(x)*1.0)
data.test.normal <- predict(preProc.train, data.test)
pred <- predict(model, newdata = data.test.normal, type = 'prob')
pred.class <- matrix(0, ncol = 1, nrow = nrow(data.test))
pred.class[pred[,2] > tune.prob$p] <- 1
defaultRate <- sum(as.numeric(pred.class))/length(pred.class)