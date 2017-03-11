model <- model.qda

data.test <- read.csv('STATS_315A_HW3/loan_testx.csv')
data.test <- data.test[complete.cases(data.test),]
ind <- sapply(data.test, is.factor)
data.test[ind] <- lapply(data.test[ind], function(x) as.numeric(x)*1.0)
data.test.normal <- predict(preProc.train, data.test)
pred <- predict(model, newdata = data.test.normal)
defaultRate <- sum(as.numeric(pred) - 1)/length(pred)