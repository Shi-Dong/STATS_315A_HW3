# This script contains a function that generates the predictions from test data
#   modelList is a list of all models
#   tuneProb is a logical parameter determining whether the tuned probability is used
#       If tuneProb == FALSE, the threshold probability is specified by probThresh
#   dataSet = c('train', 'test') specifies which dataset to use.
#       If dataSet == 'train', the returned value contains the training error rate.
#       If dataSet == 'test', the returned value contains the default rate.
predTest <- function(modelList,
                     tuneProbTogether = FALSE,
                     tuneProb = TRUE,
                     probThresh = NULL,
                     dataSet = 'test'){
    if (dataSet == 'test'){
        data.test <- read.csv('STATS_315A_HW3/loan_testx.csv')
        levels(data.test$employment) <- c('< 1', '1', '2', '3', '4', '5', '6', '7',
                                          '8', '9', '10+')
        data.test$employment[is.na(data.test$employment)] <- sample(levels(data.test$employment), 1)
        ind <- sapply(data.test, is.factor)
        data.test[ind] <- lapply(data.test[ind], function(x) as.numeric(x)*1.0)
        data.test.normal <- predict(preProc.train, data.test)
    }
    else{
        data.test <- data.train.normal
        data.test.normal <- data.train.normal
    }
    
    pred.all <- matrix(0, ncol = length(modelList), nrow = nrow(data.test.normal))
    for (i in 1:length(modelList)){
        pred.class <- matrix(0, ncol = 1, nrow = nrow(data.test.normal))
        model <- modelList[[i]]
        pred <- predict(model, newdata = data.test.normal, type = 'prob')
        if (!tuneProbTogether && tuneProb) pred.class[pred[,2] > tuneProb(model)$p] <- 1
        else if(!tuneProbTogether && !tuneProb) pred.class[pred[,2] > probThresh] <- 1
        else pred.class <- pred[,2]
        pred.all[,i] <- pred.class
    }
    pred.vote <- apply(pred.all, 1, mean)
    
    if (!tuneProbTogether){
        pred.vote[pred.vote<0.5] <- 0
        pred.vote[pred.vote>=0.5] <- 1
    }
    else if (dataSet == 'train' && is.null(probThresh)){
        error <- 1
        for (p in seq(0, 1, by = 0.01)){
            pred.try <- matrix(0, ncol = 1, nrow = length(pred.vote))
            pred.try[pred.vote>p] <- 1
            pred.try[pred.vote<=p] <- 0
            e <- predError(pred.try)$rate
            if (e<error){
                error <- e
                p.opt <- p
            }
        }
        pred.vote[pred.vote>p.opt] <- 1
        pred.vote[pred.vote<=p.opt] <- 0
    }
    else if (dataSet == 'train' && !is.null(probThresh)){
        pred.vote[pred.vote>probThresh] <- 1
        pred.vote[pred.vote<=probThresh] <- 0
        p.opt <- probThresh
    }
    else if (dataSet == 'test'){
        pred.vote[pred.vote>probThresh] <- 1
        pred.vote[pred.vote<=probThresh] <- 0
    }
    
    if (dataSet == 'test'){
        pred.vote[data.test$recover!=0] <- 1
        defaultRate <- sum(pred.vote)/length(pred.vote)
        list('Final_Pred' = pred.vote, 'Default_Rate' = defaultRate)
    }
    else{
        pred.vote[data.train$recover!=0] <- 1
        trainingError <- sum(abs(pred.vote - as.numeric(data.test.normal$default) + 1))/length(pred.vote)
        if (!tuneProbTogether) list('Final_Pred' = pred.vote, 'Training_Error' = trainingError)
        else list('Final_Pred' = pred.vote, 'Training_Error' = trainingError, 'Prob_Thresh' = p.opt)
    }
}

# The following function compares predicted values with observations
#   pred is a vector with the same number of rows as data.train.
#   every entry of pred is either 0 or 1.
predError <- function(pred){
    pred.error.index <- which(pred - as.numeric(data.train$default) + 1 != 0)
    pred.error <- data.train$default[pred.error.index]
    e <- length(pred.error) / length(pred)
    list(cases = pred.error, rate = e, index = pred.error.index)
}