tuneProb <- function(model){
    probGrid <- seq(0, 1, length = 200)
    
    pred.prob <- predict(model, newdata = data.train.normal, type = 'prob')
    e.min <- 1
    for (p in probGrid){
        pred <- matrix(0, ncol = 1, nrow = nrow(data.train.normal))
        pred[pred.prob[,2]>p] = 1
        e <- sum(abs(pred - as.numeric(as.character(data.train$default))))/nrow(data.train.normal)
        if (e<e.min){
            pred.opt <- pred
            e.min <- e
            p.opt <- p
        }
    }
    list(p = p.opt, pred = pred.opt, error = e.min)
}