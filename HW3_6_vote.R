# Average multiple models.
model <- list()
model[[length(model) + 1]] <- model.qda
model[[length(model) + 1]] <- model.glmnet
model[[length(model) + 1]] <- model.svm0
model[[length(model) + 1]] <- model.svm1
model[[length(model) + 1]] <- model.knn

pred <- matrix(0, ncol = length(model), nrow = nrow(data.train.normal))
for (i in 1:length(model)){
    pred[,i] <- predict(model[[i]], newdata = data.train.normal)
}
pred <- pred - 1
pred.vote <- apply(pred, 1, mean)
pred.vote[pred.vote<0.5] <- 0
pred.vote[pred.vote>=0.5] <- 1
error.rate <- sum(abs(pred.vote - as.numeric(as.character(data.train$default))))/nrow(data.train)