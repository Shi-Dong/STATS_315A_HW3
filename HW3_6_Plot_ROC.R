computeROC <- function(modelList){
    pRange <- seq(0, 1, by = 0.002)
    N0 <- nrow(data.train[data.train$default == 0, ])
    N1 <- nrow(data.train[data.train$default == 1, ])
    sens <- c(0)
    spec <- c(0)
    for (p in pRange){
        kkk <- predTest(modelList,
                        tuneProbTogether = T,
                        probThresh = p,
                        dataSet = 'train')
        pred <- kkk$Final_Pred
        error <- predError(pred)
        n0 <- length(error[error$cases == 0])
        n1 <- length(error[error$cases == 1])
        sens <- c(sens, (N1 - n1)/N1)
        spec <- c(spec, n0/N0)
    }
    sens <- c(sens, 1)
    spec <- c(spec, 1)
    roc <- data.frame(sens, spec)
}

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

roc.glmnet <- computeROC(list(model.glmnet))
roc.qda <- computeROC(list(model.qda))
roc.gam <- computeROC(list(model.gam))
roc.svm1 <- computeROC(list(model.svm1))
roc.svm2 <- computeROC(list(model.svm2))
roc.svm3 <- computeROC(list(model.svm3))
roc.knn <- computeROC(list(model.knn))
Nmodel <- 7
colValue <- ggplotColours(n = Nmodel)
names(colValue) <- c('glmnet', 'QDA', 'GAM', 'SVM (radial)',
                     'SVM (2nd degree)', 'SVM (3rd degree)',
                     'k-Nearest Neighbors')
plot <- ggplot(NULL, aes(spec, sens)) +
        geom_line(data = roc.glmnet, aes(color = 'glmnet'), size = 1.5) +
        geom_line(data = roc.qda, aes(color = 'QDA'), size = 1.5) +
        geom_line(data = roc.gam, aes(color = 'GAM'), size = 1.5) +
        geom_line(data = roc.svm1, aes(color = 'SVM (radial)'), size = 1.5) +
        geom_line(data = roc.svm2, aes(color = 'SVM (2nd degree)'), size = 1.5) +
        geom_line(data = roc.svm3, aes(color = 'SVM (3rd degree)'), size = 1.5) +
        geom_line(data = roc.knn, aes(color = 'k-Nearest Neighbors'), size = 1.5) +
        scale_color_manual(name = "", values = colValue) +
        xlab('False Positive Rate') +
        ylab('True Positive Rate') +
        ggtitle('ROC curves for individual models') +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
              axis.title = element_text(size = 15),
              legend.text = element_text(size = 15)) +
        ylim(0,1)
print(plot)