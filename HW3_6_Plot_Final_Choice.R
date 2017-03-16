# This script plots the final choice of the model probability threshold

# This function plots the training error and predicted default rate on the test set
#   as a function of probability threshold.
plotProbThresh <- function(modelList){
    pRange <- seq(0, 1, by = 0.01)
    trainError <- c()
    defaultRate <- c()
    for (p in pRange){
        trainError <- c(trainError,
                        predTest(modelList, 
                                 tuneProbTogether = T, 
                                 probThresh = p, 
                                 dataSet = 'train')$Training_Error)
        defaultRate<- c(defaultRate,
                        predTest(modelList,
                                 tuneProbTogether = T,
                                 probThresh = p,
                                 dataSet = 'test')$Default_Rate)
    }
    list(trainError = trainError, defaultRate = defaultRate, probRange = pRange)
}

p <- ggplot(kkk, aes(pRange)) +
     scale_color_manual(name = "", values = c('Training Error' = 'red',
                                              'Default Rate' = 'blue')) +
     geom_point(aes(y = trainError, color = 'Training Error')) +
     geom_point(aes(y = defaultRate, color = 'Default Rate')) +
     scale_y_continuous(breaks = c(0,0.07,0.25,0.5,0.75,1)) + 
     xlab('Threshold Probability') +
     ylab('Rate') +
     ggtitle('Training error and default rate for mixed models') +
     theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'),
           axis.title = element_text(size = 15),
           legend.text = element_text(size = 15)) +
     geom_hline(yintercept = 0.07, linetype = 'dashed')
     
print(p)