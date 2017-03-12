# This script finds the best parameter df for GAM model with smoothing splines
train.control = trainControl(method = 'repeatedcv', number = 10,
                             repeats = 3)
library(gam)
gamGrid <- expand.grid(df = 1:15)
model.gam.sweep <- train(default ~ ., data = data.train.normal,
                         method = 'gamSpline', trControl = train.control,
                         tuneGrid = gamGrid)
