# This script finds the best parameter df for GAM model with smoothing splines
library(gam)
gamGrid <- expand.grid(df = c(1,15))
model.gam.sweep <- train(default ~ ., data = data.train.normal,
                         method = 'gamSpline', trControl = train.control,
                         tuneGrid = gamGrid)
