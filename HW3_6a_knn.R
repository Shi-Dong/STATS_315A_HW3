# This script finds the best parameter k for k-NN model
knnGrid = expand.grid(k = 1:15)
model.knn.sweep  = train(default ~ out_prncp + fees_rec + amount + interest + prin_rec + status,
                         data = data.train.normal,
                         method = 'knn', trControl = train.control,
                         tuneGrid = knnGrid)