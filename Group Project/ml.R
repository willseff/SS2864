install.packages('caret')
install.packages('rpart.plot')
library(caret)
library(rpart.plot)

training_index <- createDataPartition(Boston$crim, p = 0.8, list = FALSE)

# data partition
data_training <- Boston[training_index, ]
data_testing <- Boston[-training_index, ]


cart_original <- train(x = data_training[, c(3, 8, 9, 10)],
                       y = data_training$medv,
                       method = "rpart",
                       tuneLength = 10,
                       metric = "RMSE",
                       # trainControl: control parameters for train
                       # trainControl(method,
                       #              number,
                       #              repeats,
                       #              classProbs,
                       #              summaryFunction,
                       #              sampling)
                       # method: the resampling method
                       # number: the number of folds
                       # classProbs: a logical; should class probabilities be computed for classification models (along with predicted values) in each resample
                       # summaryFunction: a function to compute performance metrics across resamples
                       # sampling: a single character value describing the type of additional sampling that is conducted after resampling (usually to resolve class imbalances)
                       )

importance_cart_original <- varImp(cart_original, scale = FALSE)
plot(importance_cart_original, main = "Variable Importance in Original CART Model")
rpart.plot(cart_original$finalModel, main = "Original CART Model", box.palette = "Reds")
