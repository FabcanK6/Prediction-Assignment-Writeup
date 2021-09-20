# Prediction-Assignment-Writeup
I downloaded and loaded variable training_data and test_data respectively
training_data <- read.csv('training_data.csv', na.strings = c("NA", "#DIV/0!", ""))
test_data <- read.csv('testing_data.csv', na.strings = c("NA", "#DIV/0!", ""))
We first remove those data contains more than 95% of the observation to be NA. We filter out those records.
clnColumnIndex <- colSums(is.na(training_data))/nrow(training_data) < 0.95
clean_training_data <- training_data[,clnColumnIndex]
We then verifying we have removed NA correctly
colSums(is.na(clean_training_data))/nrow(clean_training_data)
colSums(is.na(clean_training_data))
We also remove col1 to col7 because they are not related to the model
clean_training_data <- clean_training_data[,-c(1:7)]
clean_test_data <- test_data[,-c(1:7)]
We then partition the training data into training set and cross validation set
inTrainIndex <- createDataPartition(clean_training_data$classe, p=0.75)[[1]]
training_training_data <- clean_training_data[inTrainIndex,]
training_crossval_data <- clean_training_data[-inTrainIndex,]
Now change the test data set into the same
allNames <- names(clean_training_data)
clean_test_data <- test_data[,allNames[1:52]]
decisionTreeMod <- train(classe ~., method='rpart', data=training_training_data)
Predict with decision tree and output the confusion matrix. It seems like the result of the model is not ideal.
decisionTreePrediction <- predict(decisionTreeMod, training_crossval_data)
confusionMatrix(training_crossval_data$classe, decisionTreePrediction)
plotting the decision tree
rpart.plot(decisionTreeMod$finalModel)
rfMod <- train(classe ~., method='rf', data=training_training_data, ntree=128)
rfPrediction <- predict(rfMod, training_crossval_data)
confusionMatrix(training_crossval_data$classe, rfPrediction)
Now we use it to predict the test set
predict(rfMod, clean_test_data)
As we can we from the result, the random forest algorithem far outperforms the decision tree in terms of accuracy. We are getting 99.25% in sample accuracy, while the decision tree gives us only nearly 50% in sample accuracy.
