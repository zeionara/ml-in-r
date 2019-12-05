library(caret)
library(cvAUC)
threshold <- 0.5

# Train the model
candies=read.csv("/home/zeio/ml-in-r/logistic-regression/candies-train.csv", header=T, sep=",")
filtered_candies <- candies[candies$competitorname != "Chewey Lemonhead Fruit Mix" & candies$competitorname != "Haribo Twin Snakes" & candies$competitorname != "Root Beer Barrels",]
model = glm(Y ~ chocolate + fruity + caramel + peanutyalmondy + nougat + crispedricewafer + hard + bar + pluribus + sugarpercent + pricepercent, family = binomial, data = filtered_candies)

# Test the model
test_candies = read.csv("/home/zeio/ml-in-r/logistic-regression/candies-test.csv", header=T, sep=",")
raw_predictions = predict(model, newdata = test_candies, type = "response")
predictions = ifelse(raw_predictions < threshold, 0, 1)
print(paste("Prediction for the Tootsie Roll Juniors to belong to the 1st class: ", toString(round(1 - raw_predictions[test_candies$competitorname == "Tootsie Roll Juniors"], 2))))
print(paste("Prediction for the Tootsie Roll Midgies to belong to the 1st class: ", toString(round(1 - raw_predictions[test_candies$competitorname == "Tootsie Roll Midgies"], 2))))
#print(raw_predictions[4])
print(paste("Precision: ", toString(round(precision(table(predictions, test_candies$Y)), 3))))
print(paste("Recall: ", toString(round(recall(table(predictions, test_candies$Y)), 3))))
print(paste("AUC: ", toString(round(AUC(raw_predictions, test_candies$Y), 3))))
