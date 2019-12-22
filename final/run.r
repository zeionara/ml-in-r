library(class)
set.seed(2019)
threshold <- 0.5

stars <- read.csv('/home/zeio/ml-in-r/final/stars.csv', sep=',', header=T)
test_stars <- read.csv('/home/zeio/ml-in-r/final/test-stars.csv', sep=',', header=T)
logistic_model = glm(Y ~ ., family = binomial, data = stars)
prediction = predict(logistic_model, newdata = test_stars, type = 'response')
print(paste("Point belongs to the 1st class with probability", toString(prediction), "according to the logistic regression classifier"))
print(paste("Point belongs to the class", toString(ifelse(prediction < threshold, 0, 1)), "according to the linear regression classifier"))

knn_prediction <- knn(stars,test_stars,cl=stars$Y,k=5)
print(paste("Point belongs to the class", toString(knn_prediction), "according to the KNN classifier"))

stars_vectors = stars[,c('MIP', 'STDIP', 'EKIP', 'SIP', 'MC', 'STDC', 'EKC', 'SC')]
test_star_vector =as.numeric(test_stars[1,c('MIP', 'STDIP', 'EKIP', 'SIP', 'MC', 'STDC', 'EKC', 'SC')])

distances <- c()
for (i in 1:nrow(stars_vectors)){
	distances[i] <- dist(rbind(as.numeric(stars_vectors[i,]), test_star_vector))
}
print(paste("Euclidean distance to the nearest neighbour is equal to", toString(round(min(distances), digits = 3))))
