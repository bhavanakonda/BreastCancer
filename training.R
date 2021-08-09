options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(gam)
data(brca)
cm <-colMeans(brca$x)
csd <- colSds(brca$x) 
nsx <- sweep(sweep(brca$x, 2, cm),2,csd,FUN = '/')
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- nsx[test_index,]
test_y <- brca$y[test_index]
train_x <- nsx[-test_index,]
train_y <- brca$y[-test_index]

#k_means Training Algorithm
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
set.seed(3, sample.kind = "Rounding") 
k <- kmeans(train_x,centers = 2)
pt <- predict_kmeans(test_x,k)
mean(ifelse(pt == 1,test_y == 'B',test_y == 'M'))
y_km <- ifelse(pt == 1,'B','M')
sum(pt==2 & test_y == 'M')/sum(test_y =='M')

#Logistic Regression Algorithm
fit_lm <- train(data.frame(train_x),factor(train_y), method = 'glm')
y_lm <- predict(fit_lm,test_x)
mean(y_lm == test_y)

#LDA Algorithm
set.seed(1, sample.kind = "Rounding") 
fit_lda <- train(data.frame(train_x),factor(train_y), method = 'lda')
y_lda <- predict(fit_lda,test_x)
mean(y_lda == test_y)

#QDA Algorithm
set.seed(1, sample.kind = "Rounding") 
fit_qda <- train(data.frame(train_x),factor(train_y), method = 'qda')
y_qda <- predict(fit_qda,test_x)
mean(y_qda == test_y)

#Loess Algorithm
set.seed(5, sample.kind = "Rounding") # simulate R 3.5
train_loess <- train(train_x, train_y, method = "gamLoess")

y_gam <- predict(train_loess, test_x)
mean(y_gam == test_y)

#knn model
set.seed(7, sample.kind = "Rounding") 
fit_knn <- train(data.frame(train_x),factor(train_y), method = 'knn', tuneGrid = data.frame(k = seq(3,21,2)))
fit_knn$finalModel$k
plot(fit_knn)
y_knn <- predict(fit_knn,test_x)
mean(y_knn == test_y)

#random Forrest model
set.seed(9, sample.kind = "Rounding") 
fit_rf <- train(data.frame(train_x),factor(train_y), method = 'rf', tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),importance = TRUE)
fit_rf$finalModel$mtry
varImp(fit_rf)
plot(fit_rf)
y_rf <- predict(fit_rf,test_x)
mean(y_rf == test_y)

#ensemble of above models
y_hat <- ifelse(sum(y_km == 'B' &
                    y_lm == 'B' &
                    y_lda == 'B' &
                    y_qda == 'B' &
                    y_gam == 'B' &
                    y_knn == 'B' &
                    y_rf == 'B')>3,'B','M')
mean(y_hat == test_y)
