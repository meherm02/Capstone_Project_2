######Loading the Iris data set#####
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)

iris_data <- read_csv("/Users/mehermankikar/Downloads/iris-species/Iris.csv")
iris_data
colnames(iris_data) <- c("ID", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
str(iris_data)

library(caret)

######Making test and train sets######
index <- createDataPartition(iris_data$Species, p = 0.50, list = FALSE)
iris_train <- iris_data[index,]
str(iris_train)
iris_test <- iris_data[-index,]
str(iris_test)

######Summarizing the data and Visualization#####
summary(iris_train)

x <- iris_train[,1:4]
y <- iris_train[,5]
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[i], main=names(iris_train)[i])
}

#####Creating Models#####

library(caret)
control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'
####Create LDA model
set.seed(101)
fit.lda <- train(Species~., data=iris_train, method='rf', 
                 trControl=control, metric = metric)
##### Create KNN Model
set.seed(101)
fit.knn <- train(Species~., data=iris_train, method='knn', 
                 trControl=control, metric=metric)
##### Create RF Model
set.seed(101)
fit.rf <- train(Species~., data=iris_train, method='ranger', 
                trControl=control, metric=metric)

###Combining the three models previously created
iris.results <- resamples(list(lda=fit.lda, knn=fit.knn,rf=fit.rf))

####Results#####
summary(iris.results)

######Making Predictions With the Models Created and Testing Accuracy
iris_prediction <- predict(fit.lda, iris_test)
confusionMatrix(table(iris_prediction, iris_test$Species))

