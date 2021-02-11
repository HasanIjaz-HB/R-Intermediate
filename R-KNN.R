
# Use the following attributes as predictive attributes: "LB", "AC", "FM", "UC", "ASTV", "MSTV",
# "ALTV", "MLTV", "DL", "DS", "DP", "Width", "Min", "Max", "Nmax", "Nzeros", "Mode", "Mean",
# "Median", "Variance", and "Tendency". Use CLASS - FHR pattern class as target attribute. 
# The dataset is available on UCI Machine Learning Repository:
#   https://archive.ics.uci.edu/ml/datasets/Cardiotocography

##### READING DATA
library("readxl")
getwd()
cardio <- read_excel("CTG.xls", sheet = "Raw Data")
cardio <- cardio[-1, -c(1:6, 29:38)]
cardio <- cardio[, -c(24)]


##### DATA PRE-PROCESSING
summary(cardio)
cardio <- cardio[!is.na(cardio$CLASS),]
summary(cardio)
cardio <- cardio[,-12]
summary(cardio)
# Data Normalization
cardio_n <- cardio

# Second Way for Normalization (without using package)
min_max <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

cardio_n[, -22] <- sapply(cardio_n[, -22], min_max)
summary(cardio_n)
#save(cardio_n, file = "cardio.RData")

# Creating training and testing datasets
library(caret)
set.seed(1)
my_indexes <- caret::createDataPartition(y = cardio_n$CLASS, times = 1, p = .75, list = F)

training_ <- as.data.frame(cardio_n[my_indexes,])
test <- as.data.frame(cardio_n[-my_indexes,])

head(cardio_n$CLASS)
head(training$CLASS)
head(test$CLASS)

set.seed(1)
my_indexes2 <- caret::createDataPartition(y = training_$CLASS, times = 1, p = .80, list = F)

training <- as.data.frame(training_[my_indexes2,])
validation <- as.data.frame(training_[-my_indexes2,])
##### MODELING
# Applying kNN algorithm

#install.packages("class")
library(class)
set.seed(1)
(knn_predictions <- knn(train = training[, -22], test = test[, -22], cl = training[[22]], k = 3))

head(knn_predictions)
head(test[[22]])

#finding predictions of the model
(my_table <- table(knn_predictions, test[[22]], dnn=c("Predictions","Actual/Reference")))


#knn with probabilities
choosen_k <- 6
set.seed(1)
(knn_predictions <- knn(train = training[, -22], test = test[, -22], cl = training[[22]], k = choosen_k, prob ="T"))
probabilities <- attributes(knn_predictions)$prob
results <- cbind(test,knn_predictions,probabilities)

#knn with probabities using different k values
my_acc <- NULL
for(i in 1:20){
  set.seed(1)
  knn_predictions <- knn(train = training[, -22], test = validation[, -22], cl = training[[22]], k = i,prob =T)
  my_table <- confusionMatrix(data = knn_predictions, reference = validation[[22]], dnn = c("Predictions", "Actual/Reference"))
  my_acc[i] <- my_table$overall["Accuracy"]
}

plot(x=1:length(my_acc), y = my_acc, type = "o", col = "blue", xlab = "k", ylab = "Accuracy",
     main = "Performance Evaluation")
grid(NA, 5, lwd = 2)
text(x=1:length(my_acc), y = my_acc,  round(my_acc,2), cex=1, pos=3,col="red")

# when a new data is given
set.seed(3)
rnum <- sample(x=1 : nrow(test),size=1) # assume it is a new data
newdata <- test[rnum,-22]
knn_predictions_newdata <- knn(train = training[, -22], test = newdata, cl = training[[22]], k = choosen_k, prob ="T")
knn_predictions_newdata
attributes(knn_predictions_newdata)$prob

#Our predicted probability values is 0.6667 whereas the class lecture code has predicted value as 1.0
#Our probability table has less number of 1's as compared to the exercise done in class
# I choose k as 6 whereas 3 was used in class
#The class model obtained on the recorded class (no lecture) is more accurate