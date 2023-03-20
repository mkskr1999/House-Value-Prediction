df <- read.csv("C:/Users/mksai/OneDrive/Desktop/ML Project/House-Value-Prediction/Data/Housing_Data_5.csv")
df <- subset(df, select = -c(X))
head(df,10)
str(df)
breaks <- c(0, 400000, 800000, 5000000)
df$range_1 <- cut(df$zestimate, breaks = breaks, labels = c("Low", "Medium", "High"))
(head(df,10))
# Splitting dataset into training and testing sets
library(caret)
set.seed(321)
r<-df[df$range_1, ]
trainIndex <- sample(nrow(r), 0.7 * nrow(r))
training <- df[trainIndex,]
testing <- df[-trainIndex,]
training$range_1 <- factor(training$range_1)
testing$range_1 <- factor(testing$range_1)
library(e1071)
model <- naiveBayes(range_1 ~ ., data = training)
(model)
# laplace smoothing
#model <- naiveBayes(home_team_result ~ ., data = training,laplace=1)
# Making predictions on testing set
predictions <- predict(model, newdata = testing)
(predictions)
confusionMatrix(predictions, testing$range_1)

nb_cm<- confusionMatrix(predictions, testing$range_1)

plot(nb_cm$table, col = nb_cm$byClass, 
     main = "Naive Bayes Confusion Matrix", 
     xlab = "Predicted Species", ylab = "Actual Species")

library(ggplot2)
ggplot(data = as.data.frame(nb_cm$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Freq), size = 15, color = "black") +
  labs(title = "Naive Bayes Confusion Matrix",
       x = "Actual Species", y = "Predicted Species")