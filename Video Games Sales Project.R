if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(broom)
library(randomForest)

setwd("D:/R files/My Own Project")
data <- read.csv("vgsales.csv")

colnames(data)

######################################################
# Keeping only relevant columns and removing rows having NA values
data <- data[c('Platform', 'Year', 'Genre', 'Publisher', 'Global_Sales')] %>% na.omit()

# Since data size is too large, only 50% will be used
set.seed(1, sample.kind="Rounding")
data_to_use_index <- createDataPartition(y = data$Global_Sales, times = 1, p = 0.5, list = FALSE, )
data_to_use <- data[data_to_use_index,]

# Mutating Platform, Genre and Publisher columns as categorical data
data_to_use <- data_to_use %>% mutate(Platform = factor(Platform), Genre = factor(Genre), Publisher = factor(Publisher))

# Test set will be 30% of data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data_to_use$Global_Sales, times = 1, p = 0.3, list = FALSE, )
train_set <- data_to_use[-test_index,]
temp <- data_to_use[test_index,]


# Making sure Platform, Genre and Publisher in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "Platform") %>%
  semi_join(train_set, by = "Genre") %>%
  semi_join(train_set, by = "Publisher")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

######################################################
#Visualizing data
data_to_use %>% ggplot(aes(Genre, fill = Platform)) + geom_bar(position = position_dodge())

data_to_use %>% group_by(Genre) %>% summarise(Sales = sum(Global_Sales)) %>% slice_max(order_by =  Sales, n = 10) %>% 
  ggplot(aes(reorder(Genre, -Sales), Sales)) + geom_col() +  scale_x_discrete(guide = guide_axis(n.dodge = 2))

######################################################

#Creating and training a k-Nearest Neighbors model
fit_knn <- train(Genre ~ .,  method = "knn", tuneGrid = data.frame(k = seq(39, 42, 1)), data = train_set)
ggplot(fit_knn)

prediction_knn <- predict(fit_knn, newdata = test_set)

accuracy_knn <- mean(prediction_knn == test_set$Genre)

accuracies <- data_frame(Method = "kNN Model", Accuracy = accuracy_knn)

######################################################

#Creating and training a Decision Tree model
fit_tree <- train(Genre ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.01, len = 30)), data = train_set)
ggplot(fit_tree)

prediction_tree <- predict(fit_tree, newdata = test_set)

accuracy_tree <- mean(prediction_tree == test_set$Genre)

accuracies <- bind_rows(accuracies,
                          data_frame(Method="Decision Tree Model",
                                     RMSE = accuracy_tree ))

######################################################

#Creating and training a Random Forest model
fit_rf <- train(Genre ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(2, 10)),
                    data = train_set)
ggplot(fit_rf)

prediction_rf <- predict(fit_rf, newdata = test_set)

accuracy_rf <- mean(prediction_rf == test_set$Genre)

accuracies <- bind_rows(accuracies,
                        data_frame(Method="Random Forest Model",
                                   RMSE = accuracy_rf ))
