getwd()
setwd("/Users/stephenkwok/Desktop/Data101R/test")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("Metrics")
library(rpart)
library(rpart.plot)
library(Metrics)

df <- read.csv("apple_quality 2.csv")
df <- na.omit(df)


# Categorical variables creation. I broke them up for appropriate distribution
df$size_class <- cut(df$Size,
                     breaks = c(-8, -2, 0, 1, Inf),
                     labels = c("Tiny", "Small", "Medium", "Large"),
                     right = FALSE)

df$crunchiness_class = cut(df$Crunchiness,
                       breaks = c(-7, 0.1, 1, 2, Inf),
                       labels = c("Mushy", "Soft", "Firm", "Crunchy"),
                       right = FALSE)

df$sweetness_class = cut(df$Sweetness,
                        breaks = c(-7, 0, Inf),
                        labels = c("Unsweet", "Sweet"),
                        right = FALSE)


# Scramble/Randomize the dataset
v <- sample(1:nrow(df))
v[1:5]
df_scrambled = df[v, ]

# Cross Validation
n <- 100
df_train = df_scrambled[nrow(df_scrambled)-n:nrow(df_scrambled), ]
df_test = df_scrambled[1:n,]

# Numerical Variable Prediction: Ripeness

# TRAINING SET

# Benchmark for rpart : Roughly 6.56987039710093
df_benchmark = rpart(Ripeness ~ ., data = df_train, method = "anova")
df_benchmark_predictions = predict(df_benchmark, df_train)
df_benchmark_mse = mse(df_train$Crunchiness, df_benchmark_predictions)
print(paste("MSE: ", df_benchmark_mse))
cat("Benchmark MSE: 6.56987039710093")

# Benchmark for lm
# When you run this code it LAGS (idk why). Roughly 6.79676977287847
df_benchmark_model = lm(Ripeness ~ ., data = df_train)
df_benchmark_model_predictions = predict(df_benchmark_model, df_train)
df_benchmark_model_mse = mse(df_train$Crunchiness, df_benchmark_model_predictions)
print(paste("MSE: ", df_benchmark_model_mse))
cat("Benchmark MSE: 6.79676977287847")

# rpart models b/c rpart has a lower MSE than lm
# improving my prediction with rpart + subsets
tree1 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness  + crunchiness_class,
              data = df_train[df_train$size_class == 'Tiny' & df_train$Quality == 'Good', ], method = "anova")
tree2 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness  + crunchiness_class,
              data = df_train[df_train$size_class == 'Small' & df_train$Quality == 'Good', ], method = "anova")
tree3 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness  + crunchiness_class,
              data = df_train[df_train$size_class == 'Medium' & df_train$Quality == 'Good', ], method = "anova")
tree4 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Large' & df_train$Quality == 'Good', ], method = "anova")

tree5 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness  + crunchiness_class,
              data = df_train[df_train$size_class == 'Tiny' & df_train$Quality == 'Bad',], method = "anova")
tree6 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Small' & df_train$Quality == 'Bad', ], method = "anova")
tree7 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness  + crunchiness_class,
              data = df_train[df_train$size_class == 'Medium' & df_train$Quality == 'Bad', ], method = "anova")
tree8 = rpart(Ripeness ~ Size + Weight + Sweetness + Crunchiness + Juiciness  + crunchiness_class,
              data = df_train[df_train$size_class == 'Large' & df_train$Quality == 'Bad', ], method = "anova")

# predict rpart models
treepred1 = predict(tree1, df_train[df_train$size_class == "Tiny" & df_train$Quality == 'Good', ])
treepred2 = predict(tree2, df_train[df_train$size_class == "Small" & df_train$Quality == 'Good', ])
treepred3 = predict(tree3, df_train[df_train$size_class == "Medium" & df_train$Quality == 'Good', ])
treepred4 = predict(tree4, df_train[df_train$size_class == "Large" & df_train$Quality == 'Good', ])

treepred5 = predict(tree5, df_train[df_train$size_class == "Tiny" & df_train$Quality == 'Bad', ])
treepred6 = predict(tree6, df_train[df_train$size_class == "Small" & df_train$Quality == 'Bad', ])
treepred7 = predict(tree7, df_train[df_train$size_class == "Medium" & df_train$Quality == 'Bad', ])
treepred8 = predict(tree8, df_train[df_train$size_class == "Large" & df_train$Quality == 'Bad', ])

# decision model
df_tree_decision = rep(0, nrow(df))
df_tree_decision[df_train$size_class == 'Tiny' & df_train$Quality == 'Good'] = treepred1
df_tree_decision[df_train$size_class == 'Small' & df_train$Quality == 'Good'] = treepred2
df_tree_decision[df_train$size_class == 'Medium' & df_train$Quality == 'Good'] = treepred3
df_tree_decision[df_train$size_class == 'Large' & df_train$Quality == 'Good'] = treepred4

df_tree_decision[df_train$size_class == 'Tiny' & df_train$Quality == 'Bad'] = treepred5
df_tree_decision[df_train$size_class == 'Small' & df_train$Quality == 'Bad'] = treepred6
df_tree_decision[df_train$size_class == 'Medium' & df_train$Quality == 'Bad'] = treepred7
df_tree_decision[df_train$size_class == 'Large' & df_train$Quality == 'Bad'] = treepred8

# Accuracy of the prediction model using MSE
df_tree_mse = mse(df_train$Ripeness, df_tree_decision)
print(paste("MSE:", df_tree_mse)) # Roughly 3.77329118671437
cat("Improved MSE: 3.77329118671437")
df_benchmark_mse/ df_tree_mse # 74.8332% better MSE than benchmark
cat("Benchmark compared to mine is 1.748332, meaning mine is 74.8332% better.")


# TESTING SET

# Benchmark for rpart: Roughly 6.85048078704212
df_benchmark_tree = rpart(Ripeness ~ ., data = df_test, method = "anova")
df_benchmark_tree_predictions = predict(df_benchmark_tree, df_test)
df_benchmark_tree_mse = mse(df_test$Crunchiness, df_benchmark_tree_predictions)
print(paste("MSE: ", df_benchmark_tree_mse))
cat("Benchmark MSE: 6.85048078704212")

# Benchmark for lm: Roughlt 6.982134595234
df_benchmark_model = lm(Ripeness ~ ., data = df_test)
df_benchmark_model_predictions = predict(df_benchmark_model, df_test)
df_benchmark_model_mse = mse(df_test$Crunchiness, df_benchmark_model_predictions)
print(paste("MSE: ", df_benchmark_model_mse))
cat("Benchmark MSE: 6.982134595234")

# predict rpart models
treepred1 = predict(tree1, df_test[df_test$size_class == "Tiny" & df_test$Quality == 'Good', ])
treepred2 = predict(tree2, df_test[df_test$size_class == "Small" & df_test$Quality == 'Good', ])
treepred3 = predict(tree3, df_test[df_test$size_class == "Medium" & df_test$Quality == 'Good', ])
treepred4 = predict(tree4, df_test[df_test$size_class == "Large" & df_test$Quality == 'Good', ])

treepred5 = predict(tree5, df_test[df_test$size_class == "Tiny" & df_test$Quality == 'Bad', ])
treepred6 = predict(tree6, df_test[df_test$size_class == "Small" & df_test$Quality == 'Bad', ])
treepred7 = predict(tree7, df_test[df_test$size_class == "Medium" & df_test$Quality == 'Bad', ])
treepred8 = predict(tree8, df_test[df_test$size_class == "Large" & df_test$Quality == 'Bad', ])

# decision model
df_tree_decision = rep(0, nrow(df))
df_tree_decision[df_test$size_class == 'Tiny' & df_test$Quality == 'Good'] = treepred1
df_tree_decision[df_test$size_class == 'Small' & df_test$Quality == 'Good'] = treepred2
df_tree_decision[df_test$size_class == 'Medium' & df_test$Quality == 'Good'] = treepred3
df_tree_decision[df_test$size_class == 'Large' & df_test$Quality == 'Good'] = treepred4

df_tree_decision[df_test$size_class == 'Tiny' & df_test$Quality == 'Bad'] = treepred5
df_tree_decision[df_test$size_class == 'Small' & df_test$Quality == 'Bad'] = treepred6
df_tree_decision[df_test$size_class == 'Medium' & df_test$Quality == 'Bad'] = treepred7
df_tree_decision[df_test$size_class == 'Large' & df_test$Quality == 'Bad'] = treepred8

# Accuracy of the prediction model using MSE
df_tree_mse = mse(df_test$Ripeness, df_tree_decision)
print(paste("MSE:", df_tree_mse)) # Roughly 3.88200291673678
cat("Improved MSE: 3.88200291673678")
df_benchmark_tree_mse/df_tree_mse # 76.4677% better MSE than benchmark
cat("Benchmark compared to mine is 1.764677, meaning mine is 76.4677% better.")


# Categorical Variable Prediction: Quality
tree <- rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + Ripeness, data = df_train, method = "class")
rpart.plot(tree)
tree_predictions <- rep('Bad', nrow(df_train))
tree_predictions <- predict(tree, newdata = df_train, type = "class")
df_train$prediction = tree_predictions
tree_mse = mean(df_train$Quality == df_train$prediction)
print(paste("Accuracy:", tree_mse))
cat("Accuracy: 0.732820512820513")

# using rpart models to improve accuracy
tree1 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Tiny' & df_train$sweetness_class == 'Unsweet', ], method = "class")
tree2 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Small' & df_train$sweetness_class == 'Unsweet', ], method = "class")
tree3 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Medium' & df_train$sweetness_class == 'Unsweet', ], method = "class")
tree4 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Large' & df_train$sweetness_class == 'Unsweet', ], method = "class")

tree5 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Tiny' & df_train$sweetness_class == 'Sweet', ], method = "class")
tree6 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Small' & df_train$sweetness_class == 'Sweet', ], method = "class")
tree7 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Medium' & df_train$sweetness_class == 'Sweet', ], method = "class")
tree8 = rpart(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + crunchiness_class,
              data = df_train[df_train$size_class == 'Large' & df_train$sweetness_class == 'Sweet', ], method = "class")

# put into predict functions
treepred1 = predict(tree1, df_train[df_train$size_class == "Tiny" & df_train$sweetness_class == 'Unsweet', ], type = "class")
treepred2 = predict(tree2, df_train[df_train$size_class == "Small" & df_train$sweetness_class == 'Unsweet', ], type = "class")
treepred3 = predict(tree3, df_train[df_train$size_class == "Medium" & df_train$sweetness_class == 'Unsweet', ], type = "class")
treepred4 = predict(tree4, df_train[df_train$size_class == "Large" & df_train$sweetness_class == 'Unsweet', ], type = "class")

treepred5 = predict(tree5, df_train[df_train$size_class == "Tiny" & df_train$sweetness_class == 'Sweet', ], type = "class")
treepred6 = predict(tree6, df_train[df_train$size_class == "Small" & df_train$sweetness_class == 'Sweet', ], type = "class")
treepred7 = predict(tree7, df_train[df_train$size_class == "Medium" & df_train$sweetness_class == 'Sweet', ], type = "class")
treepred8 = predict(tree8, df_train[df_train$size_class == "Large" & df_train$sweetness_class == 'Sweet', ], type = "class")

# fix labels so it is not 1 and 2. Make it good and bad
class_labels = c('bad', 'good')
treepred1_labeled = class_labels[treepred1]
treepred2_labeled = class_labels[treepred2]
treepred3_labeled = class_labels[treepred3]
treepred4_labeled = class_labels[treepred4]

treepred5_labeled = class_labels[treepred5]
treepred6_labeled = class_labels[treepred6]
treepred7_labeled = class_labels[treepred7]
treepred8_labeled = class_labels[treepred8]

# decision model
df_tree_decision = rep('Bad', nrow(df_train))
df_train$df_tree_decision[df_train$size_class == 'Tiny' & df_train$sweetness_class == 'Unsweet'] = treepred1_labeled
df_train$df_tree_decision[df_train$size_class == 'Small' & df_train$sweetness_class == 'Unsweet'] = treepred2_labeled
df_train$df_tree_decision[df_train$size_class == 'Medium' & df_train$sweetness_class == 'Unsweet'] = treepred3_labeled
df_train$df_tree_decision[df_train$size_class == 'Large' & df_train$sweetness_class == 'Unsweet'] = treepred4_labeled

df_train$df_tree_decision[df_train$size_class == 'Tiny' & df_train$sweetness_class == 'Sweet'] = treepred5_labeled
df_train$df_tree_decision[df_train$size_class == 'Small' & df_train$sweetness_class == 'Sweet'] = treepred6_labeled
df_train$df_tree_decision[df_train$size_class == 'Medium' & df_train$sweetness_class == 'Sweet'] = treepred7_labeled
df_train$df_tree_decision[df_train$size_class == 'Large' & df_train$sweetness_class == 'Sweet'] = treepred8_labeled


improved_accuracy = mean(df_train$Quality == df_train$df_tree_decision)
print(paste("Improved Accuracy: ", improved_accuracy)) # 0.841538461538462
cat("Improved Accuracy is 0.841538461538462, which is > 10%.")
improved_accuracy / tree_mse # 1.148355, so 14.8355% increase in accuracy
cat("Increase in Accuracy: 14.8355%")


# Categorical for Testing Set

# put trees from testing set into prediction
treepred1 = predict(tree1, df_test[df_test$size_class == "Tiny" & df_test$sweetness_class == 'Unsweet', ], type = "class")
treepred2 = predict(tree2, df_test[df_test$size_class == "Small" & df_test$sweetness_class == 'Unsweet', ], type = "class")
treepred3 = predict(tree3, df_test[df_test$size_class == "Medium" & df_test$sweetness_class == 'Unsweet', ], type = "class")
treepred4 = predict(tree4, df_test[df_test$size_class == "Large" & df_test$sweetness_class == 'Unsweet', ], type = "class")

treepred5 = predict(tree5, df_test[df_test$size_class == "Tiny" & df_test$sweetness_class == 'Sweet', ], type = "class")
treepred6 = predict(tree6, df_test[df_test$size_class == "Small" & df_test$sweetness_class == 'Sweet', ], type = "class")
treepred7 = predict(tree7, df_test[df_test$size_class == "Medium" & df_test$sweetness_class == 'Sweet', ], type = "class")
treepred8 = predict(tree8, df_test[df_test$size_class == "Large" & df_test$sweetness_class == 'Sweet', ], type = "class")

# fix labels so it is not 1 and 2. Make it good and bad
class_labels = c('bad', 'good')
treepred1_labeled = class_labels[treepred1]
treepred2_labeled = class_labels[treepred2]
treepred3_labeled = class_labels[treepred3]
treepred4_labeled = class_labels[treepred4]

treepred5_labeled = class_labels[treepred5]
treepred6_labeled = class_labels[treepred6]
treepred7_labeled = class_labels[treepred7]
treepred8_labeled = class_labels[treepred8]

# put predictions into decision model

df_test_tree_decision = rep('bad', nrow(df_test))
df_test$df_test_tree_decision[df_test$size_class == 'Tiny' & df_test$sweetness_class == 'Unsweet'] = treepred1_labeled
df_test$df_test_tree_decision[df_test$size_class == 'Small' & df_test$sweetness_class == 'Unsweet'] = treepred2_labeled
df_test$df_test_tree_decision[df_test$size_class == 'Medium' & df_test$sweetness_class == 'Unsweet'] = treepred3_labeled
df_test$df_test_tree_decision[df_test$size_class == 'Large' & df_test$sweetness_class == 'Unsweet'] = treepred4_labeled

df_test$df_test_tree_decision[df_test$size_class == 'Tiny' & df_test$sweetness_class == 'Sweet'] = treepred5_labeled
df_test$df_test_tree_decision[df_test$size_class == 'Small' & df_test$sweetness_class == 'Sweet'] = treepred6_labeled
df_test$df_test_tree_decision[df_test$size_class == 'Medium' & df_test$sweetness_class == 'Sweet'] = treepred7_labeled
df_test$df_test_tree_decision[df_test$size_class == 'Large' & df_test$sweetness_class == 'Sweet'] = treepred8_labeled

improved_prediction_accuracy = mean(df_test$Quality == df_test$df_test_tree_decision)
print(paste("Improved Accuracy: ", improved_prediction_accuracy)) # 0.82
improved_prediction_accuracy / tree_mse # 1.124473, so 12.4473% increase in accuracy
cat("Increase in Accuracy: 12.4473%")
