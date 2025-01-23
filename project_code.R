#Data Cleaning and Processing

library(dplyr)
library(corrplot)
library(ggplot2)
library(tidyr)
library(fastDummies)
library(caret)
library(class)
library(kknn)

dataset <- read.csv("C:/Users/kaur6/Downloads/CSP 571/Project/heart+disease/processed.cleveland.data", header = FALSE)
colnames(dataset) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
str(dataset)

dataset[dataset == "?"] <- NA
sum(is.na(dataset))
dataset <- na.omit(dataset)
str(dataset)

dataset$target <- ifelse(dataset$target == 0, 0, 1)
str(dataset)

dataset$ca <- as.numeric(dataset$ca)
dataset$thal <- as.numeric(dataset$thal)
str(dataset)

numeric_var <-dataset %>% 
  select("age","trestbps","chol","thalach","oldpeak")

categorical_var<- dataset %>%
  select("sex","cp","fbs","restecg","exang","slope","ca",
         "thal","target")%>%
  mutate_if(is.numeric, as.factor)

processed_dataset = cbind(categorical_var,numeric_var)
str(processed_dataset)

#Exploratory Data Analysis
dim(processed_dataset)
summary(processed_dataset)

cor_matrix <- cor(dataset, use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45,
         number.cex = 0.7,
         title = "Correlation Heatmap")

target_variable <- dataset$target
features_dataset <- dataset %>%
  select(-target)
correlations <- sapply(features_dataset, function(x) cor(x, target_variable, use = "complete.obs"))
correlations_df <- data.frame(Feature = names(correlations), Correlation = correlations)
ggplot(correlations_df, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Correlation with the Target Feature",
       x = "Features",
       y = "Correlation") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

numerical_features <- c("age", "trestbps", "chol", "thalach", "oldpeak")

par(mfrow = c(3, 2))
for (feature in numerical_features) {
  hist(processed_dataset[[feature]], main = paste("Histogram of", feature), xlab = feature, col = "skyblue", border = "white")
}

categorical_features <- c("sex", "cp", "fbs", "exang")

par(mfrow = c(2, 2))
for (feature in categorical_features) {
  boxplot(processed_dataset[[feature]] ~ processed_dataset$target, main = paste(feature, "vs Heart Disease"), xlab = feature, ylab = "Heart Disease", col = "lightgreen")
}

par(mfrow = c(1, 1))
age_counts <- processed_dataset %>%
  count(age) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

age_counts$age <- factor(age_counts$age, levels = age_counts$age[order(age_counts$n, decreasing = TRUE)])

ggplot(age_counts, aes(x = age, y = n, fill = age)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +  # Use a color palette for different colors
  labs(title = "Top 10 Ages by Count",
       x = "Age",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

minAge <- min(processed_dataset$age, na.rm = TRUE)
maxAge <- max(processed_dataset$age, na.rm = TRUE)
meanAge <- mean(processed_dataset$age, na.rm = TRUE)
cat("Min Age:", minAge, "\n")
cat("Max Age:", maxAge, "\n")
cat("Mean Age:", meanAge, "\n")

processed_dataset1 <- processed_dataset %>%
  mutate(age_category = case_when(
    age >= 29 & age < 40 ~ "Young",
    age >= 40 & age < 55 ~ "Middle",
    age > 55 ~ "Elder"
  )) %>%
  filter(!is.na(age_category)) 

age_category_counts <- processed_dataset1 %>%
  count(age_category)

ggplot(age_category_counts, aes(x = age_category, y = n, fill = age_category)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +  
  labs(title = "Count of Age Categories",
       x = "Age Category",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

processed_dataset2 <- processed_dataset %>%
  mutate(age_category = case_when(
    age >= 29 & age < 40 ~ "Young",
    age >= 40 & age < 55 ~ "Middle",
    age > 55 ~ "Elder"
  )) %>%
  filter(!is.na(age_category)) %>%
  count(age_category)

colors <- c("blue", "green", "yellow")
explode <- c(0, 0, 0.1)

ggplot(processed_dataset2, aes(x = "", y = n, fill = age_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +
  labs(title = "Distribution of Age Categories",
       fill = "Age Category") +
  theme_void(base_size = 15) +
  theme(plot.title = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)) +
  geom_text(aes(label = scales::percent(n / sum(n))), 
            position = position_stack(vjust = 0.5),
            size = 5)

ggplot(processed_dataset, aes(x = sex)) +
  geom_bar(fill = "skyblue") +  # Adjust fill color as needed
  labs(title = "Count of Sex",
       x = "Sex",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggplot(processed_dataset, aes(x = sex, fill = factor(slope))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Sex by Slope",
       x = "Sex",
       y = "Count",
       fill = "Slope") +
  scale_fill_manual(values = c("blue", "green", "red")) +  # Adjust colors as needed
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggplot(processed_dataset, aes(x = factor(cp))) +
  geom_bar(fill = "skyblue") +  
  labs(title = "Count of Chest Pain Type (cp)",
       x = "Chest Pain Type",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggplot(processed_dataset, aes(x = factor(cp), fill = factor(target))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Chest Pain Type by Target",
       x = "Chest Pain Type",
       y = "Count",
       fill = "Target") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggplot(processed_dataset, aes(x = factor(thal))) +
  geom_bar(fill = "skyblue") +  
  labs(title = "Count of Thalassemia Types",
       x = "Thalassemia Type",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggplot(processed_dataset, aes(x = factor(target))) +
  geom_bar(fill = "skyblue") + 
  labs(title = "Count of Target Variable",
       x = "Target",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

# Feature Importance analysis
library(randomForest)
rf_model <- randomForest(as.factor(target) ~ ., data = processed_dataset, importance = TRUE)
importance_data <- importance(rf_model)
importance_df <- as.data.frame(importance_data)
print(importance_df)
sorted_importance <- importance_df[order(-importance_df$MeanDecreaseAccuracy, -importance_df$MeanDecreaseGini), ]
top_features <- rownames(sorted_importance)[1:9]
print(top_features)

#Dataset with important predictors only
columns_to_include <- c("target", "ca", "thal", "cp", "oldpeak", "sex", "thalach", "exang", "age", "slope")
new_dataset <- processed_dataset %>%
  select(all_of(columns_to_include))
str(new_dataset)

#Feature Engineering/Normalization/Scaling
categorical_val <- c()
continuous_val <- c()

for (column in names(new_dataset)) {
  cat("——————–\n")
  cat(paste(column, ": ", paste(unique(new_dataset[[column]]), collapse = ", "), "\n"))
  
  if (length(unique(new_dataset[[column]])) <= 10) {
    categorical_val <- c(categorical_val, column)
  } else {
    continuous_val <- c(continuous_val, column)
  }
}

cat("Categorical Variables:\n")
print(categorical_val)
cat("Continuous Variables:\n")
print(continuous_val)


categorical_val <- setdiff(categorical_val, 'target')
dfs <- dummy_cols(new_dataset, select_columns = categorical_val, remove_first_dummy = FALSE)
dfs <- dfs %>% select(-all_of(categorical_val))
head(dfs, 6)

col_to_scale <- c('thalach', 'oldpeak', 'age')
scaled_data <- dfs %>%
  mutate(across(all_of(col_to_scale), scale))
head(scaled_data, 6)


#Modelling (imp predictors)
target <- dfs$target
X <- dfs %>% select(-target)
data <- cbind(X, target)
set.seed(42)
train_index <- createDataPartition(data$target, p = 0.7, list = FALSE)
train_set <- data[train_index, ]
test_set <- data[-train_index, ]
X_train <- train_set %>% select(-target)
y_train <- train_set$target
X_test <- test_set %>% select(-target)
y_test <- test_set$target

#Logistic Regression
logistic_model <- glm(target ~ ., data = train_set, family = binomial)
prob_pred <- predict(logistic_model, newdata = test_set, type = "response")
y_pred1 <- ifelse(prob_pred > 0.5, 1, 0)
y_test_numeric <- as.numeric(as.character(y_test))
accuracy <- sum(y_pred1 == y_test_numeric) / length(y_test_numeric)
print(paste("Accuracy:", round(accuracy, 4)))

#SVM
library(e1071)
svm_model <- svm(target ~ ., data = train_set, kernel = "radial")
y_pred_svm <- predict(svm_model, newdata = test_set)
accuracy_svm <- sum(y_pred_svm == y_test) / length(y_test)
print(paste("SVM Accuracy:", round(accuracy_svm, 4)))

#Random forest
library(randomForest)
rf_model <- randomForest(target ~ ., data = train_set)
y_pred_rf <- predict(rf_model, newdata = test_set)
accuracy_rf <- sum(y_pred_rf == y_test) / length(y_test)
print(paste("Random Forest Accuracy:", round(accuracy_rf, 4)))

#xgboost
library(xgboost)
train_set_xgb <- train_set
test_set_xgb <- test_set
train_set_xgb$target <- as.numeric(train_set_xgb$target) - 1
test_set_xgb$target <- as.numeric(test_set_xgb$target) - 1
X_train_xgb <- train_set_xgb %>% select(-target)
y_train_xgb <- train_set_xgb$target
X_test_xgb <- test_set_xgb %>% select(-target)
y_test_xgb <- test_set_xgb$target
dtrain_xgb <- xgb.DMatrix(data = as.matrix(X_train_xgb), label = y_train_xgb)
dtest_xgb <- xgb.DMatrix(data = as.matrix(X_test_xgb), label = y_test_xgb)
params_xgb <- list(
  objective = "binary:logistic", # for binary classification
  eval_metric = "logloss",       # evaluation metric
  eta = 0.01,                    # learning rate
  max_depth = 6                  # maximum depth of trees
)
xgb_model <- xgb.train(
  params = params_xgb,
  data = dtrain_xgb,
  nrounds = 100,                 
  watchlist = list(train = dtrain_xgb, test = dtest_xgb),
  early_stopping_rounds = 10     
)
y_pred_xgb <- predict(xgb_model, newdata = dtest_xgb)
y_pred_xgb_class <- ifelse(y_pred_xgb > 0.5, 1, 0)

accuracy_xgb <- sum(y_pred_xgb_class == y_test_xgb) / length(y_test_xgb)
print(paste("XGBoost Accuracy:", round(accuracy_xgb, 4)))


#KNN Classification
k <- 10
y_pred_knn <- knn(train = X_train, test = X_test, cl = y_train, k = k)

accuracy <- sum(y_pred_knn == y_test) / length(y_test)
print(paste("Accuracy:", round(accuracy, 4)))

k_values <- seq(1, 20, by = 1)
# Perform 10-fold cross-validation
cv_results <- train(
  x = X_train, y = y_train,
  method = "knn",
  tuneGrid = data.frame(k = k_values),
  trControl = trainControl(method = "cv", number = 10)
)
print(cv_results)

optimal_k <- cv_results$bestTune$k
print(paste("Optimal k:", optimal_k))

y_pred_knn <- knn(train = X_train, test = X_test, cl = y_train, k = optimal_k)
accuracy <- sum(y_pred_knn == y_test) / length(y_test)
print(paste("Accuracy:", round(accuracy, 4)))

feature_var <- nearZeroVar(X_train)
X_train_filtered <- X_train[, -feature_var]
X_test_filtered <- X_test[, -feature_var]
y_pred_knn_filtered <- knn(train = X_train_filtered, test = X_test_filtered, cl = y_train, k = optimal_k)
accuracy_filtered <- sum(y_pred_knn_filtered == y_test) / length(y_test)
print(paste("Accuracy with filtered features:", round(accuracy_filtered, 4)))

X_train_df <- as.data.frame(X_train)
X_test_df <- as.data.frame(X_test)
y_train_df <- as.factor(y_train)
y_test_df <- as.factor(y_test)

fit_knn_kknn <- kknn(
  formula = y_train ~ .,
  train = X_train_df,
  test = X_test_df,
  k = optimal_k,
  distance = 1  # Manhattan distance
)

y_pred_knn_manhattan_kknn <- fitted(fit_knn_kknn)
accuracy_manhattan_kknn <- sum(y_pred_knn_manhattan_kknn == y_test_df) / length(y_test_df)
print(paste("Accuracy with k =", optimal_k, "and Manhattan distance (kknn):", round(accuracy_manhattan_kknn, 4)))




