install.packages(c("readxl", "randomForest", "caret", "ggplot2", "dplyr", "Metrics", "writexl", "xgboost", "reshape2"))
# Load libraries
library(readxl)
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(Metrics)
library(writexl)
library(xgboost)
library(reshape2)
# Load dataset
df <- read_excel("C:/Users/roysr/OneDrive/Desktop/PROJECT WORK/KOLKATA DATASET.xlsx")
df$DATE <- as.Date(df$DATE)
#Create lag variables up to lag3 
df <- df %>%
  mutate(
    lag1_PM25 = lag(PM2.5, 1),
    lag2_PM25 = lag(PM2.5, 2),
    lag3_PM25 = lag(PM2.5, 3)
  ) %>%
  na.omit()
#Define train-test splits
splits <- c(0.70, 0.75, 0.80, 0.85)
#Grid search parameter ranges ---
mtry_vals <- 1:3
nodesize_vals <- c(5, 10, 15, 20)
ntree_val <- 500
#Storage for results
results <- data.frame(
  Split_Percent = numeric(),
  mtry = integer(),
  nodesize = integer(),
  Features = character(),
  Train_RMSE = numeric(),
  Test_RMSE = numeric(),
  Train_R2 = numeric(),
  Test_R2 = numeric(),
  Overfitting = character(),
  stringsAsFactors = FALSE
)
set.seed(123)
for(split in splits) {
  train_index <- 1:round(split * nrow(df))
  train_data <- df[train_index, ]
  test_data <- df[-train_index, ]
  for(mtry_val in mtry_vals) {
    for(nodesize_val in nodesize_vals) {
      rf_all <- randomForest(
        PM2.5 ~ lag1_PM25 + lag2_PM25 + lag3_PM25 + HUMIDITY + TEMPERATURE,
        data = train_data,
        ntree = ntree_val,
        mtry = mtry_val,
        nodesize = nodesize_val,
        importance = TRUE
      )
      imp_df <- data.frame(
        Feature = rownames(importance(rf_all)),
        Importance = importance(rf_all)[, "%IncMSE"]
      ) %>% arrange(desc(Importance))
      cat("\nSplit:", split * 100, "% | mtry:", mtry_val, "| nodesize:", nodesize_val, "\n")
      cat("Feature Importance\n")
      print(imp_df)
      top_features <- imp_df$Feature[1:min(3, nrow(imp_df))]
      formula_rf <- as.formula(paste("PM2.5 ~", paste(top_features, collapse = " + ")))
      rf_top <- randomForest(
        formula_rf,
        data = train_data,
        ntree = ntree_val,
        mtry = mtry_val,
        nodesize = nodesize_val
      )
      train_pred <- predict(rf_top, train_data)
      test_pred <- predict(rf_top, test_data)
      train_rmse <- rmse(train_data$PM2.5, train_pred)
      test_rmse <- rmse(test_data$PM2.5, test_pred)
      train_r2 <- R2(train_pred, train_data$PM2.5)
      test_r2 <- R2(test_pred, test_data$PM2.5)
      overfit_flag <- ifelse(abs(train_rmse - test_rmse) < 2, "No", "Yes")
      results <- rbind(results, data.frame(
        Split_Percent = split * 100,
        mtry = mtry_val,
        nodesize = nodesize_val,
        Features = paste(top_features, collapse = ", "),
        Train_RMSE = round(train_rmse, 3),
        Test_RMSE = round(test_rmse, 3),
        Train_R2 = round(train_r2, 3),
        Test_R2 = round(test_r2, 3),
        Overfitting = overfit_flag
      ))
    }
  }
}
non_overfit_results <- results %>% filter(Overfitting == "No")
best_model <- non_overfit_results %>%
  mutate(Total_RMSE = Train_RMSE + Test_RMSE) %>%
  arrange(Total_RMSE) %>%
  head(1)
print(results)
print(best_model)
#Forecast using best configuration (Random Forest)
df_forecast <- read_excel("C:/Users/roysr/OneDrive/Desktop/PROJECT WORK/KOLKATA DATASET.xlsx")
df_forecast$DATE <- as.Date(df_forecast$DATE)
forecast_df <- df_forecast %>%
  arrange(DATE) %>%
  mutate(lag1_PM25 = lag(PM2.5, 1)) %>%
  select(DATE, PM2.5, lag1_PM25, HUMIDITY, TEMPERATURE) %>%
  filter(!is.na(lag1_PM25) & !is.na(PM2.5) & !is.na(HUMIDITY) & !is.na(TEMPERATURE))
set.seed(123)
train_index <- 1:round(0.85 * nrow(forecast_df))
train_data <- forecast_df[train_index, ]
test_data <- forecast_df[-train_index, ]
rf_final <- randomForest(PM2.5 ~ lag1_PM25 + HUMIDITY + TEMPERATURE, data = train_data, ntree = 500, mtry = 2,nodesize = 10)
test_pred <- predict(rf_final, test_data)
jan2025_data <- df_forecast %>%
  filter(format(DATE, "%Y-%m") == "2025-01") %>%
  select(DATE, HUMIDITY, TEMPERATURE)
forecasted_PM25 <- numeric(nrow(jan2025_data))
last_known_PM25 <- df_forecast %>% filter(DATE < min(jan2025_data$DATE)) %>% tail(1) %>% pull(PM2.5)
for(I in 1:nrow(jan2025_data)){
  input_row <- data.frame(
    lag1_PM25 = last_known_PM25,
    HUMIDITY = jan2025_data$HUMIDITY[I],
    TEMPERATURE = jan2025_data$TEMPERATURE[I]
  )
  pred <- predict(rf_final, input_row)
  forecasted_PM25[I] <- pred
  last_known_PM25 <- pred
}
jan2025_forecast <- jan2025_data %>%
  mutate(Predicted_PM25 = forecasted_PM25)
write_xlsx(jan2025_forecast, "C:/Users/roysr/OneDrive/Desktop/PROJECT WORK/ PM2.5 _Jan2025_Forecast_RF.xlsx")
results_df <- data.frame(
  Actual = test_data$PM2.5,
  Predicted = test_pred,
  Date = test_data$DATE
)
#Print Actual Vs Predicted PM2.5  on Test Set
p <- ggplot(results_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted PM2.5  on Test Set",
       x = "Date", y = " PM2.5  Level",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "7 days") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)
ggsave("C:/Users/roysr/OneDrive/Desktop/PROJECT WORK/Actual_vs_Predicted_ _RF.pdf", plot = p, width = 10, height = 6)
# ---------------- XGBoost Model ----------------
xgb_results <- data.frame()
eta_vals <- c(0.01, 0.05, 0.1)
max_depth_vals <- c(3, 5, 7)
for(split in splits) {
  train_index <- 1:round(split * nrow(df))
  train_data <- df[train_index, ]
  test_data <- df[-train_index, ]
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, c("lag1_PM25", "lag2_PM25", "lag3_PM25", "HUMIDITY", "TEMPERATURE")]), label = train_data$PM2.5)
  dtest <- xgb.DMatrix(data = as.matrix(test_data[, c("lag1_PM25", "lag2_PM25", "lag3_PM25", "HUMIDITY", "TEMPERATURE")]), label = test_data$PM2.5)
  for(eta in eta_vals) {
    for(depth in max_depth_vals) {
      param <- list(objective = "reg:squarederror", eta = eta, max_depth = depth)
      xgb_model <- xgb.train(params = param, data = dtrain, nrounds = 100, verbose = 0)
      imp <- xgb.importance(model = xgb_model)
      cat("\nSplit:", split * 100, "% | eta:", eta, "| max_depth:", depth, "\n")
      cat("Feature Importance (Gain)\n")
      print(imp[, c("Feature", "Gain")])
      top_feats <- imp$Feature[1:min(3, nrow(imp))]
      if(length(top_feats) == 0) next
      dtrain_top <- xgb.DMatrix(data = as.matrix(train_data[, top_feats]), label = train_data$PM2.5)
      dtest_top <- xgb.DMatrix(data = as.matrix(test_data[, top_feats]), label = test_data$PM2.5)
      xgb_model_top <- xgb.train(params = param, data = dtrain_top, nrounds = 100, verbose = 0)
      train_pred <- predict(xgb_model_top, dtrain_top)
      test_pred <- predict(xgb_model_top, dtest_top)
      train_rmse <- rmse(train_data$PM2.5, train_pred)
      test_rmse <- rmse(test_data$PM2.5, test_pred)
      train_r2 <- R2(train_pred, train_data$PM2.5)
      test_r2 <- R2(test_pred, test_data$PM2.5)
      overfit_flag <- ifelse(abs(train_rmse - test_rmse) < 2, "No", "Yes")
      xgb_results <- rbind(xgb_results, data.frame(
        Split_Percent = split * 100,
        eta = eta,
        max_depth = depth,
        Features = paste(top_feats, collapse = ", "),
        Train_RMSE = round(train_rmse, 3),
        Test_RMSE = round(test_rmse, 3),
        Train_R2 = round(train_r2, 3),
        Test_R2 = round(test_r2, 3),
        Overfitting = overfit_flag
      ))
    }
  }
}
print(xgb_results)
#Filter best XGB configuration from the results
best_xgb_row <- xgb_results %>%
  filter(Overfitting == "No") %>%
  mutate(Total_RMSE = Train_RMSE + Test_RMSE) %>%
  arrange(Total_RMSE) %>%
  head(1)
print(best_xgb_row)
#Best fit XGBoost forecast for Jan 2025 using lag1_PM25, lag3_PM25, HUMIDITY ---
best_eta <- 0.05
best_max_depth <- 7
split <- 0.85
#Prepare train-test split
train_index <- 1:round(split * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]
#Select features for training and testing
features_xgb <- c("lag1_PM25", "lag3_PM25", "HUMIDITY")
#Prepare data matrices for XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, features_xgb]), label = train_data$PM2.5)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, features_xgb]), label = test_data$PM2.5)
#Train XGBoost model with best params
params <- list(objective = "reg:squarederror", eta = best_eta, max_depth = best_max_depth)
xgb_model_best <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
#Predictions on test set
xgb_test_pred <- predict(xgb_model_best, dtest)
#Forecast for Jan 2025 using XGBoost
jan2025_data_xgb <- df_forecast %>%
  filter(format(DATE, "%Y-%m") == "2025-01") %>%
  select(DATE, HUMIDITY) %>%
  arrange(DATE)
# Prepare lag variables for forecasting
# Initialize lag1_PM25 and lag3_PM25
last_known_PM25 <- df_forecast %>% filter(DATE < min(jan2025_data_xgb$DATE)) %>% tail(3) %>% pull(PM2.5)
forecasted_PM25_xgb <- numeric(nrow(jan2025_data_xgb))
for(I in 1:nrow(jan2025_data_xgb)) {
  lag1_val <- ifelse(I == 1, last_known_PM25[3], forecasted_PM25_xgb[I-1])      # previous day forecast or last known
  lag3_val <- ifelse(I <= 3, last_known_PM25[4 - I], forecasted_PM25_xgb[I-3]) # 3 days ago value
  input_row <- data.frame(
    lag1_PM25 = lag1_val,
    lag3_PM25 = lag3_val,
    HUMIDITY = jan2025_data_xgb$HUMIDITY[I]
  )
  dinput <- xgb.DMatrix(data = as.matrix(input_row))
  pred <- predict(xgb_model_best, dinput)
  forecasted_PM25_xgb[I] <- pred
}
jan2025_forecast_xgb <- jan2025_data_xgb %>%
  mutate(Predicted_PM25 = forecasted_PM25_xgb)
write_xlsx(jan2025_forecast_xgb, "C:/Users/roysr/OneDrive/Desktop/PROJECT WORK/ PM2.5 _Jan2025_Forecast_XGBoost.xlsx")
xgb_results_df <- data.frame(
  Actual = test_data$PM2.5,
  Predicted = xgb_test_pred,
  Date = test_data$DATE
)
p_xgb <- ggplot(xgb_results_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted PM2.5 on Test Set (XGBoost)",
       x = "Date", y = " PM2.5  Level",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "darkorange")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "7 days") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p_xgb)
ggsave("C:/Users/roysr/OneDrive/Desktop/PROJECT WORK/Actual_vs_Predicted_PM25_XGBoost.pdf",plot = p_xgb, width = 10, height = 6)
# Combine RF and XGB predictions on test set for comparison
comparison_df <- data.frame(
  Date = test_data$DATE,
  Actual = test_data$PM2.5,
  RF_Predicted = test_pred,
  XGB_Predicted = xgb_test_pred
)
# Melt data for ggplot
comparison_long <- melt(comparison_df, id.vars = "Date", variable.name = "Type", value.name = "PM25")
# Plot comparison
p_compare <- ggplot(comparison_long, aes(x = Date, y = PM25, color = Type, linetype = Type)) +
  geom_line(linewidth = 1) +
  labs(title = "Actual vs Predicted PM2.5 on Test Set: Random Forest vs XGBoost",
       x = "Date", y = "PM2.5 Level", color = "Legend", linetype = "Legend") +
  scale_color_manual(values = c("Actual" = "black", "RF_Predicted" = "blue", "XGB_Predicted" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "RF_Predicted" = "dashed", "XGB_Predicted" = "dotted")) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "7 days") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p_compare)
ggsave("C:/Users/roysr/OneDrive/Desktop/PROJECT WORK/Actual_vs_Predicted_RF_XGBoost_P.pdf", plot = p_compare, width = 10, height = 6)
