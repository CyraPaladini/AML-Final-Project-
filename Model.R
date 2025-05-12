library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(DALEX)

df <- read_csv("/Users/cyra/Documents/Applied Machine Learning/Final Project/final_data.csv")
df <- final_data %>%
  filter(!is.na(covid_cases_per_100k)) %>%
  select(-FIPS, -county_clean, -State, -covid_cases) #drop rows w missing target 

#train-test split
set.seed(42)
train_index <- createDataPartition(df$covid_cases_per_100k, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

#train rf model 
set.seed(42)
model <- train(
  covid_cases_per_100k ~ ., 
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  importance = TRUE
)

explainer_rf <- explain(
  model = model$finalModel,  
  data = train_data %>% select(-covid_cases_per_100k),  
  y = train_data$covid_cases_per_100k,
  label = "Random Forest"
)

importance_rf <- model_parts(explainer_rf)
plot(importance_rf) +
  ggtitle("Variable Importance via DALEX (Permutation Method)")

county_row <- test_data[10, ] 
local_explanation <- predict_parts(
  explainer = explainer_rf,
  new_observation = county_row %>% select(-covid_cases_per_100k)
)
plot(local_explanation) +
  ggtitle(" Explanation for County's Predicted COVID Rate")


#predict on tests
preds <- predict(model, newdata = test_data)
metrics <- postResample(preds, test_data$covid_cases_per_100k)
cat("Random Forest Model Performance:\n")
print(metrics)

#dumb yet reasonable- average covid cases
baseline_pred <- mean(train_data$covid_cases_per_100k)
baseline_rmse <- sqrt(mean((test_data$covid_cases_per_100k - baseline_pred)^2))
cat("Baseline Model RMSE (mean prediction):", baseline_rmse, "\n")

#actual vs, pred
ggplot(data.frame(Actual = test_data$covid_cases_per_100k, Predicted = preds),
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted COVID Cases per 100k ppl",
       x = "Actual", y = "Predicted")

varImpPlot(model$finalModel, type = 2, main = "Random Forest Variable Importance")
