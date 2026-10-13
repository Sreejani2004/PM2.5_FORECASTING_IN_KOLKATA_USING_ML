# PM₂.₅ Forecasting in Kolkata Using Machine Learning Algorithms

## Project Overview
This project forecasts daily PM₂.₅ concentrations in Kolkata using historical environmental data from 2019 to 2024. Random Forest and XGBoost regression models were used to analyse nonlinear relationships between air pollution, temperature, and humidity and generate daily forecasts for January 2025.

## Dataset
- **Observations:** 2,192 daily records
- **Variables:** PM₂.₅, temperature, and humidity
- **Forecast Period:** January 2025

## Methodology
- Prepared and analysed historical environmental data.
- Developed Random Forest and XGBoost regression models.
- Evaluated model performance using RMSE and R².

## Results
- **Test RMSE:** Above 11 and below 13
- **Test R²:** Approximately 0.80

Both models demonstrated promising predictive performance on the test dataset.

## Tools & Technologies
Tools & Technologies

-Language: R
-Packages: readxl, randomForest, caret, ggplot2, dplyr, Metrics, writexl, xgboost, reshape2
-Techniques: Random Forest, XGBoost, feature importance, hyperparameter tuning, regression evaluation and forecasting.

## Conclusion
This project demonstrates the application of ensemble machine learning techniques to short-term air quality forecasting, with potential applications in pollution monitoring and public health planning.
