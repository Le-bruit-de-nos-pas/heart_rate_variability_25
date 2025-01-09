library(readxl)
library(tidyverse)
library(data.table)

# Summary Table -------------------
Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)


dim(Marc_Anne_HR_Variability_MSA)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA$Score_UMSARS1_total
Marc_Anne_HR_Variability_MSA$Score_UMSARS_2_total
Marc_Anne_HR_Variability_MSA$AMS_27___score_global_COMPASS <- as.numeric(Marc_Anne_HR_Variability_MSA$AMS_27___score_global_COMPASS)

paste0(names(Marc_Anne_HR_Variability_MSA)[6:55], collapse = '","')

unique(Marc_Anne_HR_Variability_MSA$cause)
unique(Marc_Anne_HR_Variability_MSA$date_)
unique(Marc_Anne_HR_Variability_MSA$AMS__011___Année_d_apparition_1er_symptome_maladie)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

Marc_Anne_HR_Variability_MSA$Date_de_l_examen
Marc_Anne_HR_Variability_MSA$date_

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 365.25
)



# Extract the year from Date_de_l_examen
Marc_Anne_HR_Variability_MSA$Year_de_l_examen <- as.numeric(format(as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen), "%Y"))

# Ensure AMS__011___Année_d_apparition_1er_symptome_maladie is numeric
Marc_Anne_HR_Variability_MSA$Year_1st_symptom <- as.numeric(Marc_Anne_HR_Variability_MSA$AMS__011___Année_d_apparition_1er_symptome_maladie)

# Calculate the difference
Marc_Anne_HR_Variability_MSA$Symptom_duration <- Marc_Anne_HR_Variability_MSA$Year_de_l_examen - Marc_Anne_HR_Variability_MSA$Year_1st_symptom



# Define a vector of variable names
variables <- c("Sexe_", "Age_lors_de_l_examen", "Symptom_duration", "Followup_duration",
               "Score_UMSARS1", "Score_UMSARS2", "cause",
               "AMS_27___score_global_COMPASS", "AMS_030___cotation_SCOPA", "score_UMSARS_4")


Marc_Anne_HR_Variability_MSA$AMS_030___cotation_SCOPA <- as.numeric(Marc_Anne_HR_Variability_MSA$AMS_030___cotation_SCOPA)

# Initialize an empty list to store the results
results <- list()

# Loop through the variables
for (var in variables) {
  # Calculate summary statistics for the current variable
  print(var)
  print("____________________________________________________________")
  stats <- list(
    mean = mean(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    sd = sd(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    median = median(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    quantile_25 = quantile(Marc_Anne_HR_Variability_MSA[[var]], 0.25, na.rm = TRUE),
    quantile_75 = quantile(Marc_Anne_HR_Variability_MSA[[var]], 0.75, na.rm = TRUE)
  )
  
  # Store the results in the list with the variable name as the key
  results[[var]] <- stats
}

# View the results
results



# Initialize an empty list to store the results
results <- list()

# Loop through the variables
for (var in variables) {
  # Calculate the number of missing values for the current variable
  print(var)
  print("____________________________________________________________")
  missing_count <- sum(is.na(Marc_Anne_HR_Variability_MSA[[var]]))
  
  # Store the result in the list with the variable name as the key
  results[[var]] <- missing_count
  print(paste("Number of missing values:", missing_count))
}

# View the results
results



# Define a vector of variable names
variables <- c("ms/mmHg","Mean_RR_(ms)","pNN50_(%)","SDNN_(ms)","rMSSD_(ms)","Ptot_(ms²)",
               "VLF_(ms²)","LF_(ms²)","HF_(ms²)","LF/HF","pLF1_(ms²)","pLF2_(ms²)","pHF1_(ms²)",
               "pHF2_(ms²)","IMAI1","IMAI2","Triangular_index","TINN_(ms)","X_(ms)","Y_(beats)",
               "M_(ms)","N_(ms)","Approximate_Entropy","Sample_Entropy","Shanon_Entropy_(SE)",
               "Conditional_Entropy_(CE)","Corrected_CE_(CCE)","Normalized_CCE_(NCCE)","ρ",
               "Lempel-Ziv_Complexity_(LZC)","Centroid_(ms)","SD1_(ms)","SD2_(ms)","SD1/SD2",
               "OV","OV%","1V","1V%","2V","2V%","2UV","2UV%","MP","MP%","α1_(DFA)",
               "α2_(DFA)","H_(DFA)","H_(Higuchi)","H_(Katz)","Hurst")



# Initialize an empty list to store the results
results <- list()

# Loop through the variables
for (var in variables) {
  # Calculate summary statistics for the current variable
  print(var)
  print("____________________________________________________________")
  stats <- list(
    mean = mean(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    sd = sd(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    median = median(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    quantile_25 = quantile(Marc_Anne_HR_Variability_MSA[[var]], 0.25, na.rm = TRUE),
    quantile_75 = quantile(Marc_Anne_HR_Variability_MSA[[var]], 0.75, na.rm = TRUE)
  )
  
  # Store the results in the list with the variable name as the key
  results[[var]] <- stats
}

# View the results
results



# Initialize an empty list to store the results
results <- list()

# Loop through the variables
for (var in variables) {
  # Calculate the number of missing values for the current variable
  print(var)
  print("____________________________________________________________")
  missing_count <- sum(is.na(Marc_Anne_HR_Variability_MSA[[var]]))
  
  # Store the result in the list with the variable name as the key
  results[[var]] <- missing_count
  print(paste("Number of missing values:", missing_count))
}

# View the results
results

# ---------------

# Deltas PAS PAD -------------------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)


Deltas_PAS_PAD <- Marc_Anne_HR_Variability_MSA %>% select(patid,PAS_couche:PAS_minute10) %>%
  gather(Minute, PAS_Stand, PAS_minute1:PAS_minute10) %>% drop_na() %>%
  mutate(Delta=PAS_couche-PAS_Stand) %>%
  group_by(patid) %>% summarise(Delta_PAS=max(Delta,na.rm=T)) %>% filter(Delta_PAS!="-Inf") %>%
  full_join(
    Marc_Anne_HR_Variability_MSA %>% select(patid,PADcouche:PAD_minute10) %>%
      gather(Minute, PAD_Stand, PAD_minute1:PAD_minute10) %>% drop_na() %>%
      mutate(Delta=PADcouche-PAD_Stand) %>%
      group_by(patid) %>% summarise(Delta_PAD=max(Delta,na.rm=T)) %>% filter(Delta_PAD!="-Inf")
  ) 


fwrite(Deltas_PAS_PAD, "Deltas_PAS_PAD.txt")



variables <- c("Delta_PAS", "Delta_PAD")

# Initialize an empty list to store the results
results <- list()

# Loop through the variables
for (var in variables) {
  # Calculate summary statistics for the current variable
  print(var)
  print("____________________________________________________________")
  stats <- list(
    mean = mean(Deltas_PAS_PAD[[var]], na.rm = TRUE),
    sd = sd(Deltas_PAS_PAD[[var]], na.rm = TRUE),
    median = median(Deltas_PAS_PAD[[var]], na.rm = TRUE),
    quantile_25 = quantile(Deltas_PAS_PAD[[var]], 0.25, na.rm = TRUE),
    quantile_75 = quantile(Deltas_PAS_PAD[[var]], 0.75, na.rm = TRUE)
  )
  
  # Store the results in the list with the variable name as the key
  results[[var]] <- stats
}

# View the results
results

# ---------------

# Sudoscans ------------------


Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `sudoscan_(fait/non_fait)`:`calcul_risque_neuropathie_végétative_cardiaque_(%)_(_risque_eval)`)

names(Marc_Anne_HR_Variability_MSA)



variables <- c("sudoscan_-_conduction_main_droite_(µsiemens)", "sudoscan_-_conduction_main_gauche_(µsiemens)",
               "sudoscan_-_conduction_moy_mains_(µsiemens)" , "sudoscan_-_conduction_pied_droit_(µsiemens)",
               "sudoscan_-_conduction_pied_gauche_(µsiemens)","sudoscan_-_conduction_moy_pieds_(µsiemens)",
               "sudoscan_-_asymétrie_mains_(%)", "sudoscan_-_asymétrie_pieds_(%)",
               "calcul_risque_neuropathie_végétative_cardiaque_(%)_(_risque_eval)")

# Initialize an empty list to store the results
results <- list()

# Loop through the variables
for (var in variables) {
  # Calculate summary statistics for the current variable
  print(var)
  print("____________________________________________________________")
  stats <- list(
    mean = mean(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    sd = sd(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    median = median(Marc_Anne_HR_Variability_MSA[[var]], na.rm = TRUE),
    quantile_25 = quantile(Marc_Anne_HR_Variability_MSA[[var]], 0.25, na.rm = TRUE),
    quantile_75 = quantile(Marc_Anne_HR_Variability_MSA[[var]], 0.75, na.rm = TRUE)
  )
  
  # Store the results in the list with the variable name as the key
  results[[var]] <- stats
}

# View the results
results



# ------------------

# plot regression fit Total UMSARS ~ Each HR variability metric ---------
Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 365.25
)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst,  Score_UMSARS1, Score_UMSARS2, Followup_duration, cause)

Deltas_PAS_PAD <- fread("Deltas_PAS_PAD.txt")

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% left_join(Deltas_PAS_PAD)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst,  Score_UMSARS1, Score_UMSARS2) %>%
  mutate(UMSARS=Score_UMSARS1+Score_UMSARS2) %>%
  select(-c(Score_UMSARS1, Score_UMSARS2))


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))


names(Marc_Anne_HR_Variability_MSA)

# 400 400
Marc_Anne_HR_Variability_MSA %>%
  ggplot(aes(oneV, UMSARS)) +
  geom_jitter(alpha=0.6,colour="#0087fa", shape=1, stroke=2, size=0.8) +
  geom_smooth(fill="#ff004f", colour="#ff004f", alpha=0.1) +
  theme_light() +
  ylab("UMSARS 1+2") + xlab("1V")

# ---------

# Heart rate variability metrics and UMSARS ------------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 365.25
)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst,  Score_UMSARS1, Score_UMSARS2, Followup_duration, cause)

Deltas_PAS_PAD <- fread("Deltas_PAS_PAD.txt")

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% left_join(Deltas_PAS_PAD)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst,  Score_UMSARS1, Score_UMSARS2) %>%
  mutate(UMSARS=Score_UMSARS1+Score_UMSARS2) %>%
  select(-c(Score_UMSARS1, Score_UMSARS2))


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))


library(leaps)
library(glmnet)
library(car)


# Ensure predictors are scaled
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>%
  mutate(across(where(is.numeric), scale))

# Correlation matrix
cor_matrix <- cor(Marc_Anne_HR_Variability_MSA %>% select(where(is.numeric)), method = "spearman")
print(cor_matrix)


# fwrite(data.frame(cor_matrix), "cor_matrix.csv")


# Find pairs of highly correlated variables
#high_corr <- which(abs(cor_matrix) > 0.8, arr.ind = TRUE)
#print(high_corr)



#Best Subset Selection
# set.seed(1)
# regit_full <- regsubsets(UMSARS ~ ., data = Marc_Anne_HR_Variability_MSA, nvmax = 50, really.big=T)
# reg_summary <- summary(regit_full)
# 
# 
# ignore <- data.frame(reg_summary$which)
# 
# fwrite(ignore, "ignore.csv")


Best_Subset_Predictors <- fread("Best_Subset_Preds_HR_UMSARS.csv")

names(Best_Subset_Predictors)

light_blue = rgb(0/255, 135/255, 250/255)  # Light blue: RGB(0, 135, 250)
light_pink = rgb(255/255, 0/255, 79/255)   # Light pink: RGB(255, 0, 79)

Best_Subset_Predictors %>% gather(Var, Pres, `ms/mmHg`:`Hurst`) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=vars , y=Var, fill = Predictor_Included)) + 
  geom_tile(color = "snow", size = 0.1, show.legend = F) + 
  scale_fill_manual( values= c("snow", light_pink) ) +
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  coord_equal() + 
  theme_minimal() +
  # scale_x_continuous(breaks = seq(min(Best_Subset_Predictors$vars),max(Best_Subset_Predictors$vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Number of Predictors") +ylab("Predictor Included (yes/no)")



# Plot RSS, Adjusted R², Cp, and BIC

# Define the colors for the alternating lines
# light_blue = rgb(0/255, 135/255, 250/255)  # Light blue: RGB(0, 135, 250)
# light_pink = rgb(255/255, 0/255, 79/255)   # Light pink: RGB(255, 0, 79)

# Set up the plot layout with 2 rows and 2 columns
# par(mfrow = c(2, 2))  # Arrange plots in a grid

# Plot RSS with alternating colors and thicker lines
# plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", lwd = 3, col = light_pink)
# # Plot Adjusted R² with alternating colors and thicker lines
# plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l", lwd = 3, col = light_blue)
# # Plot Cp with alternating colors and thicker lines
# plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", lwd = 3, col = light_blue)
# # Plot BIC with alternating colors and thicker lines
# plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 3, col = light_pink)


# # Cross-Validation for Model Selection
# set.seed(1)
# k <- 10
# n <- nrow(Marc_Anne_HR_Variability_MSA)
# folds <- sample(rep(1:k, length = n))
# cv.errors <- matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))
# 
# 
# predict.regsubsets <- function(object, newdata, id) {
#   form <- as.formula(object$call[[2]])
#   mat <- model.matrix(form, newdata)
#   coefi <- coef(object, id = id)
#   xvars <- names(coefi)
#   mat[, xvars, drop = FALSE] %*% coefi
# }
# 
# 
# for (j in 1:k) {
#   best.fit <- regsubsets(UMSARS ~ ., data = Marc_Anne_HR_Variability_MSA[folds != j, ], nvmax = 10, really.big=T)
#   for (i in 1:10) {
#     pred <- predict.regsubsets(best.fit, Marc_Anne_HR_Variability_MSA[folds == j, ], id = i)
#     cv.errors[j, i] <- mean((Marc_Anne_HR_Variability_MSA$UMSARS[folds == j] - pred)^2)
#   }
# }
# 
# mean.cv.errors <- apply(cv.errors, 2, mean)
# 
# # Plot Cross-Validation Errors
# data.frame(mean.cv.errors) %>%
#   mutate(N = row_number()) %>%
#   ggplot(aes(N, mean.cv.errors)) +
#   geom_point(size = 3, alpha = 1, shape = 4) +
#   geom_line(size = 2, alpha = 0.3, colour = "deepskyblue4") +
#   theme_minimal() +
#   xlab("\nNumber of Predictors") +
#   ylab("10-fold Cross-Validation Error\n")
# 
# # Final Model Selection and Coefficients
# best_model_size <- which.min(mean.cv.errors)
# regfit.best <- regsubsets(UMSARS ~ ., data = Marc_Anne_HR_Variability_MSA, nvmax = 10)
# coef(regfit.best, best_model_size)

# # Check Multicollinearity with VIF
# vif_results <- vif(lm(UMSARS ~ ., data = Marc_Anne_HR_Variability_MSA))
# print(vif_results)


# Ensure predictors are scaled
X <- model.matrix(UMSARS ~ ., data = Marc_Anne_HR_Variability_MSA)[, -1]  # Design matrix (exclude intercept)
y <- Marc_Anne_HR_Variability_MSA$UMSARS  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

# Plot LASSO cross-validation results
plot(lasso_cv)
title("LASSO Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.min")
print(lasso_coeffs)



# Ridge Regression (alpha = 0)
set.seed(1)
ridge_cv <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

# Best lambda
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Plot Ridge cross-validation results
plot(ridge_cv)
title("Ridge Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
ridge_coeffs <- coef(ridge_cv, s = "lambda.min")
print(ridge_coeffs)

# Compare MSE
lasso_mse <- min(lasso_cv$cvm)
ridge_mse <- min(ridge_cv$cvm)
print(paste("LASSO MSE:", lasso_mse))
print(paste("Ridge MSE:", ridge_mse))

# Visualize Coefficients
lasso_coefs_df <- data.frame(
  Feature = rownames(as.matrix(lasso_coeffs)),
  Coefficient = as.numeric(lasso_coeffs)
) %>%
  filter(Coefficient != 0)

ggplot(lasso_coefs_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "LASSO Coefficients", x = "Feature", y = "Coefficient")

ridge_coefs_df <- data.frame(
  Feature = rownames(as.matrix(ridge_coeffs)),
  Coefficient = as.numeric(ridge_coeffs)
)

ggplot(ridge_coefs_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Ridge Coefficients", x = "Feature", y = "Coefficient")



# Predict function for glmnet models
lasso_predictions <- predict(lasso_cv, newx = X, s = "lambda.min")
ridge_predictions <- predict(ridge_cv, newx = X, s = "lambda.min")

cor(lasso_predictions, data.frame(y))
cor(ridge_predictions, data.frame(y))

# Create data frames for plotting
lasso_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(lasso_predictions)
)

ridge_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(ridge_predictions)
)

# LASSO Plot
ggplot(lasso_plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "LASSO: Actual vs Predicted", x = "Actual Values", y = "Predicted Values")

# Ridge Plot
ggplot(ridge_plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "Ridge: Actual vs Predicted", x = "Actual Values", y = "Predicted Values")

var(y)

mean((y - mean(y))^2)

# Calculate residuals for LASSO and Ridge
lasso_residuals <- y - as.numeric(lasso_predictions)
ridge_residuals <- y - as.numeric(ridge_predictions)

# Plot residuals
par(mfrow = c(1, 2))
plot(lasso_residuals, main = "LASSO Residuals", xlab = "Index", ylab = "Residuals")
plot(ridge_residuals, main = "Ridge Residuals", xlab = "Index", ylab = "Residuals")

plot(lasso_cv)


# Reverse standardization for Lasso predictions
y_pred <- predict(lasso_cv, newx = X, s = "lambda.min")
y_pred_rescaled <- y_pred * sd(y) + mean(y)

data.frame(Actual = y, Predicted = y_pred_rescaled) %>%
  ggplot(aes(x = Actual, y = lambda.min)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "LASSO: Actual vs Predicted (Rescaled)", x = "Actual Values", y = "Predicted Values")





# Set seed for reproducibility
set.seed(1)

# Split the data into 70% training and 30% testing
train_index <- sample(1:nrow(Marc_Anne_HR_Variability_MSA), size = 0.7 * nrow(Marc_Anne_HR_Variability_MSA))
train_data <- Marc_Anne_HR_Variability_MSA[train_index, ]
test_data <- Marc_Anne_HR_Variability_MSA[-train_index, ]

# Check the sizes of the splits
cat("Training data size: ", nrow(train_data), "\n")
cat("Testing data size: ", nrow(test_data), "\n")


library(randomForest)
# Fit a random forest model
set.seed(1)
rf_model <- randomForest(UMSARS ~ ., data = train_data, ntree = 10000, mtry = sqrt(ncol(Marc_Anne_HR_Variability_MSA)-1))

# View the model summary
print(rf_model)

# Check the importance of each feature
data.frame(importance(rf_model)) %>% arrange(IncNodePurity) %>%
  arrange(-IncNodePurity)

# Make predictions on test data (assuming 'test' is your testing dataset)
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate performance using MSE
mse_rf <- mean((test_data$UMSARS - rf_predictions)^2)
cat("MSE for Random Forest: ", mse_rf, "\n")



library(e1071)

# Fit the SVM model (use radial basis kernel)
svm_model <- svm(UMSARS ~ ., data = train_data, kernel = "radial", cost = 10, gamma = 0.1)

# View model summary
summary(svm_model)

# Make predictions on the test data
svm_predictions <- predict(svm_model, newdata = test_data)

# Evaluate performance using MSE on the test set
mse_svm <- mean((test_data$UMSARS - svm_predictions)^2)
cat("MSE for SVM: ", mse_svm, "\n")



# library(tensorflow)
# library(keras)
# 
# # Define the model with multiple hidden layers
# model <- keras_model_sequential() %>%
#   layer_dense(units = 128, activation = "relu", input_shape = ncol(train_data) - 1) %>%
#   layer_dense(units = 64, activation = "relu") %>%
#   layer_dense(units = 32, activation = "relu") %>%
#   layer_dense(units = 1) # Output layer for regression
# 
# # Compile the model
# model %>% compile(
#   optimizer = "adam",
#   loss = "mse",
#   metrics = list("mse")
# )
# 
# # Train the model
# history <- model %>% fit(
#   as.matrix(train_data[,-ncol(train_data)]), # Exclude target variable
#   train_data$UMSARS,
#   epochs = 100,
#   batch_size = 32,
#   validation_split = 0.2
# )
# 
# # Evaluate on test data
# test_mse <- model %>% evaluate(
#   as.matrix(test_data[,-ncol(test_data)]), 
#   test_data$UMSARS
# )
# cat("Test MSE for Neural Network: ", test_mse$loss, "\n")
# 
# 
# 
# 
# # Create a data frame with the results
# results <- data.frame(
#   Model = c("Random Forest", "SVM", "Neural Network"),
#   MSE = c(mse_rf, mse_svm, test_mse)
# )
# 
# # Print results
# print(results)


# ------------------

# Heart rate variability metrics and Death ------------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 30.5
)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst, Followup_duration, cause)


Marc_Anne_HR_Variability_MSA %>% group_by(cause) %>% count() # 129 vs 20

Marc_Anne_HR_Variability_MSA %>% group_by(cause) %>% summarise(mean=mean(-1*Followup_duration, na.rm=T))

# cause  mean
# 1     0 61.3
# 2     1 43.2

Marc_Anne_HR_Variability_MSA %>% group_by(cause) %>% summarise(sd=sd(-1*Followup_duration, na.rm=T))

# cause    sd
# 1     0  18.0
# 2     1  23.0

names(Marc_Anne_HR_Variability_MSA)
unique(Marc_Anne_HR_Variability_MSA$cause)
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(is.na(cause),0,cause))

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))


library("survival")
library("survminer")

Marc_Anne_HR_Variability_MSA$Followup_duration <- Marc_Anne_HR_Variability_MSA$Followup_duration * (-1)

fit <- survfit(Surv(Followup_duration, cause) ~ 1, data = Marc_Anne_HR_Variability_MSA)

print(fit)

# 800 vs 1200

ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           #cumcensor = TRUE,
           #break.time.by = 1,
           xlab = "\n Elapsed Time (Months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_pubclean(), 
           tables.theme = theme_pubclean(),
           tables.height = 0.15 ,
           title = "Kaplan–Meier Survival Curve",
           palette = c( "black"))



names(Marc_Anne_HR_Variability_MSA)




# Standardize HRV features (columns 1 to 50)
hrv_features <- Marc_Anne_HR_Variability_MSA[, 1:50]
scaled_hrv <- as.data.frame(scale(hrv_features))

# Add standardized HRV features back to the dataset
data_standardized <- cbind(scaled_hrv, 
                           Followup_duration = Marc_Anne_HR_Variability_MSA$Followup_duration, 
                           cause = Marc_Anne_HR_Variability_MSA$cause)


# Check standardized summary
summary(data_standardized[, 1:50])


Marc_Anne_HR_Variability_MSA %>% group_by(cause) %>%
  summarise(mean=mean(oneVperc                                           ))


# Fit the baseline Cox model (without regularization)
cox_model <- coxph(Surv(Followup_duration, cause) ~ ., data = data_standardized)
summary(cox_model)




library(glmnet)

# Prepare the data for glmnet (we exclude Followup_duration and cause as they are the response variables)
x <- as.matrix(data_standardized[, 1:50])  # HRV features
y <- Surv(data_standardized$Followup_duration, data_standardized$cause)  # Survival object

# Fit Cox model with LASSO regularization (alpha = 1 for LASSO)
lasso_model <- cv.glmnet(x, y, family = "cox", alpha = 1)

# Check the lambda that gives the minimum cross-validation error
lasso_model$lambda.min

# Get the coefficients for the model with the minimum lambda
coef(lasso_model, s = "lambda.min")


# Fit Cox model with Ridge regularization (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(x, y, family = "cox", alpha = 0)

# Check the lambda that gives the minimum cross-validation error
ridge_model$lambda.min

# Get the coefficients for the model with the minimum lambda
coef(ridge_model, s = "lambda.min")



# Predicted probabilities from the LASSO model
lasso_probs <- predict(lasso_model, newx = x, s = "lambda.min", type = "response")
lasso_pred <- ifelse(lasso_probs >= 1, 1, 0)  # Convert probabilities to binary outcomes

# Predicted probabilities from the Ridge model
ridge_probs <- predict(ridge_model, newx = x, s = "lambda.min", type = "response")
ridge_pred <- ifelse(ridge_probs >= 0.7, 1, 0)  # Convert probabilities to binary outcomes

# Actual outcomes
true_outcomes <- data_standardized$cause


# Create confusion matrices
lasso_conf_matrix <- table(Predicted = lasso_pred, Actual = true_outcomes)
ridge_conf_matrix <- table(Predicted = ridge_pred, Actual = true_outcomes)


# Print the confusion matrices
print("Confusion Matrix for LASSO Model:")
print(lasso_conf_matrix)

print("Confusion Matrix for Ridge Model:")
print(ridge_conf_matrix)



# Convert to data frame for ggplot
lasso_df <- as.data.frame(as.table(lasso_conf_matrix))
ridge_df <- as.data.frame(as.table(ridge_conf_matrix))


# Plot LASSO confusion matrix
ggplot(lasso_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  labs(
    title = "Confusion Matrix: LASSO Model",
    x = "Actual Outcome",
    y = "Predicted Outcome",
    fill = "Frequency"
  ) +
  theme_minimal()

# Plot Ridge confusion matrix
ggplot(ridge_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  labs(
    title = "Confusion Matrix: Ridge Model",
    x = "Actual Outcome",
    y = "Predicted Outcome",
    fill = "Frequency"
  ) +
  theme_minimal()



# Plot the logistic regression fit using ggplot
ggplot(data_standardized, aes(x = H__Higuchi_  , y = cause)) +
  #geom_point(aes(color = cause), alpha = 0.6, show.legend = F) +  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T, color = "black", show.legend = F) +  # Logistic regression curve
  labs(
    x = "\n H Higuchi",
    y = "Probability of Death \n"
  ) +
  theme_pubclean()


# Plot the logistic regression fit using ggplot
ggplot(data_standardized, aes(x = Hurst  , y = cause)) +
  #geom_point(aes(color = cause), alpha = 0.6, show.legend = F) +  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T, color = "black", show.legend = F) +  # Logistic regression curve
  labs(
    x = "\n Hurst",
    y = "Probability of Death \n"
  ) +
  theme_pubclean()


ggplot(data_standardized, aes(x = H__Higuchi_  , y = Followup_duration)) +
  # geom_point( alpha = 0.6) +  # Scatter plot of Entropy vs Death (cause)
  geom_smooth(method = "lm" , color = "black") +  # Logistic regression curve
  labs(
    x = "\n H Higuchi",
    y = "# Follow-up Months \n"
  ) +
  theme_pubclean()


ggplot(data_standardized, aes(x = Hurst  , y = Followup_duration)) +
  # geom_point( alpha = 0.6) +  # Scatter plot of Entropy vs Death (cause)
  geom_smooth(method = "lm" , color = "black") +  # Logistic regression curve
  labs(
    x = "\n Hurst",
    y = "# Follow-up Months \n"
  ) +
  theme_pubclean()






data_balanced <- data_standardized %>% filter(cause==0) %>%
  sample_n(129, replace = T) %>%
  bind_rows(data_standardized %>% filter(cause==1) %>%
              sample_n(129, replace = F))




library(glmnet)

# Prepare the data for glmnet (we exclude Followup_duration and cause as they are the response variables)
x <- as.matrix(data_balanced[, 1:50])  # HRV features
y <- Surv(data_balanced$Followup_duration, data_balanced$cause)  # Survival object

# Fit Cox model with LASSO regularization (alpha = 1 for LASSO)
lasso_model <- cv.glmnet(x, y, family = "cox", alpha = 1)

# Check the lambda that gives the minimum cross-validation error
lasso_model$lambda.min

# Get the coefficients for the model with the minimum lambda
coef(lasso_model, s = "lambda.min")


# Fit Cox model with Ridge regularization (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(x, y, family = "cox", alpha = 0)

# Check the lambda that gives the minimum cross-validation error
ridge_model$lambda.min

# Get the coefficients for the model with the minimum lambda
coef(ridge_model, s = "lambda.min")



# Predicted probabilities from the LASSO model
lasso_probs <- predict(lasso_model, newx = x, s = "lambda.min", type = "response")
lasso_pred <- ifelse(lasso_probs >= 0.5, 1, 0)  # Convert probabilities to binary outcomes

# Predicted probabilities from the Ridge model
ridge_probs <- predict(ridge_model, newx = x, s = "lambda.min", type = "response")
ridge_pred <- ifelse(ridge_probs >= 0.5, 1, 0)  # Convert probabilities to binary outcomes

# Actual outcomes
true_outcomes <- data_balanced$cause


# Create confusion matrices
lasso_conf_matrix <- table(Predicted = lasso_pred, Actual = true_outcomes)
ridge_conf_matrix <- table(Predicted = ridge_pred, Actual = true_outcomes)


# Print the confusion matrices
print("Confusion Matrix for LASSO Model:")
print(lasso_conf_matrix)

print("Confusion Matrix for Ridge Model:")
print(ridge_conf_matrix)

data.frame(ridge_probs) %>% bind_cols( data_balanced$cause) %>%
  rename("Died"="...2") %>% mutate(Died=ifelse(Died==0, "No", "Yes")) %>%
  ggplot(aes(X1, colour=Died, fill=Died)) +
  geom_density( adjust = 2,alpha=0.7) +
  xlab("\n Ridge Risk Score") + ylab("Patient density \n") +
  theme_pubclean() +
  scale_fill_manual(values=c("#0087fa", "#ff004f")) +
  scale_colour_manual(values=c("#0087fa", "#ff004f")) 


data.frame(lasso_probs) %>% bind_cols( data_balanced$cause) %>%
  rename("Died"="...2") %>% mutate(Died=ifelse(Died==0, "No", "Yes")) %>%
  ggplot(aes(X1, colour=Died, fill=Died)) +
  geom_density( adjust = 2,alpha=0.7) +
  xlab("\n LASSO Risk Score") + ylab("Patient density \n") +
  theme_pubclean() +
  scale_fill_manual(values=c("#0087fa", "#ff004f")) +
  scale_colour_manual(values=c("#0087fa", "#ff004f")) 


# -----------
# Heart rate variability metrics & UMSARS & and Death ------------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 30.5
)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst, Score_UMSARS1, Score_UMSARS2, Followup_duration, cause)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  mutate(UMSARS=Score_UMSARS1+Score_UMSARS2) %>%
  select(-c(Score_UMSARS1, Score_UMSARS2))


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(is.na(cause),0,cause))

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))

Marc_Anne_HR_Variability_MSA$Followup_duration <- Marc_Anne_HR_Variability_MSA$Followup_duration * (-1)

Marc_Anne_HR_Variability_MSA %>% group_by(cause) %>% summarise(mean=mean(UMSARS))

cor(Marc_Anne_HR_Variability_MSA$UMSARS, Marc_Anne_HR_Variability_MSA$Followup_duration)

# 10 lost months for each 10 points increase

Marc_Anne_HR_Variability_MSA %>%
  ggplot(aes(UMSARS, Followup_duration)) +
  geom_smooth(method="lm", colour="black")  +
  theme_minimal() +
  xlab("\n Total UMSARS (1+2)") + ylab("Time-to-death (months) \n") +
  theme_pubclean() 


Marc_Anne_HR_Variability_MSA %>%
  ggplot(aes(H__Higuchi_, Followup_duration)) +
  geom_smooth(method="lm", colour="black", fill="black", alpha=0.2)  +
  theme_minimal() +
  xlab("\n H Higuchi") + ylab("Time-to-death (months) \n") +
  theme_pubclean() 


Marc_Anne_HR_Variability_MSA %>%
  ggplot(aes(oneVperc, Followup_duration)) +
  geom_smooth(method="lm", colour="black", fill="black", alpha=0.2)  +
  theme_minimal() +
  xlab("\n 1V %") + ylab("Time-to-death (months) \n") +
  theme_pubclean() 


Marc_Anne_HR_Variability_MSA %>%
  ggplot(aes(Lempel_Ziv_Complexity__LZC_  , Followup_duration)) +
  geom_smooth(method="lm", colour="black", fill="black", alpha=0.2)  +
  theme_minimal() +
  xlab("\n Lempel Ziv Complexity") + ylab("Time-to-death (months) \n") +
  theme_pubclean() 



cor_matrix <- cor(Marc_Anne_HR_Variability_MSA %>% select(where(is.numeric)), method = "spearman")
print(cor_matrix)

names(Marc_Anne_HR_Variability_MSA)
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-cause)


library(glmnet)
library(caret)

# Assume the data is loaded in the "data" variable
# Extract relevant columns: HRV features (1:50), time-to-death, and UMSARS
data_relevant <- Marc_Anne_HR_Variability_MSA

# Standardize the HRV features
data_standardized <- data_relevant
data_standardized[, 1:50] <- scale(data_standardized[, 1:50])

# Define time-to-death and response variable
time_to_death <- data_standardized$Followup_duration
umsars <- data_standardized$UMSARS
hrv_features <- as.matrix(data_standardized[, 1:50])

umsars_scaled <- scale(umsars)
hrv_features_scaled <- hrv_features

umsars_scaled <- data.frame(umsars_scaled)

hrv_features_scaled <- data.frame(hrv_features_scaled)

hrv_features_scaled

hrv_features_scaled <- as.matrix(hrv_features_scaled)

dim(hrv_features_scaled)

# Fit LASSO model (alpha = 1 for LASSO)
lasso_model <- cv.glmnet(hrv_features_scaled, time_to_death, alpha = 1)

# Get the lambda that minimizes the cross-validation error for LASSO
lasso_lambda_min <- lasso_model$lambda.min
cat("Optimal lambda for LASSO: ", lasso_lambda_min, "\n")

# Get the coefficients for the LASSO model at lambda.min
lasso_coefficients <- coef(lasso_model, s = "lambda.min")
cat("LASSO Coefficients:\n")
print(lasso_coefficients)



# Fit Ridge model (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(hrv_features_scaled, time_to_death, alpha = 0)

# Get the lambda that minimizes the cross-validation error for Ridge
ridge_lambda_min <- ridge_model$lambda.min
cat("Optimal lambda for Ridge: ", ridge_lambda_min, "\n")

# Get the coefficients for the Ridge model at lambda.min
ridge_coefficients <- coef(ridge_model, s = "lambda.min")
cat("Ridge Coefficients:\n")
print(ridge_coefficients)

umsars_only <- data.frame(time_to_death) %>% bind_cols(data.frame(umsars_scaled))

lm_umsars <- lm(time_to_death ~ umsars_scaled, data=umsars_only)




# Predict using LASSO model (with scaled HRV features)
lasso_preds <- predict(lasso_model, newx = hrv_features_scaled, s = "lambda.min")

# Predict using Ridge model (with scaled HRV features)
ridge_preds <- predict(ridge_model, newx = hrv_features_scaled, s = "lambda.min")


# Predict using the linear model
lm_preds <- predict(lm_umsars, newdata = umsars_scaled)

# Now let's evaluate the models by calculating the Mean Squared Error (MSE)
mse_lasso <- mean((lasso_preds - time_to_death)^2)
mse_ridge <- mean((ridge_preds - time_to_death)^2)
mse_lm <- mean((lm_preds - time_to_death)^2)

cat("MSE for LASSO Model: ", mse_lasso, "\n")
cat("MSE for Ridge Model: ", mse_ridge, "\n")
cat("MSE for Linear Model (UMSARS only): ", mse_lm, "\n")

# Alternatively, you can compute R-squared for the models
r_squared_lasso <- 1 - sum((lasso_preds - time_to_death)^2) / sum((time_to_death - mean(time_to_death))^2)
r_squared_ridge <- 1 - sum((ridge_preds - time_to_death)^2) / sum((time_to_death - mean(time_to_death))^2)
r_squared_lm <- 1 - sum((lm_preds - time_to_death)^2) / sum((time_to_death - mean(time_to_death))^2)

cat("R-squared for LASSO Model: ", r_squared_lasso, "\n")
cat("R-squared for Ridge Model: ", r_squared_ridge, "\n")
cat("R-squared for Linear Model (UMSARS only): ", r_squared_lm, "\n")



# Calculate residuals for the LASSO model
lasso_residuals <- time_to_death - lasso_preds

# Calculate residuals for the Ridge model
ridge_residuals <- time_to_death - ridge_preds

# Calculate residuals for the linear model (UMSARS only)
lm_residuals <- time_to_death - lm_preds



data.frame(lm_residuals) %>%
  ggplot(aes(lm_residuals)) +
  geom_density( adjust = 1,alpha=0.5, colour="black", fill="black") +
  xlab("\n UMSARS Residual") + ylab("Patient density \n") +
  theme_pubclean() +
  coord_cartesian(xlim=c(-50,50))


data.frame(lasso_residuals) %>%
  ggplot(aes(lasso_residuals)) +
  geom_density( adjust = 1,alpha=0.5, colour="#ff004f", fill="#ff004f") +
  xlab("\n LASSO Residual") + ylab("Patient density \n") +
  theme_pubclean() +
  coord_cartesian(xlim=c(-50,50))


data.frame(ridge_residuals) %>%
  ggplot(aes(ridge_residuals)) +
  geom_density( adjust = 1,alpha=0.5, colour="#0087fa", fill="#0087fa") +
  xlab("\n Ridge Residual") + ylab("Patient density \n") +
  theme_pubclean() +
  coord_cartesian(xlim=c(-50,50))


data.frame(lasso_preds) %>%
  bind_cols(data.frame(time_to_death)) %>%
  ggplot(aes(x=`lambda.min`, y=time_to_death )) +
  geom_point(shape=1,stroke=2, colour="#ff004f") +
  geom_smooth(colour="black", fill="#ff004f", alpha=0.1) +
  theme_pubclean() +
  xlab("\n LASSO Predicitons") + ylab("Time-to-death \n") 




data.frame(ridge_preds) %>%
  bind_cols(data.frame(time_to_death)) %>%
  ggplot(aes(x=`lambda.min`, y=time_to_death )) +
  geom_point(shape=1,stroke=2, colour="#0087fa") +
  geom_smooth(colour="black", fill="#0087fa", alpha=0.1) +
  theme_pubclean() +
  xlab("\n Ridge Predicitons") + ylab("Time-to-death \n") 


data.frame(lm_preds) %>%
  bind_cols(data.frame(time_to_death)) %>%
  ggplot(aes(x=`lm_preds`, y=time_to_death )) +
  geom_point(shape=1,stroke=2, colour="black") +
  geom_smooth(colour="black", fill="black", alpha=0.1) +
  theme_pubclean() +
  xlab("\n UMSARS Predicitons") + ylab("Time-to-death \n") 




X <- umsars_scaled %>% bind_cols(hrv_features_scaled)
X <- as.matrix(X)


# Fit LASSO model (alpha = 1 for LASSO)
lasso_model <- cv.glmnet(X, time_to_death, alpha = 1)

# Get the lambda that minimizes the cross-validation error for LASSO
lasso_lambda_min <- lasso_model$lambda.min
cat("Optimal lambda for LASSO: ", lasso_lambda_min, "\n")

# Get the coefficients for the LASSO model at lambda.min
lasso_coefficients <- coef(lasso_model, s = "lambda.min")
cat("LASSO Coefficients:\n")
print(lasso_coefficients)



# Fit Ridge model (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(X, time_to_death, alpha = 0)

# Get the lambda that minimizes the cross-validation error for Ridge
ridge_lambda_min <- ridge_model$lambda.min
cat("Optimal lambda for Ridge: ", ridge_lambda_min, "\n")

# Get the coefficients for the Ridge model at lambda.min
ridge_coefficients <- coef(ridge_model, s = "lambda.min")
cat("Ridge Coefficients:\n")
print(ridge_coefficients)

umsars_only <- data.frame(time_to_death) %>% bind_cols(data.frame(umsars_scaled))

lm_umsars <- lm(time_to_death ~ umsars_scaled, data=umsars_only)




# Predict using LASSO model (with scaled HRV features)
lasso_preds <- predict(lasso_model, newx = X, s = "lambda.min")

# Predict using Ridge model (with scaled HRV features)
ridge_preds <- predict(ridge_model, newx = X, s = "lambda.min")


# Predict using the linear model
lm_preds <- predict(lm_umsars, newdata = umsars_scaled)

# Now let's evaluate the models by calculating the Mean Squared Error (MSE)
mse_lasso <- mean((lasso_preds - time_to_death)^2)
mse_ridge <- mean((ridge_preds - time_to_death)^2)
mse_lm <- mean((lm_preds - time_to_death)^2)

cat("MSE for LASSO Model: ", mse_lasso, "\n")
cat("MSE for Ridge Model: ", mse_ridge, "\n")
cat("MSE for Linear Model (UMSARS only): ", mse_lm, "\n")

# Alternatively, you can compute R-squared for the models
r_squared_lasso <- 1 - sum((lasso_preds - time_to_death)^2) / sum((time_to_death - mean(time_to_death))^2)
r_squared_ridge <- 1 - sum((ridge_preds - time_to_death)^2) / sum((time_to_death - mean(time_to_death))^2)
r_squared_lm <- 1 - sum((lm_preds - time_to_death)^2) / sum((time_to_death - mean(time_to_death))^2)

cat("R-squared for LASSO Model: ", r_squared_lasso, "\n")
cat("R-squared for Ridge Model: ", r_squared_ridge, "\n")
cat("R-squared for Linear Model (UMSARS only): ", r_squared_lm, "\n")





data.frame(lasso_preds) %>%
  bind_cols(data.frame(time_to_death)) %>%
  ggplot(aes(x=`lambda.min`, y=time_to_death )) +
  geom_point(shape=1,stroke=2, colour="black") +
  geom_smooth(colour="black", fill="black", alpha=0.1) +
  theme_pubclean() +
  xlab("\n LASSO [+UMSARS] Predicitons") + ylab("Time-to-death \n") 




cor_matrix <- cor(data.frame(X) %>% select(where(is.numeric)), method = "spearman")
print(cor_matrix)



ignore <- data.frame(X) %>% bind_cols(data.frame(time_to_death))

set.seed(1)
regit_full <- leaps::regsubsets(time_to_death ~ . , data=ignore, nvmax = 51, really.big=T)
reg_summary <- summary(regit_full)

ignore <- data.frame(reg_summary$which)

fwrite(ignore, "ignore_2.csv")




Best_Subset_Predictors <- fread("Best_Subset_Preds_HR_UMSARS_v2.csv")

names(Best_Subset_Predictors)

light_blue = rgb(0/255, 135/255, 250/255)  # Light blue: RGB(0, 135, 250)
light_pink = rgb(255/255, 0/255, 79/255)   # Light pink: RGB(255, 0, 79)

Best_Subset_Predictors %>% gather(Var, Pres, UMSARS:`Hurst`) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=vars , y=Var, fill = Predictor_Included)) + 
  geom_tile(color = "snow", size = 0.1, show.legend = F) + 
  scale_fill_manual( values= c("snow", light_pink) ) +
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  coord_equal() + 
  theme_minimal() +
  # scale_x_continuous(breaks = seq(min(Best_Subset_Predictors$vars),max(Best_Subset_Predictors$vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Number of Predictors") +ylab("Predictor Included (yes/no)")




# Plot RSS, Adjusted R², Cp, and BIC

# Define the colors for the alternating lines
light_blue = rgb(0/255, 135/255, 250/255)  # Light blue: RGB(0, 135, 250)
light_pink = rgb(255/255, 0/255, 79/255)   # Light pink: RGB(255, 0, 79)

# Set up the plot layout with 2 rows and 2 columns
par(mfrow = c(2, 2))  # Arrange plots in a grid

# Plot RSS with alternating colors and thicker lines
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", lwd = 3, col = light_pink)
# # Plot Adjusted R² with alternating colors and thicker lines
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l", lwd = 3, col = light_blue)
# # Plot Cp with alternating colors and thicker lines
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", lwd = 3, col = light_blue)
# # Plot BIC with alternating colors and thicker lines
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 3, col = light_pink)




# -----------

# Heart rate variability metrics & UMSARS & and Death MEDIATION ANALYSIS ------------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 30.5
)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst, Score_UMSARS1, Score_UMSARS2, Followup_duration, cause)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  mutate(UMSARS=Score_UMSARS1+Score_UMSARS2) %>%
  select(-c(Score_UMSARS1, Score_UMSARS2))


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(is.na(cause),0,cause))

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))

Marc_Anne_HR_Variability_MSA$Followup_duration <- Marc_Anne_HR_Variability_MSA$Followup_duration * (-1)

Marc_Anne_HR_Variability_MSA %>% group_by(cause) %>% summarise(mean=mean(UMSARS))

cor(Marc_Anne_HR_Variability_MSA$UMSARS, Marc_Anne_HR_Variability_MSA$Followup_duration)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-cause)


library(glmnet)
library(caret)

# Assume the data is loaded in the "data" variable
# Extract relevant columns: HRV features (1:50), time-to-death, and UMSARS
data_relevant <- Marc_Anne_HR_Variability_MSA

# Standardize the HRV features
data_standardized <- data_relevant
data_standardized[, 1:50] <- scale(data_standardized[, 1:50])

# Define time-to-death and response variable
time_to_death <- data_standardized$Followup_duration
umsars <- data_standardized$UMSARS
hrv_features <- as.matrix(data_standardized[, 1:50])

umsars_scaled <- scale(umsars)
hrv_features_scaled <- hrv_features

umsars_scaled <- data.frame(umsars_scaled)

hrv_features_scaled <- data.frame(hrv_features_scaled)

hrv_features_scaled

hrv_features_scaled <- as.matrix(hrv_features_scaled)

dim(hrv_features_scaled)

# Fit models (alpha = 1 for LASSO)
lasso_model <- cv.glmnet(hrv_features_scaled, time_to_death, alpha = 1)

ridge_model <- cv.glmnet(hrv_features_scaled, time_to_death, alpha = 0)

# Predict using LASSO model (with scaled HRV features)
lasso_preds <- predict(lasso_model, newx = hrv_features_scaled, s = "lambda.min")
ridge_preds <- predict(ridge_model, newx = hrv_features_scaled, s = "lambda.min")


time_to_death <- scale(time_to_death)

library(mediation)

# Step 1: Total Effect
model_total <- lm(time_to_death ~ lasso_preds)

# Step 2: Mediator Path
model_mediator <- lm(as.matrix(umsars_scaled) ~ lasso_preds)

# Step 3: Direct and Indirect Effects
model_direct <- lm(time_to_death ~ lasso_preds + as.matrix(umsars))

# Mediation Analysis
mediate_result <- mediate(model_mediator, model_direct, treat = "lasso_preds", mediator = "as.matrix(umsars)")

summary(mediate_result)





# Step 1: Total Effect
model_total <- lm(time_to_death ~ ridge_preds)

# Step 2: Mediator Path
model_mediator <- lm(as.matrix(umsars_scaled) ~ ridge_preds)

# Step 3: Direct and Indirect Effects
model_direct <- lm(time_to_death ~ ridge_preds + as.matrix(umsars))

# Mediation Analysis
mediate_result <- mediate(model_mediator, model_direct, treat = "ridge_preds", mediator = "as.matrix(umsars)")

summary(mediate_result)


# -----------
# plot regression fit Total Followup duration ~ Each HR variability metric ---------
Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 30.5
)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst,  Score_UMSARS1, Score_UMSARS2, Followup_duration, cause)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, Followup_duration, `ms/mmHg`:Hurst,  Score_UMSARS1, Score_UMSARS2) %>%
  mutate(UMSARS=Score_UMSARS1+Score_UMSARS2) %>%
  select(-c(Score_UMSARS1, Score_UMSARS2))


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))

Marc_Anne_HR_Variability_MSA$Followup_duration <- -1 * Marc_Anne_HR_Variability_MSA$Followup_duration


names(Marc_Anne_HR_Variability_MSA)


# 400 400
Marc_Anne_HR_Variability_MSA %>%
  ggplot(aes(Hurst, Followup_duration)) +
  geom_jitter(alpha=0.6,colour="#0087fa", shape=1, stroke=2, size=0.8) +
  geom_smooth(fill="#ff004f", colour="#ff004f", alpha=0.1) +
  theme_light() +
  ylab("Follow-up Duration") + xlab("Hurst")

# ---------

# Correlation symptom/disease progression and HRV ---------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)


dim(Marc_Anne_HR_Variability_MSA)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA$Score_UMSARS1_total
Marc_Anne_HR_Variability_MSA$Score_UMSARS_2_total
Marc_Anne_HR_Variability_MSA$AMS_27___score_global_COMPASS <- as.numeric(Marc_Anne_HR_Variability_MSA$AMS_27___score_global_COMPASS)

paste0(names(Marc_Anne_HR_Variability_MSA)[6:55], collapse = '","')

unique(Marc_Anne_HR_Variability_MSA$cause)
unique(Marc_Anne_HR_Variability_MSA$date_)
unique(Marc_Anne_HR_Variability_MSA$AMS__011___Année_d_apparition_1er_symptome_maladie)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

Marc_Anne_HR_Variability_MSA$Date_de_l_examen
Marc_Anne_HR_Variability_MSA$date_

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 30.5
)



# Extract the year from Date_de_l_examen
Marc_Anne_HR_Variability_MSA$Year_de_l_examen <- as.numeric(format(as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen), "%Y"))

# Ensure AMS__011___Année_d_apparition_1er_symptome_maladie is numeric
Marc_Anne_HR_Variability_MSA$Year_1st_symptom <- as.numeric(Marc_Anne_HR_Variability_MSA$`AMS__011___Année_d_apparition_1er_symptome_maladie`)

# Calculate the difference
Marc_Anne_HR_Variability_MSA$Symptom_duration <- Marc_Anne_HR_Variability_MSA$Year_de_l_examen - Marc_Anne_HR_Variability_MSA$Year_1st_symptom

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(patid, Symptom_duration, `ms/mmHg`:Hurst, Score_UMSARS1, Score_UMSARS2)
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(UMSARS=Score_UMSARS1+Score_UMSARS2) %>%
  select(-c(Score_UMSARS1, Score_UMSARS2))

names(Marc_Anne_HR_Variability_MSA)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>%
  mutate(across(where(is.numeric), scale))


cor_matrix <- cor(Marc_Anne_HR_Variability_MSA %>% select(where(is.numeric)), method = "spearman")
cor_matrix <- data.frame(cor_matrix)
data.frame(cor_matrix[,1]) %>% bind_cols(data.frame(row.names(cor_matrix)))



names(Marc_Anne_HR_Variability_MSA)






# PCA
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-UMSARS)


Symptom_duration <- Marc_Anne_HR_Variability_MSA %>% select(Symptom_duration)
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-Symptom_duration)
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

names(Marc_Anne_HR_Variability_MSA)


# Step 1: Perform PCA
pca_result <- prcomp(Marc_Anne_HR_Variability_MSA, scale. = TRUE)

# Step 2: Extract loadings
loadings <- pca_result$rotation

# Step 3: Create a matrix of loadings
loadings_matrix <- as.data.frame(loadings)

data.frame(row.names(loadings))

fwrite(loadings_matrix, "loadings_matrix_sympt.csv")

# Step 4: Correlate PCs with external variable
# Extract scores (principal component values for observations)
pc_scores <- pca_result$x

# Compute correlations
correlations <- apply(pc_scores, 2, function(pc) cor(pc, Symptom_duration))

data.frame(correlations)

# Print results
print("Loadings matrix:")
print(loadings_matrix)

print("Correlations of PCs with external variable:")
print(correlations)

# -----------
# See Delta PAS PAD over disease progression --------------
Deltas_PAS_PAD <- fread("Deltas_PAS_PAD.txt")


Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))

# Convert the columns to Date format
Marc_Anne_HR_Variability_MSA$Date_de_l_examen <- as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen)
Marc_Anne_HR_Variability_MSA$date_ <- as.Date(Marc_Anne_HR_Variability_MSA$date_)

# Calculate the difference in years
Marc_Anne_HR_Variability_MSA$Followup_duration <- as.numeric(
  difftime(Marc_Anne_HR_Variability_MSA$Date_de_l_examen, 
           Marc_Anne_HR_Variability_MSA$date_, 
           units = "days") / 30.5
)

# Extract the year from Date_de_l_examen
Marc_Anne_HR_Variability_MSA$Year_de_l_examen <- as.numeric(format(as.Date(Marc_Anne_HR_Variability_MSA$Date_de_l_examen), "%Y"))

# Ensure AMS__011___Année_d_apparition_1er_symptome_maladie is numeric
Marc_Anne_HR_Variability_MSA$Year_1st_symptom <- as.numeric(Marc_Anne_HR_Variability_MSA$AMS__011___Année_d_apparition_1er_symptome_maladie)

# Calculate the difference
Marc_Anne_HR_Variability_MSA$Symptom_duration <- Marc_Anne_HR_Variability_MSA$Year_de_l_examen - Marc_Anne_HR_Variability_MSA$Year_1st_symptom

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(patid, Symptom_duration, `ms/mmHg`:Hurst, Score_UMSARS1, Score_UMSARS2)
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(UMSARS=Score_UMSARS1+Score_UMSARS2) %>%
  select(-c(Score_UMSARS1, Score_UMSARS2))

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% inner_join(Deltas_PAS_PAD) 

cor(Marc_Anne_HR_Variability_MSA$Symptom_duration, Marc_Anne_HR_Variability_MSA$Delta_PAS)
cor(Marc_Anne_HR_Variability_MSA$Symptom_duration, Marc_Anne_HR_Variability_MSA$Delta_PAD)



Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>%
  mutate(across(where(is.numeric), scale))


cor_matrix <- cor(Marc_Anne_HR_Variability_MSA %>% select(where(is.numeric)), method = "spearman")
cor_matrix <- data.frame(cor_matrix)
data.frame(cor_matrix[,1]) %>% bind_cols(data.frame(row.names(cor_matrix)))

# -----------------
# Heart rate variability Compare Prob vs Poss and MSC vs MSP ------------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

names(Marc_Anne_HR_Variability_MSA)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst,  `AMS__017_-__Diagnostic_AMS`)


#Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>%
#  mutate(across(where(is.numeric), scale))

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(Dx=ifelse( grepl("Possible",`AMS__017_-__Diagnostic_AMS`), 0, 1))

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-`AMS__017_-__Diagnostic_AMS`)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))



library(randomForest)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()




rf_model <- randomForest(as.factor(Dx) ~ ., data = Marc_Anne_HR_Variability_MSA, importance = TRUE)

print(rf_model)

# Get Feature Importance
importance_values <- importance(rf_model)

data.frame(importance_values) %>% arrange(-MeanDecreaseAccuracy )

Marc_Anne_HR_Variability_MSA %>%
  group_by(Dx) %>% summarise(mean=mean(Mean_RR__ms_))

results <- Marc_Anne_HR_Variability_MSA %>%
  group_by(Dx) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) 


wilcox_results <- Marc_Anne_HR_Variability_MSA %>%
  select(-Dx) %>%
  map_df(~{
    test <- wilcox.test(
      .[Marc_Anne_HR_Variability_MSA$Dx == 0],
      .[Marc_Anne_HR_Variability_MSA$Dx == 1]
    )
    tibble(statistic = test$statistic, p_value = test$p.value)
  }, .id = "Feature")

# Add feature names to results
wilcox_results$Feature <- colnames(Marc_Anne_HR_Variability_MSA)[-51]

# Display results
data.frame(wilcox_results)


library(glmnet)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>%
  mutate(Dx = ifelse(Dx == 1, "MSA-C", "MSA-P"))


Marc_Anne_HR_Variability_MSA %>%
  ggplot(aes( VLF__mstwo_, colour=Dx, fill=Dx)) +
  geom_density(alpha=0.5) +
  theme_pubclean() +
  scale_fill_manual(values=c("#0087fa", "#ff004f")) +
  scale_colour_manual(values=c("#0087fa", "#ff004f")) +
  ylab("Patient density")


# Function to create density plots for each feature
create_density_plot <- function(feature_name) {
  ggplot(Marc_Anne_HR_Variability_MSA, aes_string(x = feature_name, colour = "Dx", fill = "Dx")) +
    geom_density(alpha = 0.5) +
    theme_pubclean() +
    scale_fill_manual(values = c("#0087fa", "#ff004f")) +
    scale_colour_manual(values = c("#0087fa", "#ff004f")) +
    labs(y = "Patient density", title = feature_name)
}

names(Marc_Anne_HR_Variability_MSA)

# Generate density plots for all columns except Dx
feature_names <- colnames(Marc_Anne_HR_Variability_MSA)[-51]  # Exclude Dx

plots <- map(feature_names, create_density_plot)




install.packages("randomForest")
install.packages("caret")
install.packages("ROSE")  # for upsampling (if you prefer SMOTE or other techniques)


library(randomForest)
library(caret)
library(ROSE)
library(pROC)


Marc_Anne_HR_Variability_MSA$Dx <- as.factor(Marc_Anne_HR_Variability_MSA$Dx)

# Separate the features (all columns except the last one) and the target variable (Dx)
features <- Marc_Anne_HR_Variability_MSA[, -ncol(Marc_Anne_HR_Variability_MSA)]
target <- Marc_Anne_HR_Variability_MSA$Dx

# Apply ROSE to upsample the smaller class
set.seed(123)  # Set a random seed for reproducibility
upsampled_data <- ROSE(Dx ~ ., data = Marc_Anne_HR_Variability_MSA, seed = 123)$data

# Ensure the target variable is a factor after SMOTE
upsampled_data$Dx <- as.factor(upsampled_data$Dx)

# Create a random forest model with Leave-One-Out Cross-Validation (LOOCV)
# We'll use caret's train function to handle LOOCV
train_control <- trainControl(method = "LOOCV")

# Random Forest Model
rf_model <- train(Dx ~ ., data = upsampled_data, method = "rf", trControl = train_control)

# Evaluate the model's performance
print(rf_model)


# Optionally, check feature importance
importance <- randomForest(Dx ~ ., data = upsampled_data)
print(importance)

ignore <- rf_model$pred

library(tidyverse)

ignore %>% filter(mtry==26) %>%
  group_by(pred, obs) %>% count() 

upsampled_data %>% group_by(Dx) %>% count()

# Get predicted probabilities for ROC curve (we need probabilities, not just class labels)
rf_probabilities <- predict(rf_model, upsampled_data, type = "prob")

# Calculate the ROC curve and AUC for the positive class (usually class '1' or the second class)
roc_curve <- roc(upsampled_data$Dx, rf_probabilities[,2])  # Index 2 for the probability of the second class

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "gray", lty = 2)  # Add diagonal line (random model)

# Calculate and display AUC
auc_value <- auc(roc_curve)
cat("AUC: ", auc_value, "\n")




# ------------------

# Heart rate variability metrics and Systolic BP Delta ------------------

Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

Deltas_PAS_PAD <- fread("Deltas_PAS_PAD.txt")

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% left_join(Deltas_PAS_PAD)

names(Marc_Anne_HR_Variability_MSA)

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% mutate(cause=ifelse(cause=="Décès",1,0))



Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst,  Delta_PAS)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))


library(leaps)
library(glmnet)
library(car)


fwrite(Marc_Anne_HR_Variability_MSA, "data_delta_pas.csv")


# Ensure predictors are scaled
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>%
  mutate(across(where(is.numeric), scale))

unique(Marc_Anne_HR_Variability_MSA$Delta_PAS)
# Correlation matrix
cor_matrix <- cor(Marc_Anne_HR_Variability_MSA %>% select(where(is.numeric)), method = "spearman")
print(cor_matrix)

data.frame(cor_matrix) %>% select(Delta_PAS)



# Best Subset Selection
set.seed(1)
regit_full <- regsubsets(Delta_PAS ~ ., data = Marc_Anne_HR_Variability_MSA, nvmax = 50, really.big=T)
reg_summary <- summary(regit_full)

ignore <- data.frame(reg_summary$which)

fwrite(ignore, "ignore.csv")


Best_Subset_Predictors <- fread("Best_Subset_Preds_HR_DeltaPAS.csv")

names(Best_Subset_Predictors)

light_blue = rgb(0/255, 135/255, 250/255)  # Light blue: RGB(0, 135, 250)
light_pink = rgb(255/255, 0/255, 79/255)   # Light pink: RGB(255, 0, 79)

Best_Subset_Predictors %>% gather(Var, Pres, `ms/mmHg`:`Hurst`) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=N_vars , y=Var, fill = Predictor_Included)) + 
  geom_tile(color = "snow", size = 0.1, show.legend = F) + 
  scale_fill_manual( values= c("snow", light_pink) ) +
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  coord_equal() + 
  theme_minimal() +
  # scale_x_continuous(breaks = seq(min(Best_Subset_Predictors$vars),max(Best_Subset_Predictors$vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Number of Predictors") +ylab("Predictor Included (yes/no)")



# Plot RSS, Adjusted R², Cp, and BIC

# Define the colors for the alternating lines
light_blue = rgb(0/255, 135/255, 250/255)  # Light blue: RGB(0, 135, 250)
light_pink = rgb(255/255, 0/255, 79/255)   # Light pink: RGB(255, 0, 79)

# Set up the plot layout with 2 rows and 2 columns
par(mfrow = c(2, 2))  # Arrange plots in a grid

# Plot RSS with alternating colors and thicker lines
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", lwd = 3, col = light_pink)
# # Plot Adjusted R² with alternating colors and thicker lines
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l", lwd = 3, col = light_blue)
# # Plot Cp with alternating colors and thicker lines
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", lwd = 3, col = light_blue)
# # Plot BIC with alternating colors and thicker lines
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 3, col = light_pink)




# Ensure predictors are scaled
X <- model.matrix(Delta_PAS ~ ., data = Marc_Anne_HR_Variability_MSA)[, -1]  # Design matrix (exclude intercept)
y <- Marc_Anne_HR_Variability_MSA$Delta_PAS  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

# Plot LASSO cross-validation results
plot(lasso_cv)
title("LASSO Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.min")
print(lasso_coeffs)



# Ridge Regression (alpha = 0)
set.seed(1)
ridge_cv <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

# Best lambda
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Plot Ridge cross-validation results
plot(ridge_cv)
title("Ridge Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
ridge_coeffs <- coef(ridge_cv, s = "lambda.min")
print(ridge_coeffs)

# Compare MSE
lasso_mse <- min(lasso_cv$cvm)
ridge_mse <- min(ridge_cv$cvm)
print(paste("LASSO MSE:", lasso_mse))
print(paste("Ridge MSE:", ridge_mse))


# ------------------

# plot regression fit Delta SBP ~ Each HR variability metric ---------
Marc_Anne_HR_Variability_MSA <- read_xlsx(path="Marc_Anne_HR_Variability_MSA.xlsx", trim_ws = TRUE)

Deltas_PAS_PAD <- fread("Deltas_PAS_PAD.txt")

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% left_join(Deltas_PAS_PAD)


Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% 
  select(patid, `ms/mmHg`:Hurst, Delta_PAS )

Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% drop_na()
Marc_Anne_HR_Variability_MSA <- Marc_Anne_HR_Variability_MSA %>% select(-patid)

colnames(Marc_Anne_HR_Variability_MSA) = gsub("-", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("/", "_", colnames(Marc_Anne_HR_Variability_MSA))

colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\(", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("\\)", "_", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("%", "perc", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("²", "2", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("1", "one", colnames(Marc_Anne_HR_Variability_MSA))
colnames(Marc_Anne_HR_Variability_MSA) = gsub("2", "two", colnames(Marc_Anne_HR_Variability_MSA))

names(Marc_Anne_HR_Variability_MSA)


# 400 400

create_density_plot <- function(feature_name) {
  ggplot(Marc_Anne_HR_Variability_MSA, aes(x = !!sym(feature_name), y=Delta_PAS )) +
    geom_jitter(alpha=0.6,colour="#0087fa", shape=1, stroke=2, size=0.8) +
    geom_smooth(fill="#ff004f", colour="#ff004f", alpha=0.1) +
    theme_light() +
    labs(y = "SBP Delta", x=feature_name , title = feature_name)
}

names(Marc_Anne_HR_Variability_MSA)

# Generate density plots for all columns except Dx
feature_names <- colnames(Marc_Anne_HR_Variability_MSA)

plots <- map(feature_names, create_density_plot)


# ---------
