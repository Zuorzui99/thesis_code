# GLMM tree fitting 

# libraries ---------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)
library(glmertree)
library(partykit)
library(lme4)


# Data --------------------------------------------------------
data <- fread("data_long_filtered_new.csv ")

data <- data %>%
  distinct(across(-deviceold), .keep_all = TRUE)

# excluding the observations that are -100 as these are technical problems 
data <- data[data$percentage_improvement != -100, ]

# transforming categorical variables to factor 
data$gender <- as.factor(data$gender)
data$dropout <- as.factor(data$dropout)
data$confirmed <- as.factor(data$confirmed)
data$car_license_combined <- as.factor(data$car_license_combined)
data$deviceold <- as.factor(data$deviceold)
data$age_grp <- as.factor(data$age_grp)
data$brand <- as.factor(data$brand)
data$migration_background <- as.factor(data$migration_background)
data$income_class <- as.factor(data$income_class)


# rescaling continuous variables
data$income <- scale(data$income)
data$standardized_battery_drainage <- scale(data$standardized_battery_drainage)
data$total_duration_day1 <- scale(data$total_duration_day1)
data$average_labelling_time <- scale(data$average_labelling_time)
data$avg_time_confirmed <- scale(data$avg_time_confirmed)
data$time_quest <- scale(data$time_quest)



# for active days and users 
threshold <- 3

data_new <- data %>%
  group_by(user_id) %>%
  mutate(active_day = ifelse(percentage_change == 0, 0, 1),
         sum_inactive_days = sum(percentage_change == 0),
         active_user = ifelse(sum_inactive_days > threshold, 0, 1)) %>%
  ungroup()

data_new$active_day <- as.factor(data_new$active_day)
data_new$active_user <- as.factor(data_new$active_user)

active_data <- subset(data_new, active_day == 1)


# ------------------------------------------------------------------------------
# glmm -tree with gaussian and intercept only in node specific model
# testing for different sets of variables 
# !(not used in the analysis, was just to understand how the model works) 

glmm_tree_model1 <- lmertree(
  percentage_change ~ 1
  | (Day | user_id) | 
    age_grp + gender + income + car_license_combined +
    migration_background  ,
  data = data
)
plot(glmm_tree_model1)


glmm_tree_model2 <- lmertree(
  percentage_change ~ 1
  | (Day | user_id) | 
    brand + deviceold + standardized_battery_drainage ,
  data = data
)
plot(glmm_tree_model2)


glmm_tree_model3 <- lmertree(
  percentage_change ~ 1
  | (Day | user_id) | 
    confirmed + dropout+ total_duration_day1,
  data = data
)
plot(glmm_tree_model3)


glmm_tree_model4 <- lmertree(
  percentage_change ~ 1
  | (Day | user_id) | 
    average_labelling_time + time_quest + avg_time_confirmed,
  data = data
)

plot(glmm_tree_model4)



# GLMM-tree on binary outcome variable for active days 
# only splitting variables 
# no random slope -> like with traditional GLMM, it leads to convergence issues

glmm_tree_model_bin <- glmertree(
  active_day ~  1
  | (1 | user_id) | 
    migration_background +age_grp + gender +brand + deviceold + confirmed + dropout +
    average_labelling_time + time_quest + avg_time_confirmed + total_duration_day1  +  standardized_battery_drainage,
  family = "binomial",
  data = data_new
)

plot(glmm_tree_model_bin, type = "simple", which = "tree")
plot(glmm_tree_model_bin)

tot_duration_day1_value <- -0.708
mean_duration <- attr(data$average_labelling_time, "scaled:center")
sd_duration <- attr(data$average_labelling_time, "scaled:scale")

original_duration_day1 <- tot_duration_day1_value * sd_duration + mean_duration



# GLMM-tree on non-zero part of the data 
glmm_tree_model_full <- lmertree(
  percentage_improvement ~  
    average_labelling_time + time_quest + avg_time_confirmed + total_duration_day1  +  standardized_battery_drainage
  | (Day | user_id) | 
    migration_background +age_grp + gender +brand + deviceold + confirmed + dropout ,
  data = active_data
)

#plot(glmm_tree_model_full)
plot(glmm_tree_model_full, type = "simple", which = "tree")
plot(glmm_tree_model_full, which = "tree.coef")





# inspecting residual:

data_new <- data[,c("user_id", "age_grp", "gender", "income", "car_license_combined", 
                    "migration_background", "brand", "deviceold", "standardized_battery_drainage", "confirmed", "dropout", 
                    "total_duration_day1",
                    "average_labelling_time", "time_quest", "avg_time_confirmed")]
data_complete <- data_new[complete.cases(data_new),]


resids2 <- residuals(glmm_tree_model_full)
preds2 <- predict(glmm_tree_model_full)
plot(data_complete$user_id, resids2)
scatter.smooth(preds2, resids2)



