# This is the script of the Hurdle model to answer the first research question 


# libraries ------------------------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)
library(censReg)
library(plm)
library(glmmML)
library(maxLik)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(car)
library(tidyr)
library(sjstats)
library(DHARMa)

# Data ------------------------------------------------------------------------
data <- fread("data_long_filtered_new.csv ")

data <- data %>%
  distinct(across(-deviceold), .keep_all = TRUE)

# excluding the observations that are -100 as these are technical problems 
data <- data[data$percentage_change != -100, ]


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






#--------------------------------------------------------------------------------

# determine the inactive users, by first determining the inactive days and then depending on the amount
# of inactive days determine the inactive users

threshold <- 3

data_new <- data %>%
  group_by(user_id) %>%
  mutate(active_day = ifelse(percentage_change == 0, 0, 1),
         sum_inactive_days = sum(percentage_change == 0),
         active_user = ifelse(sum_inactive_days > threshold, 0, 1)) %>%
  ungroup()

data_new$active_day <- as.factor(data_new$active_day)
data_new$active_user <- as.factor(data_new$active_user)





#---------------------------------------------------------------------------

# Strategy: 
# 1. Fit a logistic model predicting if a user is active or not on a day (0/1)


activity_distribution <- data_new %>%
  group_by(Day) %>%
  summarise(inactive_users = sum(active_day == 0), 
            active_users = sum(active_day == 1))

data_long <- activity_distribution %>%
  pivot_longer(cols = c(active_users, inactive_users), names_to = "Status", values_to = "Count")


# plots of activity distribution 
ggplot(data_long, aes(x = as.factor(Day), y = Count, fill = Status))+
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(
    x = "Day", 
    y = "Number of Users",
    fill = "User Status") +
  scale_fill_discrete(labels = c("inactive_users" = "Not Active", "active_users" = "Active"))+
  theme_minimal(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Binary variable (User Activity)

data_new$Day_factor <- as.factor(data_new$Day)

table_active_day <- table(data_new$Day_factor, data_new$active_day)
prop_table_active_day <- prop.table(table_active_day, 1) * 100
print(round(prop_table_active_day,2))







# MODEL FITTING (LOGISTIC GLMM) -------------------------------------------------------

set.seed(123)

# Null-model: random intercept
random_intercept_model <-  glmmTMB(active_day ~ 1 + (1|user_id),
                                   data = data_new, 
                                   family = binomial
)
summary(random_intercept_model)



# random intercept and slope model

random_intercept_slope <-  glmmTMB(active_day ~ 1 + (Day|user_id) ,
                                   data = data_new, 
                                   family = binomial
)

summary(random_intercept_slope)
# Convergence problem, random effect structure to complicated for the data -> convergance problems




# 1. Model: Background Characteristics 
logistic_model1 <- glmmTMB(active_day ~ Day + gender + income_class + car_license_combined + migration_background + age_grp +
                             (1|user_id), 
                           family = binomial, 
                           data = data_new)

(logistic1_summary <- summary(logistic_model1))
coefs_log1 <- coef(logistic1_summary)
estimates_log1 <- coefs_log1$cond[, "Estimate"]
std_errors_log1 <- coefs_log1$cond[, "Std. Error"]

lower_log_ci_log1 <- estimates_log1 - 1.96*std_errors_log1
upper_log_ci_log1 <- estimates_log1 + 1.96*std_errors_log1

(lower_ci_log1 <- exp(lower_log_ci_log1))
(upper_ci_log1 <- exp(upper_log_ci_log1))

# exponentiated coeffiecients
(exp_coef1 <- exp(estimates_log1))



# 2. Model: Technical Profile  
logistic_model2 <- glmmTMB(active_day ~ Day + brand + deviceold + standardized_battery_drainage +
                             (1|user_id), 
                           family = binomial, 
                           data = data_new)

(logistic2_summary <- summary(logistic_model2))
coefs_log2 <- coef(logistic2_summary)
estimates_log2 <- coefs_log2$cond[, "Estimate"]
std_errors_log2 <- coefs_log2$cond[, "Std. Error"]

lower_log_ci_log2 <- estimates_log2 - 1.96*std_errors_log2
upper_log_ci_log2 <- estimates_log2 + 1.96*std_errors_log2

(lower_ci_log2 <- exp(lower_log_ci_log2))
(upper_ci_log2 <- exp(upper_log_ci_log2))

# exponentiated coefficients
(exp_coef2 <- exp(estimates_log2))


# 3. Model: Motivation  

logistic_model3 <- glmmTMB(active_day ~ Day + confirmed + dropout + total_duration_day1 +
                             (1|user_id), 
                           family = binomial, 
                           data = data_new)

(logistic3_summary <- summary(logistic_model3))
coefs_log3 <- coef(logistic3_summary)
estimates_log3 <- coefs_log3$cond[, "Estimate"]
std_errors_log3 <- coefs_log3$cond[, "Std. Error"]

lower_log_ci_log3 <- estimates_log3 - 1.96*std_errors_log3
upper_log_ci_log3 <- estimates_log3 + 1.96*std_errors_log3

(lower_ci_log3 <- exp(lower_log_ci_log3))
(upper_ci_log3 <- exp(upper_log_ci_log3))

(exp_coef3 <- exp(estimates_log3))


# 4. Model: Ability 

logistic_model4 <- glmmTMB(active_day ~ Day + avg_time_confirmed + average_labelling_time + time_quest +
                             (1|user_id), 
                           family = binomial, 
                           data = data_new)

(logistic4_summary <- summary(logistic_model4))
coefs_log4 <- coef(logistic4_summary)
estimates_log4 <- coefs_log4$cond[, "Estimate"]
std_errors_log4 <- coefs_log4$cond[, "Std. Error"]

lower_log_ci_log4 <- estimates_log4 - 1.96*std_errors_log4
upper_log_ci_log4 <- estimates_log4 + 1.96*std_errors_log4

(lower_ci_log4 <- exp(lower_log_ci_log4))
(upper_ci_log4 <- exp(upper_log_ci_log4))

(exp_coef4 <- exp(estimates_log4))


# Full model 
logistic_model_full <- glmmTMB(active_day ~ Day + gender + income_class + car_license_combined + migration_background + age_grp +
                                 brand +  standardized_battery_drainage + deviceold +
                                 confirmed + dropout + total_duration_day1 +
                                 avg_time_confirmed + average_labelling_time + time_quest +
                                 (1|user_id), 
                               family = binomial, 
                               data = data_new)

(logistic_full_summary <- summary(logistic_model_full))
coefs_log_full <- coef(logistic_full_summary)
estimates_log_full <- coefs_log_full$cond[, "Estimate"]
std_errors_log_full <- coefs_log_full$cond[, "Std. Error"]

lower_log_ci_log_full <- estimates_log_full - 1.96*std_errors_log_full
upper_log_ci_log_full <- estimates_log_full + 1.96*std_errors_log_full

(lower_ci_log_full <- exp(lower_log_ci_log_full))
(upper_ci_log_full <- exp(upper_log_ci_log_full))

# exp coeficients 
(exp_coef_full <- exp(estimates_log_full))




# Assumption on normality of random effects 


sim_res <- simulateResiduals(logistic_model_full)
plot(sim_res)

residual_bin <- residuals(logistic_model_full, type = "pearson")
fitted_bin <- fitted(logistic_model_full)
plot(residual_bin)
abline(h = 0, col = "red")

plot(fitted_bin, residual_bin)
abline(h = 0, col = "red")







# 2. Count model ---------------------------------------------
# Keeping only the observations for the active days 

active_data <- subset(data_new, active_day == 1)


sample_users <- active_data %>%
  distinct(user_id) %>%
  sample_n(20) %>% 
  pull(user_id)

df_sampled <- active_data %>% 
  filter(user_id %in% sample_users)

ggplot(df_sampled, aes(x=Day, y = percentage_change, color = as.factor(user_id))) +
  geom_line() + 
  geom_point() +
  theme_minimal()

# Gaussian family 
# Fitting a linear mixed effects model for modeling the percentage change in missing data on days 
# where user did make changes 

active_data <- subset(data_new, active_day == 1)

null_model_gaus <-  glmmTMB(percentage_change ~ 1, 
                            family = gaussian,
                            data = active_data)

random_intercept_gaus <-  glmmTMB(percentage_change ~ 1 +
                                    (1|user_id), 
                                  family = gaussian, 
                                  data = active_data)
summary(random_intercept_gaus)


# ICC
performance::icc(random_intercept_gaus)




anova(null_model_gaus, random_intercept_gaus)

random_slope_gaus <- glmmTMB(percentage_change ~ 1 +
                               (Day|user_id), 
                             family = gaussian,
                             data = active_data)
summary(random_slope_gaus)

anova(random_intercept_gaus, random_slope_gaus)





# 1. Model: Background characteristics 
gaus_model1 <- glmmTMB(percentage_change ~  age_grp + gender + income_class + car_license_combined +
                         migration_background + 
                         (Day|user_id), 
                       family = gaussian, 
                       data = active_data)

(gaus1_summary <- summary(gaus_model1))
(ci1 <- confint(gaus_model1))




# 2. Model: Technical profile 
gaus_model2 <- glmmTMB(percentage_change ~  brand + deviceold + standardized_battery_drainage +
                         (Day|user_id), 
                       family = gaussian,
                       data = active_data)

(gaus2_summary <- summary(gaus_model2))
(ci2 <- confint(gaus_model2))


# 3. Model: Motivation 
gaus_model3 <- glmmTMB(percentage_change ~  confirmed + dropout + total_duration_day1 +
                         (Day|user_id), 
                       family = gaussian,
                       data = active_data)

(gaus3_summary <- summary(gaus_model3))
(ci3 <- confint(gaus_model3))


# 4. Model: Ability 
gaus_model4 <- glmmTMB(percentage_change ~ average_labelling_time + time_quest + avg_time_confirmed + 
                         (Day|user_id), 
                       family = gaussian,
                       data = active_data)

(gaus4_summary <- summary(gaus_model4))
(ci4 <- confint(gaus_model4))


# Full model
gaus_model <- glmmTMB(percentage_change ~ age_grp + gender + income_class + car_license_combined +
                        migration_background + brand + deviceold + standardized_battery_drainage + confirmed + dropout+ 
                        total_duration_day1 +
                        average_labelling_time + time_quest + avg_time_confirmed + 
                        (Day|user_id), 
                      family = gaussian,
                      data = active_data)

(gaus_summary <- summary(gaus_model))
(ci <- confint(gaus_model))


# Assumption checking 

residual_gaus <- residuals(gaus_model, type = "pearson")
fitted_gaus <- fitted(gaus_model)
plot(residual_gaus)
abline(h = 0, col = "red")

plot(fitted_gaus, residual_gaus)
abline(h = 0, col = "red")


qqnorm(residual_gaus)
qqline(residual_gaus, col = "red")








