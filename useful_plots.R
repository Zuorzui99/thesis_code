# some useful plots 


# libraries -----------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)

# Data ----------------------------------------------------------
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

# Plots -------------------------------------------------

# Distribution of percentage change in missing data on all 7 days of the study 

ggplot(data, aes(x = percentage_change)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data", 
       y = "Density") +
  theme_minimal(base_size = 14)



ggplot(active_data, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data", 
       y = "Count") +
  theme_minimal(base_size = 14)


day1 <- active_data %>%
  filter(Day == 1)

ggplot(day1, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data Day 1", 
       y = "Density") +
  theme_minimal(base_size = 14)


day2 <- active_data %>%
  filter(Day == 2)

ggplot(day2, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data Day 2",
       y = "Count") +
  theme_minimal(base_size = 14)

day3 <- active_data %>%
  filter(Day == 3)

ggplot(day3, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data Day 3", 
       y = "Density") +
  theme_minimal(base_size = 14)

day4 <- active_data %>%
  filter(Day == 4)

ggplot(day4, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data Day 4", 
       y = "Density") +
  theme_minimal(base_size = 14)


day5 <- active_data %>%
  filter(Day == 5)

ggplot(day5, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data Day 5", 
       y = "Count") +
  theme_minimal(base_size = 14)

day6 <- active_data %>%
  filter(Day == 6)

ggplot(day6, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data Day 6", 
       y = "Density") +
  theme_minimal(base_size = 14)

day7 <- active_data %>%
  filter(Day == 7)

ggplot(day7, aes(x = percentage_change)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(x = "Percentage Change in Missing Data Day 7", 
       y = "Count") +
  theme_minimal(base_size = 14)



# ----------------------------------------------------------------------
# percentage change in missing data over 7 days of the study, for every user, with smoothing line 

p <- ggplot(data, aes(x= Day, y = percentage_change, group = user_id, color = as.factor(user_id))) +
  geom_line(alpha = 0.5) +
  labs(x = "Day of Study", y = "Percentage Change in Missing Data")+
  theme_minimal() +
  theme(legend.position = "none")

p + geom_smooth(aes(group =1), method = "loess", se = FALSE, color = "red", size = 1.2) 


ggplot(active_data, aes(x = percentage_change)) +
  geom_density(alpha = 0.3, fill = "red") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.01)) +
  labs(x = "Percentage Change in Missing Data", y = "Denisty")+
  theme_minimal()



# Boxplots of daily percentage change in missing data 

ggplot(data, aes(x = as.factor(Day), y = percentage_change)) +
  geom_boxplot(fill = "lightgreen")+
  labs(x = "Day", y = "Percentage Change") +
  theme_minimal()

