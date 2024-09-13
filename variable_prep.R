# This script is used for the feature extraction of the variables used to analyze
# user engagememnt within the smart travel survey 



# libraries ----------------------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(gridExtra)
library(haven)
library(stringr)




# Data importation ---------------------------------

combined_data <- fread("combined_improvement.csv")
overview_users <- readRDS("overview_users.RDS")
overview_users$user_id <- as.integer(overview_users$user_id)
outcome_wide <- fread("outcome_new.csv")
tracked_days <- fread("tracked_days.csv")
tracked_days[, day:=as.POSIXct(as.numeric(day)/1000, origin = "1970-01-01", tz = "UTC")]
classifiedperiods_raw <- fread("Dataset2022_unlabeled.csv")
classifiedperiods_edited <- fread("Dataset2022_undeleted.csv")
classifiedperiods_all <- fread("Dataset2022_all.csv")
minutes <- fread("minutes_copy.csv")
sessions <- fread("session_info2_copy.csv")
para <- sessions[,c(4,7)]
devices <- fread("devices.csv")

register <- fread("register_123.csv")

actions_edited <- classifiedperiods_edited

# change the time to smt readable
actions_edited[, start_date:=as.POSIXct(as.numeric(start_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions_edited[, end_date:=as.POSIXct(as.numeric(end_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions_edited[, created_on:=as.POSIXct(as.numeric(created_on)/1000, origin = "1970-01-01", tz = "UTC")] 
actions_edited[, deleted_on:=as.POSIXct(as.numeric(deleted_on)/1000, origin = "1970-01-01", tz = "UTC")] 


# these variables are not needed for now
actions_edited = subset(actions_edited, select = -c(created_on, deleted_on, start_date, end_date, vehicle_id, reason_id, user_edited_stops, labeled, sametime_start,sametime_end,timeedit,
                                                    timeedit2, unlabeledappedit, unlabeledtimeedit ))





# Outcome ----------------------------------------------------------------
# The outcome variable is calculated in the script "percentage_change.R"
# The dataset is in wide format. Here we derive the long format of the data 


# All users, all days, including the once with 0% missing 

outcome_wide <- outcome_wide %>%
  left_join(overview_users, by = "user_id") 

# I am considering only those users that are in phase 1 and 2, because the once in phase 3 were not able to make any modifications
outcome_wide <- outcome_wide %>%
  filter(fase == c(1,2))





outcome_long <- outcome_wide %>%
  pivot_longer(
    cols = matches("^[1-7]$"), 
    names_to = "Day", 
    values_to = "percentage_improvement",
    values_drop_na = TRUE
  ) %>%
  arrange(user_id)

outcome_long <- outcome_long %>%
  distinct()

ggplot(outcome_long, aes(x = percentage_improvement, fill = fase)) +
  geom_density(alpha = 0.5) + 
  ylim(0, 0.05) +
  labs(x = "Percentage Change")+
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

minutes_day <- minutes %>%
  mutate(day = as.Date(minute))

minutes_info <- minutes_day %>%
  arrange(user_id, day) %>%
  group_by(user_id) %>%
  mutate(day_diff = as.numeric(difftime(day, lag(day, default = first(day)), units = "days"))) %>%
  mutate(day_diff = ifelse(is.na(day_diff), 0, day_diff)) %>%
  mutate(cum_day = cumsum(day_diff)) %>%
  mutate(Day = cum_day - min(cum_day) + 1) %>%
  ungroup() %>%
  mutate(Day = paste0(Day))

minutes_unique <- minutes_info %>%
  group_by(user_id, Day) %>%
  slice(1) %>%
  ungroup()

minutes_selected <- dplyr::select(minutes_unique, user_id, day, Day)

outcome_long <- outcome_long %>%
  inner_join(minutes_selected, by = c("user_id", "Day"))






# PREDICTOR VARIABLES ---------------------------------------------------
# Here we start defining the predictor variables. The code is divided in blocks, each
# of refers to one of the 4 blocks of variables (Personal characteristics, device characteristics,
# motivation and ability) 



# 1. Motivation predictors 
# 1.1 Dropout 

check_dropout <- function(days) {
  if(is.na(days[length(days)])) {
    return(1)
  } else {
    return(0)
  }
}

outcome_wide <- outcome_wide %>%
  rowwise() %>%
  mutate(dropout = if_else(variant_days == 7, check_dropout(c_across(matches("^[1-7]$"))), if_else(variant_days == 1 & is.na("1"), 1, 0))) %>%
  ungroup()

dropout_info <- dplyr::select(outcome_wide,user_id, dropout)

outcome_long <- outcome_long %>%
  left_join(dropout_info, by = "user_id")



# 1.2 Day confirming 

tracked_days <- tracked_days[,1:12] 
tracked_days <-  dplyr::select(tracked_days,user_id, day, confirmed)


tracked_days_info <- tracked_days %>%
  arrange(user_id, day) %>%
  group_by(user_id) %>%
  mutate(day_diff = as.numeric(difftime(day, lag(day, default = first(day)), units = "days"))) %>%
  mutate(day_diff = ifelse(is.na(day_diff), 0, day_diff)) %>%
  mutate(cum_day = cumsum(day_diff)) %>%
  mutate(Day = cum_day - min(cum_day) + 1) %>%
  ungroup() %>%
  mutate(Day = paste0(Day))


tracked_days_info <- dplyr::select(tracked_days_info,user_id, Day, confirmed)


tracked_days_info <- tracked_days_info %>%
  group_by(user_id, Day) %>%
  summarise(confirmed = first(confirmed), .groups ='drop')


outcome_long <- outcome_long %>%
  left_join(tracked_days_info, by = c("user_id", "Day"))



# -------------------------------------------------------------
# some calculations that are needed for the variables defined below 

combined_filtered <- combined_data %>%
  filter(!(percentage_missing_raw == 0 & percentage_missing_edited ==0)) %>%
  rename(Day = day)

combined_filtered$Day <- as.character(combined_filtered$Day)

combined_filtered <- dplyr::select(combined_filtered, user_id, Day, percentage_missing_raw)

outcome_long_filtered <- outcome_long %>%
  left_join(combined_filtered, by = c("user_id", "Day"))



# Number of labelling made by the user 

actions_edited <- actions_edited %>% 
  mutate(day = as.Date(timestamp_createdon))

labelling_count <- actions_edited %>%
  group_by(user_id, day) %>%
  summarise(
    user_edit_count = sum(origin_cat == "user-edit", na.rm = TRUE),
    user_insert_count = sum(origin_cat == "user_insert", na.rm = TRUE),
    .groups = 'drop'
  )



labelling_count <- labelling_count %>%
  arrange(user_id, day) %>%
  group_by(user_id) %>%
  mutate(day_diff = as.numeric(difftime(day, lag(day, default = first(day)), units = "days"))) %>%
  mutate(day_diff = ifelse(is.na(day_diff), 0, day_diff)) %>%
  mutate(cum_day = cumsum(day_diff)) %>%
  mutate(Day = cum_day - min(cum_day) + 1) %>%
  ungroup() %>%
  mutate(Day = paste0(Day))

labelling_count <- dplyr::select(labelling_count, user_id, user_insert_count, user_edit_count, Day)

outcome_long_filtered <- outcome_long_filtered %>%
  left_join(labelling_count, by = c("user_id", "Day"))



# -------------------------------------------------------------------------


# 1.3 Time spend on the first day in the app derived from the paradata

paradata <- sessions %>%
  arrange(user_id, dag_melding) %>%
  group_by(user_id) %>%
  mutate(day_diff = as.numeric(difftime(dag_melding, lag(dag_melding, default = first(dag_melding)), units = "days"))) %>%
  mutate(day_diff = ifelse(is.na(day_diff), 0, day_diff)) %>%
  mutate(cum_day = cumsum(day_diff)) %>%
  mutate(Day = cum_day - min(cum_day) + 1) %>%
  ungroup() %>%
  mutate(Day = paste0(Day))


day1_sessions <- paradata %>%
  filter(Day == 1) %>%
  distinct(session_id, .keep_all = TRUE)


unique_durations <- day1_sessions %>%
  distinct(user_id, session_id, duration, .keep_all = TRUE)

first_day_durations <- unique_durations %>%
  group_by(user_id) %>%
  summarise(total_duration_day1 = sum(duration, na.rm = TRUE), .groups = 'drop')

outcome_long_filtered <- outcome_long_filtered %>%
  left_join(first_day_durations, by = "user_id")  # the duration is saved as seconds 


outcome_long_filtered <- outcome_long_filtered %>%
  mutate(
    user_insert_count = ifelse(is.na(user_insert_count), 0, user_insert_count),
    user_edit_count = ifelse(is.na(user_edit_count), 0, user_edit_count)
  )

outcome_long_filtered$confirmed <- factor(outcome_long_filtered$confirmed, levels = c("f", "t"),  labels = c(0,1))






# 2. Register variables 
# 2.1 Age

register$age <- as.numeric(gsub("jaar", "", register$age))

# creating age groups
bins <- c(15,30,50,70,96)
labels <- c("[15,30]", "(30,50]", "(50,70]", "(70,96]")

data$age_grp <- cut(data$age, breaks = bins, labels = labels, right = TRUE)

# 2.2 Gender 

register$gender <- recode(register$sex, "Mannen" = "Male", "Vrouwen" = "Female", "Onbekend" = "Unknown")
register <- register %>%
  mutate(
    gender = recode(
      gender, 
      "Male" = 0,
      "Female" = 1,
      "Unknown" = NA_real_
    )
  )



# 2.3 Income

register$inc_num <- as.numeric(str_extract(string = register$inc, pattern = "[0-9]+")) 
register$income <- register$inc_num


InkKlas4 <- function(Inkomen){
  case_when(
    is.na(Inkomen) ~ "1. 0-25% perc", #"0. Missing",
    Inkomen <= 25 ~ "1. 0-25% perc",
    Inkomen > 25 & Inkomen <= 50 ~ "2. 25-50% perc",
    Inkomen > 50 & Inkomen <= 75 ~ "3. 50-75% perc",
    Inkomen > 75 ~ "4. 75-100% perc"
  )
  
}

register <- register %>%
  mutate(income_class = InkKlas4(income))


# 2.4 Car and License owners
register$car <- register$auto
register$car_license <- register$autolic
register <- register %>%
  mutate(
    car_license = recode(
      car_license, 
      "Nee" = 0,
      "Ja"  = 1,
      "NA" = NA_real_
    )
  )



# 2.5 Migration Background
# this variable was changed in a way that we only distinguish between population that is from the Netherlands and population that is from outside the Netherlands 
register$migration_background <- register$migration
register$migration_background <- ifelse(register$migration_background == "Nederlandse achtergrond", 0, 1)




useful_register <- dplyr::select(register,user_id, age, age_grp, gender, income, income_class, car, migration_background, car_license)

outcome_long_filtered <- outcome_long_filtered %>%
  left_join(useful_register, by = "user_id")



outcome_long_filtered$car_license_combined <- with(outcome_long_filtered, ifelse(car == 1 & car_license == 1, "Car and License", "No car or No Licnese"))
outcome_long_filtered$car_license_combined <- as.factor(outcome_long_filtered$car_license_combined)



# 3. Technical Profile 

# 3.1 Phone brand

outcome_long_filtered <- outcome_long_filtered %>%
  mutate(brand = case_when(
    brand %in% c("iPhone", "samsung") ~ brand,
    TRUE ~"Others"
  ))

# 3.2 Battery drainage 

names(batterydf)[names(batterydf) == "device_id"] <- "user_id"

batterydf <- batterydf %>%
  arrange(user_id, day, ts2)%>%
  filter(hour(ts2) >= 5 & hour(ts2) < 22)

#only between 5am and 10pm because thats the timeframe that I am considering 

battery_df <- batterydf %>%
  group_by(user_id, day) %>%
  mutate(battery_change = battery_level - lag(battery_level)) %>%
  ungroup()

negative_changes <- battery_df %>%
  filter(battery_change < 0) 


battery_drainage <- negative_changes %>%
  group_by(user_id, day) %>%
  summarise(
    battery_drainage = sum(battery_change, na.rm = TRUE)
  )%>%
  ungroup()




battery_drainage <- battery_drainage %>%
  arrange(user_id, day) %>%
  group_by(user_id) %>%
  mutate(day_diff = as.numeric(difftime(day, lag(day, default = first(day)), units = "days"))) %>%
  mutate(day_diff = ifelse(is.na(day_diff), 0, day_diff)) %>%
  mutate(cum_day = cumsum(day_diff)) %>%
  mutate(Day = cum_day - min(cum_day) + 1) %>%
  ungroup() %>%
  mutate(Day = paste0(Day))



active_time_per_day <- batterydf %>%
  group_by(user_id, day) %>%
  summarise(active_seconds = n(),
            active_hours = round(active_seconds / 3600,0))# %>%


average_time_per_day <- dplyr::select(active_time_per_day, user_id, day, active_hours)

merged_battery <- merge(battery_drainage, active_time_per_day, by = c("user_id", "day"))
merged_battery <- merged_battery %>%
  mutate(standardized_battery_drainage = ifelse(active_hours == 0, 0, round(battery_drainage / active_hours,2)))

useful_battery <- dplyr::select(merged_battery, user_id, Day,  
                                standardized_battery_drainage )

outcome_long_filtered <- outcome_long_filtered %>%
  left_join(useful_battery, by = c("user_id", "Day" ))


# 3.3 Operating Systems
# classification of phones into low, medium and high

devices <- devices[,-c(18,19,20,21,22,23,24,25,26)]

brand <- devices$brand

devices$brand_grouped = case_when(brand=="iPhone"~"iPhone" , brand == "samsung" ~"samsung",    brand == "OnePlus"|
                                    brand=='OPPO' |brand=='motorola' | brand=='Sony' | brand == "google"| brand=='HUAWEI'| brand == 'iPad'|
                                    brand=='Redmi'| brand=='realme'| brand == 'Nokia' | brand == 'Fairphone' | brand == 'lge' | 
                                    brand == 'xiaomi' | brand =='POCO'| brand =='Nothing' | brand =="Xiaomi" ~'other', 
                                  TRUE~"no")



devices_grouped <- devices %>%  mutate(
  ios1 = ifelse(brand_grouped=='iPhone', version, "android"),
  ios =    gsub("\\..*", "", ios1),
  deviceold = case_when(
    (as.numeric(ios) < 15) | as.numeric(sdk)<30 ~'old',
    (as.numeric(ios)==15 | version == '15.6') | (as.numeric(sdk) > 29 & as.numeric(sdk)<33) ~'medium', #ios 15 is not supported anymore
    (as.numeric(ios)==16 | version == '16.1.1' |version == '16.3') | as.numeric(sdk) ==33 ~'new',
    TRUE ~ 'unknown'
  ) )

useful_devices <- dplyr::select(devices_grouped, user_id, deviceold)

outcome_long_filtered <- outcome_long_filtered %>%
  left_join(useful_devices, by = c("user_id"))



# 4. Ability 

names(paradata)[names(paradata) == "dag_melding"] <- "day"

paradata <- paradata %>%
  mutate(day = as.Date(day))


#  4.1 Average labelling time 
# log messages: Routes::movementDetailsPage, Routes::select_stop_reason, Routes::select_vehicle, SelectReasonPage::build, SelectVehiclePage::build, StopDetailsPage::build

labelling_messages <- c("Routes::movementDetailsPage", "Routes::select_stop_reason", 
                        "Routes::select_vehicle", "SelectReasonPage::build", 
                        "SelectVehiclePage::build", "StopDetailsPage::build")

labelling_data <- paradata %>%
  filter(message %in% labelling_messages)


unique_durations <- labelling_data %>%
  distinct(user_id, day, session_id, .keep_all = TRUE)

daily_labelling_duration <- unique_durations %>%
  group_by(user_id, day) %>%
  summarise(daily_labelling_duration = sum(duration))


total_labelling_duration <- daily_labelling_duration %>%
  group_by(user_id) %>%
  summarise(total_labelling_duration = sum(daily_labelling_duration))

total_label_count <- labelling_count %>%
  group_by(user_id) %>%
  summarise(tot_label_count = sum(user_edit_count + user_insert_count))


merged_label <- merge(total_labelling_duration, total_label_count, by = "user_id")

merged_label$total_labelling_duration <- as.numeric(merged_label$total_labelling_duration)
merged_label$tot_label_count <- as.numeric(merged_label$tot_label_count)

merged_label <- merged_label %>%
  mutate(average_labelling_time = total_labelling_duration/tot_label_count, 
         average_labelling_time = round(ifelse(is.infinite(average_labelling_time) | is.nan(average_labelling_time), 0, average_labelling_time),2))

outcome_long_filtered <- outcome_long_filtered %>%
  left_join(merged_label, by = "user_id")



# 4.2 Questionnaire Tasks:
# log messages: Questionnaire::build, Questionnaire::onQuestionComplete, Questionnaire::onQuestionnaireComplete

questionnaire_messages <- c("QuestionINtroductionPage::build", "Questionnaire::build", "Questionnaire::onQuestionnaireComplete")


questionnaire_data <- paradata %>%
  filter(message %in% questionnaire_messages)

user_id_counts <- questionnaire_data %>%
  group_by(user_id) %>%
  summarise(row_count = n()) %>%
  ungroup

multiple_rows_user_ids <- user_id_counts %>%
  filter(row_count > 2)


questionnaire_data <- questionnaire_data %>%
  group_by(user_id) %>%
  filter(day == min(day)) %>%
  ungroup

# time needed to finish the questionnaire in seconds
# calculated by taking the difference of questionnaire start message and end message 

duration_questionnaire_task <- questionnaire_data %>%
  group_by(user_id) %>%
  summarise(
    start_time = if(any(message == "Questionnaire::build"))  min(date_time[message == "Questionnaire::build"]) else NA,
    end_time = if(any(message == "Questionnaire::onQuestionnaireComplete")) max(date_time[message == "Questionnaire::onQuestionnaireComplete"]) else NA,
    time_quest = if(!is.na(start_time) & !is.na(end_time)) as.numeric(difftime(end_time, start_time, units = "secs")) else NA
  )

# one user had a very long time for completing the questionnaire probably because he opened it and then didn't finish it immediately 
start_time <- as.POSIXct("", format = "%H:%M:%S")
end_time <- as.POSIXct("", format = "%H:%M:%S")

time_difference <- as.numeric(difftime(end_time, start_time, units = "secs"))
duration_questionnaire_task$time_quest[duration_questionnaire_task$time_quest == ] <- time_difference

person_level_quest_time <- dplyr::select(duration_questionnaire_task, user_id, time_quest)

outcome_long_filtered <- outcome_long_filtered %>%
  left_join(person_level_quest_time, by = "user_id")



# 4.3 Day Overview and Confirmation 
# log messages: DayOverviewPage::build, MovementPageNotifier::confirmDayAsync


day_conf_messages <- c("DayOverviewPage::build", "MovementPageNotifier::confirmDayAsync")


day_conf_data <- paradata %>%
  filter(message %in% day_conf_messages)



time_conf_day <- day_conf_data %>%
  group_by(user_id, day) %>%
  mutate(
    prev_message = lag(message), 
    prev_date_time = lag(date_time),
    time_conf = ifelse(
      message == "MovementPageNotifier::confirmDayAsync" & prev_message == "DayOverviewPage::build", 
      as.numeric(difftime(date_time, prev_date_time, units = "secs")),
      NA
    )
  ) %>%
  
  filter(message == "MovementPageNotifier::confirmDayAsync") %>%
  dplyr::select(user_id, date_time, time_conf)



time_conf_day <- time_conf_day %>%
  mutate(date_time = as.POSIXct(date_time),
         day = as.Date(date_time))

time_conf_day <- time_conf_day %>%
  group_by(user_id) %>%
  summarise(total_conf_time = sum(time_conf)) %>%
  mutate(total_conf_time = ifelse(is.na(total_conf_time), 0, total_conf_time))




outcome_long_filtered <- outcome_long_filtered %>%
  left_join(time_conf_day, by = "user_id")


outcome_long_filtered$confirmed <- as.numeric(as.character(outcome_long_filtered$confirmed))

result <- outcome_long_filtered %>%
  group_by(user_id) %>%
  summarize(
    total_conf_time = first(total_conf_time),
    num_confirmed_days = sum(confirmed),
    avg_time_confirmed = round(total_conf_time / num_confirmed_days,2)
  ) %>%
  mutate(
    avg_time_confirmed = ifelse(num_confirmed_days == 0, 0, avg_time_confirmed)
  ) %>%
  dplyr::select(user_id, avg_time_confirmed)


outcome_long_filtered <- outcome_long_filtered %>%
  left_join(result, by = "user_id") 




















