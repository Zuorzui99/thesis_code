# This script includes the code to derive the percentage change in missing data
# between the raw and the edited version of the stop/track predictions 




# libraries ----------------------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(gridExtra)


# Data --------------------------------------------------------------------


classifiedperiods_raw <- fread("Dataset2022_unlabeled.csv")
classifiedperiods_edited <- fread("Dataset2022_undeleted.csv")
classifiedperiods_all <- fread("Dataset2022_all.csv")
sessions <- fread("session_info2_copy.csv")
minutes <- fread("minutes_copy.csv")
devices <- fread("devices.csv")
last_info <- fread("last_info_user.csv")
start_times <- fread("start_times.csv")









# ACTIONS ---------------------------------------------------------------

actions <- classifiedperiods_all


# change the time to smt readable
actions[, start_date:=as.POSIXct(as.numeric(start_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions[, end_date:=as.POSIXct(as.numeric(end_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions[, created_on:=as.POSIXct(as.numeric(created_on)/1000, origin = "1970-01-01", tz = "UTC")] 
actions[, deleted_on:=as.POSIXct(as.numeric(deleted_on)/1000, origin = "1970-01-01", tz = "UTC")] 



# Filtering out the columns that are not needed 
actions = subset(actions, select = -c( edited, secondedit, user_edited_stops, labeled, sametime_start, sametime_end,
                                       createdon_beforestartofperiod, createdon_beforeendofperiod,  
                                       timediff_createdstart, timediff_createdend, deleted, timeedit2, unlabeledappedit, unlabeledmissing,
                                       durationperiod_min,timeedit, unlabeledtimeedit, vehicle_id, reason_id))




# If something hasn't been deleted, put the time on missing
actions[deleted_on == as.POSIXct(0, origin = "1970-01-01", tz= "UTC"), deleted_on:=NA]





# UNEDITED ACTIONS ----------------------------------------------------------
actions_raw <- classifiedperiods_raw


# change the time to smt readable
actions_raw[, start_date:=as.POSIXct(as.numeric(start_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions_raw[, end_date:=as.POSIXct(as.numeric(end_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions_raw[, created_on:=as.POSIXct(as.numeric(created_on)/1000, origin = "1970-01-01", tz = "UTC")] 
actions_raw[, deleted_on:=as.POSIXct(as.numeric(deleted_on)/1000, origin = "1970-01-01", tz = "UTC")] 


actions_raw = subset(actions_raw, select = -c(start_date, end_date, created_on,edited, secondedit, user_edited_stops, labeled, sametime_start, sametime_end,
                                              createdon_beforestartofperiod, createdon_beforeendofperiod, createdon_equalstartofperiod, createdon_equalendofperiod,
                                              timediff_createdstart, timediff_createdend, deleted, timeeditOLD, timeedit2, unlabeledappedit, unlabeledmissing,
                                              durationperiod_min,durationorigin_min, edit_sameduration,timeedit, unlabeledtimeedit, vehicle_id, reason_id))




# If something hasn't been deleted, put the time on missing
actions_raw[deleted_on == as.POSIXct(0, origin = "1970-01-01", tz= "UTC"), deleted_on:=NA]


# to make it compatible with minutes dataset, hour needs to be transformed 
actions_raw <- actions_raw %>%
  mutate(
    timestamp_createdon = timestamp_createdon + hours(1),
    timestamp_endofperiod = timestamp_endofperiod + hours(1),
    timestamp_startofperiod = timestamp_startofperiod + hours(1)
  )



# EDITED ACTIONS --------------------------------------------------------------------

actions_edited <- classifiedperiods_edited

# change the time to smt readable
actions_edited[, start_date:=as.POSIXct(as.numeric(start_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions_edited[, end_date:=as.POSIXct(as.numeric(end_date)/1000, origin = "1970-01-01", tz = "UTC")]
actions_edited[, created_on:=as.POSIXct(as.numeric(created_on)/1000, origin = "1970-01-01", tz = "UTC")] 
actions_edited[, deleted_on:=as.POSIXct(as.numeric(deleted_on)/1000, origin = "1970-01-01", tz = "UTC")] 


# these variables are not needed for now
actions_edited = subset(actions_edited, select = -c(created_on, deleted_on, start_date, end_date, vehicle_id, reason_id, user_edited_stops, labeled, sametime_start,sametime_end,timeedit,
                                                    timeedit2, unlabeledappedit, unlabeledtimeedit ))









# ----------------------------------------------------------------------------

# Function to filter the days for the actions datasets and the minutes
filter_user_actions <- function(dataset, day_number) {
  dataset %>%
    mutate(date = as.Date(timestamp_createdon)) %>%
    arrange(user_id, date) %>%
    group_by(user_id) %>%
    mutate(first_timestamp = min(date)) %>%
    ungroup() %>%
    mutate(days_since_start = as.numeric(difftime(date, first_timestamp, units = "days")) + 1) %>%
    filter(days_since_start == day_number) %>%
    select(-first_timestamp, -days_since_start)%>%
    arrange(user_id, timestamp_createdon)
  
}

#function that filters out the night time hours from the minutes dataset 
minutes_day <- function(dataset, day_number) {
  dataset %>%
    mutate(hour = hour(minute),
           date = as.Date(minute)) %>%
    group_by(user_id) %>%
    mutate(first_minute = min(date)) %>%
    ungroup() %>%
    mutate(days_since_start = as.numeric(difftime(date, first_minute, units = "days")) + 1) %>%
    filter(days_since_start == day_number) %>%
    filter(!(hour >= 22 | hour < 5)) %>%
    select(-first_minute, -days_since_start, -hour)%>%
    arrange(user_id, minute)
  
}



# function that filters the raw minutes data, and calculates the percentage missing
# in the data
raw_user_minute <- function(dataset) {
  dataset %>%
    group_by(user_id) %>%
    mutate(
      minutes_left_inday = 1020, # we are considering only the hours between 5am and 10pm
      observed = label_alg22_raw %in% c("stop", "track")) %>%
    summarise(
      observed_minutes = sum(observed, na.rm = TRUE),
      minutes_left_inday = first(minutes_left_inday),
      missing_observations = minutes_left_inday - observed_minutes,
      percentage_missing = round((missing_observations / minutes_left_inday) * 100, 2)
    )  
  
}


# function that filters the edited minuted data, and calculates the percentage missing
# in the data

edited_user_minute <- function(dataset) {
  dataset %>%
    group_by(user_id) %>%
    mutate(
      minutes_left_inday = 1020, 
      observed = label_alg22_user %in% c("stop", "track")) %>%
    summarise(
      observed_minutes = sum(observed, na.rm = TRUE),
      minutes_left_inday = first(minutes_left_inday),
      missing_observations = minutes_left_inday - observed_minutes,
      percentage_missing = round((missing_observations / minutes_left_inday) * 100, 2)
    )  
  
}





# Day 1 ----------------------------------------------------------
# for day 1 I cannot use the functions above, since every user started at different times of the day
# therefore, for day 1 another function was used 
# data for the first day 

minutes_day1 <- minutes_day(minutes, 1)
minutes_day1 <- merge(minutes_day1, start_times, by = "user_id")


raw_user_minute_day1 <- minutes_day1 %>%
  group_by(user_id) %>%
  mutate(
    start_time_minutes = hour(starttime) * 60 + minute(starttime),
    minutes_left_inday = 1440 - start_time_minutes,
    observed = label_alg22_raw %in% c("stop", "track")) %>%
  summarise(
    observed_minutes = sum(observed, na.rm = TRUE),
    minutes_left_inday = first(minutes_left_inday),
    missing_observations = minutes_left_inday - observed_minutes,
    percentage_missing = round(((missing_observations - 60) / (minutes_left_inday - 60)) * 100, 2)
    
  ) 


p1 <- ggplot(raw_user_minute_day1, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 1 - Raw",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 



edited_user_minute_day1 <- minutes_day1 %>%
  group_by(user_id) %>%
  mutate(
    start_time_minutes = hour(starttime) * 60 + minute(starttime),
    minutes_left_inday = 1440 - start_time_minutes,
    observed = label_alg22_user %in% c("stop", "track")) %>%
  summarise(
    observed_minutes = sum(observed, na.rm = TRUE),
    minutes_left_inday = first(minutes_left_inday),
    missing_observations = minutes_left_inday - observed_minutes,
    percentage_missing = round((missing_observations / minutes_left_inday) * 100, 2)
    
  ) 


p2 <- ggplot(edited_user_minute_day1, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 1 - Edited",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 


# Change in missing data for day 1 
change_day1 <- merge(raw_user_minute_day1, edited_user_minute_day1, by = "user_id", suffixes = c("_raw", "_edited"))
change_day1$percentage_change <- change_day1$percentage_missing_raw - change_day1$percentage_missing_edited


p3 <- ggplot(change_day1, aes(x = percentage_change)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 1 - Difference",
    x = "Percentage Change",
    y = "User Count") +
  theme_minimal() 




grid.arrange(p1, p2, p3, ncol = 3)




# Day2 --------------------------------------------------------------
minutes_day2 <- minutes_day(minutes, 2)

raw_user_minute_day2 <- raw_user_minute(minutes_day2)

p1 <- ggplot(raw_user_minute_day2, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 2 - Raw",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 


edited_user_minute_day2 <- edited_user_minute(minutes_day2)

p2 <- ggplot(edited_user_minute_day2, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 2 - Edited",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal()



# change in missing data for day 2 

change_day2 <- merge(raw_user_minute_day2, edited_user_minute_day2, by = "user_id", suffixes = c("_raw", "_edited"))
change_day2$percentage_change <- change_day2$percentage_missing_raw - change_day2$percentage_missing_edited

p3 <- ggplot(change_day2, aes(x = percentage_change)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 2 - Difference",
    x = "Percentage Change",
    y = "User Count") +
  theme_minimal() 


grid.arrange(p1, p2, p3, ncol = 3)



# Day 3 -------------------------------------------------------------------------

minutes_day3 <- minutes_day(minutes, 3)

raw_user_minute_day3 <- raw_user_minute(minutes_day3)

p1 <- ggplot(raw_user_minute_day3, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 3 - Raw",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 


edited_user_minute_day3 <- edited_user_minute(minutes_day3)

p2 <- ggplot(edited_user_minute_day3, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 3 - Edited",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal()


#Change in missing data on day 3  

change_day3 <- merge(raw_user_minute_day3, edited_user_minute_day3, by = "user_id", suffixes = c("_raw", "_edited"))
chnage_day3$percentage_change <- change_day3$percentage_missing_raw - change_day3$percentage_missing_edited

p3 <- ggplot(change_day3, aes(x = percentage_change)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 3 - Difference",
    x = "Percentage Change",
    y = "User Count") +
  theme_minimal() 

grid.arrange(p1, p2, p3, ncol = 3)



# Day 4 -----------------------------------------------------------------------
minutes_day4 <- minutes_day(minutes, 4)

raw_user_minute_day4 <- raw_user_minute(minutes_day4)

p1 <- ggplot(raw_user_minute_day4, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 4 - Raw",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 


edited_user_minute_day4 <- edited_user_minute(minutes_day4)

p2 <- ggplot(edited_user_minute_day4, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 4 - Edited",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal()


# change in missing data on day 4 

change_day4 <- merge(raw_user_minute_day4, edited_user_minute_day4, by = "user_id", suffixes = c("_raw", "_edited"))
change_day4$percentage_change <- change_day4$percentage_missing_raw - change_day4$percentage_missing_edited

p3 <- ggplot(change_day4, aes(x = percentage_change)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 4 - Difference",
    x = "Percentage Change",
    y = "User Count") +
  theme_minimal() 

grid.arrange(p1, p2, p3, ncol = 3)




# Day 5 ------------------------------------------------------

minutes_day5 <- minutes_day(minutes, 5)

raw_user_minute_day5 <- raw_user_minute(minutes_day5)

p1 <- ggplot(raw_user_minute_day5, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 5 - Raw",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 


edited_user_minute_day5 <- edited_user_minute(minutes_day5)

p2 <- ggplot(edited_user_minute_day5, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 5 - Edited",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal()


# Change in missing data on day 5

change_day5 <- merge(raw_user_minute_day5, edited_user_minute_day5, by = "user_id", suffixes = c("_raw", "_edited"))
change_day5$percentage_change <- change_day5$percentage_missing_raw - change_day5$percentage_missing_edited

p3 <- ggplot(change_day5, aes(x = percentage_change)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 4 - Difference",
    x = "Percentage Change",
    y = "User Count") +
  theme_minimal() 

grid.arrange(p1, p2, p3, ncol = 3)




# Day 6 --------------------------------------------------------------
minutes_day6 <- minutes_day(minutes, 6)

raw_user_minute_day6 <- raw_user_minute(minutes_day6)

p1 <- ggplot(raw_user_minute_day6, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(
    subtitle = "Day 6 - Raw",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 


edited_user_minute_day6 <- edited_user_minute(minutes_day6)

p2 <- ggplot(edited_user_minute_day6, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 6 - Edited",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal()

# Change in missing data on day 6 

change_day6 <- merge(raw_user_minute_day6, edited_user_minute_day6, by = "user_id", suffixes = c("_raw", "_edited"))
change_day6$percentage_change <- change_day6$percentage_missing_raw - change_day6$percentage_missing_edited

p3 <- ggplot(change_day6, aes(x = percentage_change)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 6 - Difference",
    x = "Percentage Change",
    y = "User Count") +
  theme_minimal() 

grid.arrange(p1, p2, p3, ncol = 3)


# Day 7 ---------------------------------------------------------------

minutes_day7 <- minutes_day(minutes, 7)

raw_user_minute_day7 <- raw_user_minute(minutes_day7)

p1 <- ggplot(raw_user_minute_day7, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 7 - Raw",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal() 


edited_user_minute_day7 <- edited_user_minute(minutes_day7)

p2 <- ggplot(edited_user_minute_day7, aes(x = percentage_missing)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 7 - Edited",
    x = "Percentage Missing",
    y = "User Count") +
  theme_minimal()


# Change in missing data on day 7 

change_day7 <- merge(raw_user_minute_day7, edited_user_minute_day7, by = "user_id", suffixes = c("_raw", "_edited"))
change_day7$percentage_change <- change_day7$percentage_missing_raw - change_day7$percentage_missing_edited

p3 <- ggplot(change_day7, aes(x = percentage_change)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    subtitle = "Day 7 - Difference",
    x = "Percentage Change",
    y = "User Count") +
  theme_minimal() 

grid.arrange(p1,p2,p3, ncol = 3)


#----------------------------------------------------


# Putting all days together in one dataset:

change_data1 <- change_day1 %>% mutate(day = 1)
change_data2 <- change_day2 %>% mutate(day = 2)
change_data3 <- change_day3 %>% mutate(day = 3)
change_data4 <- change_day4 %>% mutate(day = 4)
change_data5 <- change_day5 %>% mutate(day = 5)
change_data6 <- change_day6 %>% mutate(day = 6)
change_data7 <- change_day7 %>% mutate(day = 7)


combined_data <- bind_rows(change_data1, change_data2, change_data3,
                           change_data4, change_data5, change_data6, change_data7)



final_dataset_new <- combined_data %>%
  select(user_id, day, percentage_change) %>%
  spread(key = day, value = percentage_change)






