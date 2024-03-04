# script for data_wrangling

# data for steps, activities, etc

activities_brute <- read_csv("dados/s_health06_10/samsunghealth_gustavo.utpott_20231006175776/com.samsung.shealth.activity.day_summary.20231006175776.csv", skip = 1) %>%
  mutate(update_time = update_time - lubridate::hours(3)) %>%
  mutate(create_time = create_time - lubridate::hours(3)) %>%
  mutate(
    active_time = active_time/60000, 
    others_time = others_time/60000,
    longest_active_time = longest_active_time/60000
  ) %>%
  mutate(date = as_date(create_time)) %>%
  arrange(date) %>%
  mutate(
    week = week(date),
    month = month(date),
    year = year(date),
    week_year = str_c(week, "-", year),
    month_year = str_c(month, "-", year)
  ) %>%
  mutate(
    ma_3_steps = slider::slide_dbl(step_count, mean, .before = 1, .after = 1),
    ma_7_steps = slider::slide_dbl(step_count, mean, .before = 3, .after = 3),
    ma_31_steps = slider::slide_dbl(step_count, mean, .before = 15, .after = 15)
  )


activities_brute_weekly <- activities_brute %>%
  group_by(week, year, week_year) %>%
  summarise(
    mean_week_steps = mean(step_count, na.rm = T),
    mean_week_act_time = mean(active_time, na.rm = T),
    mean_week_calorie_spent = mean(calorie, na.rm = T)
  ) %>%
  arrange(year, week) %>%
  mutate(date_ref = as.Date(paste0(week_year,"-1"), "%U-%Y-%u"))


activities_brute_monthly <- activities_brute %>%
  group_by(month, year, month_year) %>%
  summarise(
    mean_month_steps = mean(step_count, na.rm = T),
    mean_month_act_time = mean(active_time, na.rm = T),
    mean_month_calorie_spent = mean(calorie, na.rm = T)
  ) %>%
  arrange(year, month) %>%
  mutate(date_ref = as.Date(paste0(month_year,"-1"), "%m-%Y-%d"))
  

floors_climbed <- read_csv("dados/s_health06_10/samsunghealth_gustavo.utpott_20231006175776/com.samsung.health.floors_climbed.20231006175776.csv", skip = 1)

exercises <- read_csv("dados/s_health06_10/samsunghealth_gustavo.utpott_20231006175776/com.samsung.shealth.exercise.20231006175776.csv", skip = 1)

cardiac_zone <- tibble(
  type = c("Low Intensity", "Weight control", "Aerobic", "Anaerobic", "Maximum"),
  lower_bound = c(98,119,138,158,178),
  upper_bound = c(118,137,157,177,200),
  lower_percentage = c(50,60,70,80,90),
  upper_percentage = c(60,70,80,90,100)
)

# wearables

wearables <- tibble(
  wearable = c("galaxy fit 2", "galaxy watch 4"),
  begin_date = c("2020-11-30", "2022-12-08"),
  end_date = c("2022-12-07", as.character(Sys.Date()))
)


# sleep data

sleep_brute <- read_csv("dados/s_health06_10/samsunghealth_gustavo.utpott_20231006175776/com.samsung.health.sleep_stage.20231006175776.csv", skip = 1) %>%
  mutate(start_time = start_time + lubridate::hours( as.numeric(str_remove(str_extract(time_offset, "^......"),"^...")))) %>%
  mutate(end_time = end_time + lubridate::hours( as.numeric(str_remove(str_extract(time_offset, "^......"),"^...")))) %>%
  group_by(sleep_id) %>%
  mutate(start_sleep_time = min(start_time)) %>%
  mutate(end_sleep_time = max(end_time)) %>%
  ungroup() %>%
  mutate(day_of_sleep = ifelse(day(start_sleep_time) != day(end_sleep_time), as_date(start_sleep_time), ifelse(hour(start_sleep_time)>12, as_date(start_sleep_time), as_date(start_sleep_time)-days(1)))) %>%
  mutate(day_of_sleep = as_date(day_of_sleep))



sleep_brute_phases <- sleep_brute %>%
  group_by(sleep_id, stage, day_of_sleep) %>%
  summarise(tempo_na_fase = sum(end_time-start_time))

sleep_brute_time_daily <- sleep_brute %>%
  select(day_of_sleep, start_sleep_time, end_sleep_time) %>%
  distinct() %>%
  group_by(day_of_sleep) %>%
  mutate(number_of_sleeps = n(),
         order_of_sleep = ifelse(start_sleep_time == min(start_sleep_time), "first", 
                                 ifelse((n() == 3 & start_sleep_time == max(start_sleep_time)), "third", "second"))) %>%
  pivot_wider(names_from = order_of_sleep, values_from = c(start_sleep_time, end_sleep_time)) %>%
  mutate(
    total_amount_sleep = case_when(
      number_of_sleeps == 1 ~ as.numeric(difftime(end_sleep_time_first, start_sleep_time_first, units = "hours")),
      number_of_sleeps == 2 ~ as.numeric(difftime(end_sleep_time_first, start_sleep_time_first, units = "hours")+difftime(end_sleep_time_second, start_sleep_time_second, units = "hours")),
      number_of_sleeps == 3 ~ as.numeric(difftime(end_sleep_time_first, start_sleep_time_first, units = "hours")+difftime(end_sleep_time_second, start_sleep_time_second, units = "hours")+difftime(end_sleep_time_third, start_sleep_time_third, units = "hours"))
    )
  ) %>%
  mutate(
    bedtime = ifelse(day(start_sleep_time_first) != day(day_of_sleep), 
                     period_to_seconds(hms(strftime(start_sleep_time_first, format="%H:%M:%S", tz = "GMT")))+86400,
                     period_to_seconds(hms(strftime(start_sleep_time_first, format="%H:%M:%S", tz = "GMT"))))
  ) %>%
  mutate(wake_up_max = max(end_sleep_time_first,end_sleep_time_second,end_sleep_time_third, na.rm = T)) %>%
  mutate(
    wake_up_time = period_to_seconds(hms(strftime(wake_up_max, format="%H:%M:%S", tz = "GMT")))
  ) %>%
  arrange(day_of_sleep) %>%
  mutate(
    week = week(day_of_sleep),
    month = month(day_of_sleep),
    year = year(day_of_sleep),
    week_year = str_c(week, "-", year),
    month_year = str_c(month, "-", year)
  )
  
# apagando outliers, que conferi e são realmente dados errados

# 2022-02-18, 2022-02-19, 2022-02-20, 2022-02-21 

sleep_brute_time_daily <- sleep_brute_time_daily %>%
  filter(!(day_of_sleep %in% c("2022-02-17","2022-02-18","2022-02-19","2022-02-20","2022-02-21")))

# weekly and monthly

sleep_brute_time_weekly <- sleep_brute_time_daily %>%
  group_by(week, year, week_year) %>%
  summarise(
    mean_week_total_amount_sleep = mean(total_amount_sleep, na.rm = T),
    mean_week_bedtime = mean(bedtime, na.rm = T),
    mean_week_wake_up_time = mean(wake_up_time, na.rm = T)
  ) %>%
  arrange(year, week) %>%
  mutate(date_ref = as.Date(paste0(week_year,"-1"), "%U-%Y-%u"))


sleep_brute_time_monthly <- sleep_brute_time_daily %>%
  group_by(month, year, month_year) %>%
  summarise(
    mean_month_total_amount_sleep = mean(total_amount_sleep, na.rm = T),
    mean_month_bedtime = mean(bedtime, na.rm = T),
    mean_month_wake_up_time = mean(wake_up_time, na.rm = T)
  ) %>%
  arrange(year, month) %>%
  mutate(date_ref = as.Date(paste0(month_year,"-1"), "%m-%Y-%d"))

#

sleep_brute_phases <- sleep_brute_phases %>%
  left_join(select(sleep_brute_time_daily, day_of_sleep, total_amount_sleep), by = "day_of_sleep") %>%
  mutate(time_in_phase = as.numeric(tempo_na_fase, units = "hours")) %>%
  mutate(time_in_phase_perc = time_in_phase/total_amount_sleep) %>%
  mutate(
    phase = case_when(
      stage == 40001 ~ "Awake",
      stage == 40002 ~ "Light Sleep",
      stage == 40003 ~ "REM",
      stage == 40004 ~ "Deep Sleep"
    )
  ) %>%
  arrange(day_of_sleep) %>%
  mutate(
    week = week(day_of_sleep),
    month = month(day_of_sleep),
    year = year(day_of_sleep),
    week_year = str_c(week, "-", year),
    month_year = str_c(month, "-", year)
  )

sleep_brute_phases_weekly <- sleep_brute_phases %>%
  group_by(phase, week, year, week_year) %>%
  summarise(
    mean_week_total_amount_sleep = mean(total_amount_sleep, na.rm = T),
    mean_week_time_in_phase = mean(time_in_phase, na.rm = T),
    mean_week_time_in_phase_perc = mean(time_in_phase_perc, na.rm = T)
  ) %>%
  arrange(year, week) %>%
  mutate(date_ref = as.Date(paste0(week_year,"-1"), "%U-%Y-%u"))


sleep_brute_phases_monthly <- sleep_brute_phases %>%
  group_by(phase, month, year, month_year) %>%
  summarise(
    mean_month_total_amount_sleep = mean(total_amount_sleep, na.rm = T),
    mean_month_time_in_phase = mean(time_in_phase, na.rm = T),
    mean_month_time_in_phase_perc = mean(time_in_phase_perc, na.rm = T)
  ) %>%
  arrange(year, month) %>%
  mutate(date_ref = as.Date(paste0(month_year,"-1"), "%m-%Y-%d"))
  

# heart rate data

heart <- read_csv("dados/s_health06_10/samsunghealth_gustavo.utpott_20231006175776/com.samsung.shealth.tracker.heart_rate.20231006175776.csv", skip = 1) %>%
  mutate(`com.samsung.health.heart_rate.start_time` = `com.samsung.health.heart_rate.start_time` + lubridate::hours( as.numeric(str_remove(str_extract(`com.samsung.health.heart_rate.time_offset`, "^......"),"^...")))) %>%
  mutate(`com.samsung.health.heart_rate.end_time` = `com.samsung.health.heart_rate.end_time` + lubridate::hours( as.numeric(str_remove(str_extract(`com.samsung.health.heart_rate.time_offset`, "^......"),"^..."))))

# resting heart rate (RHR)
# take minimum heart rate in the day in which i'm not sleeping
# join with sleep data to get time of day where i'm not sleeping

rest_heart_rate_brute <- heart %>%
  mutate(day_of_sleep = ifelse(hour(`com.samsung.health.heart_rate.start_time`)>12, as_date(`com.samsung.health.heart_rate.start_time`), as_date(`com.samsung.health.heart_rate.start_time`)-days(1))) %>%
  mutate(day_of_sleep = as_date(day_of_sleep)) %>%
  left_join(sleep_brute_time_daily, by = "day_of_sleep") %>%
  filter(!(`com.samsung.health.heart_rate.start_time` > start_sleep_time_first & `com.samsung.health.heart_rate.start_time` < end_sleep_time_first & !is.na(start_sleep_time_first))) %>%
  filter(!(`com.samsung.health.heart_rate.end_time` > start_sleep_time_first & `com.samsung.health.heart_rate.start_time` < end_sleep_time_first & !is.na(start_sleep_time_first))) %>%
  filter(!(`com.samsung.health.heart_rate.start_time` > start_sleep_time_second & `com.samsung.health.heart_rate.start_time` < end_sleep_time_second & !is.na(start_sleep_time_second))) %>%
  filter(!(`com.samsung.health.heart_rate.end_time` > start_sleep_time_second & `com.samsung.health.heart_rate.start_time` < end_sleep_time_second & !is.na(start_sleep_time_second))) %>%
  filter(!(`com.samsung.health.heart_rate.start_time` > start_sleep_time_third & `com.samsung.health.heart_rate.start_time` < end_sleep_time_third & !is.na(start_sleep_time_third))) %>%
  filter(!(`com.samsung.health.heart_rate.end_time` > start_sleep_time_third & `com.samsung.health.heart_rate.start_time` < end_sleep_time_third & !is.na(start_sleep_time_third))) %>%
  mutate(reference_day = as_date(`com.samsung.health.heart_rate.start_time` - lubridate::hours(3))) %>%
  group_by(reference_day) %>%
  summarise(
    minimo = min(`com.samsung.health.heart_rate.min`, na.rm = T),
    maximo = max(`com.samsung.health.heart_rate.max`, na.rm = T)
  ) %>%
  filter(reference_day >= "2020-11-30") %>%
  arrange(reference_day) %>%
  mutate(
    week = week(reference_day),
    month = month(reference_day),
    year = year(reference_day),
    week_year = str_c(week, "-", year),
    month_year = str_c(month, "-", year)
  )


rest_heart_rate_brute_weekly <- rest_heart_rate_brute %>%
  group_by(week, year, week_year) %>%
  summarise(
    mean_week_heart_min = mean(minimo, na.rm = T),
    mean_week_heart_max = mean(maximo, na.rm = T)
  ) %>%
  arrange(year, week) %>%
  mutate(date_ref = as.Date(paste0(week_year,"-1"), "%U-%Y-%u"))


rest_heart_rate_brute_monthly <- rest_heart_rate_brute %>%
  group_by(month, year, month_year) %>%
  summarise(
    mean_month_heart_min = mean(minimo, na.rm = T),
    mean_month_heart_max = mean(maximo, na.rm = T)
  ) %>%
  arrange(year, month) %>%
  mutate(date_ref = as.Date(paste0(month_year,"-1"), "%m-%Y-%d"))

mean(rest_heart_rate_brute$maximo)

# data from my second watch is more reliable

# but even just considering my second watch, there are still some outliers
# and probable some measurement error.
# so analysing this data, i've come across setting all minimun heart rates less than 40 as 
# NA

rest_heart_rate_brute <- rest_heart_rate_brute %>%
  mutate(minimo = ifelse(minimo<40, NA, minimo))

# heartbeats

heartbeats_brute <- heart %>%
  mutate(time_span = `com.samsung.health.heart_rate.end_time`-`com.samsung.health.heart_rate.start_time`) %>%
  filter(time_span > 3500) %>%
  mutate(reference_day = as_date(`com.samsung.health.heart_rate.start_time`)) %>%
  group_by(reference_day) %>%
  summarise(
    daily_time = sum(time_span),
    mean_heart_rate = mean(`com.samsung.health.heart_rate.heart_rate`)
  ) %>%
  arrange(reference_day) %>%
  mutate(
    week = week(reference_day),
    month = month(reference_day),
    year = year(reference_day),
    week_year = str_c(week, "-", year),
    month_year = str_c(month, "-", year)
  )

# 

heartbeats_brute_weekly <- heartbeats_brute %>%
  group_by(week, year, week_year) %>%
  summarise(
    mean_week_heartbeats = mean(mean_heart_rate, na.rm = T)
  ) %>%
  arrange(year, week) %>%
  mutate(date_ref = as.Date(paste0(week_year,"-1"), "%U-%Y-%u"))


heartbeats_brute_monthly <- heartbeats_brute %>%
  group_by(month, year, month_year) %>%
  summarise(
    mean_month_heartbeats = mean(mean_heart_rate, na.rm = T)
  ) %>%
  arrange(year, month) %>%
  mutate(date_ref = as.Date(paste0(month_year,"-1"), "%m-%Y-%d"))

# make check box for using data of galaxy watch 4 and galaxy fit 2.

min_date <- min(activities_brute$date)
max_date <- max(activities_brute$date)

min_date_sleep <- min(sleep_brute$day_of_sleep)
max_date_sleep <- max(sleep_brute$day_of_sleep)
