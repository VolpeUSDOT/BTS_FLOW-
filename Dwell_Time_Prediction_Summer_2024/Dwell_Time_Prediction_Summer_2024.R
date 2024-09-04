#access packages
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(nnet)
library(glmnet)
library(caret)
library(randomForest)
library(rpart)
library(rattle)
library(lmtest)
library(DBI)
library(odbc)

#upload data files

## window from Jan 1, 2023 to present (May 6), viewing from POV of May 1, 2024

## The below files contain confidential information and are not shared.

bookings <- read_csv("SAV_OC_Aggregated.csv")

throughput <- read_excel("SAV_THROUGHPUT_2.xlsx")

terminal <- read_csv("SAV_terminal_utilization.csv")

SAV_PO <- read_csv("SAV_PO.csv")

SAV_PO_Agg <- read_csv("SAV_PO_Aggregated.csv")

trucking <- read_csv("SAV_trucking_utilization.csv")

warehouse <- read_csv("SAV_warehouse_utilization.csv")

GPA_gatemoves <- read_csv("GPA_gatemoves.csv")

GPA_dwell_dist <- read_csv("GPA_dwell_distributions.csv")

GPA_avg_dwell <- read_csv("GPA_Avg_dwell.csv")

week0 <- read_csv("march17week0.csv")

week1 <- read_csv("march17week1.csv")

week2 <- read_csv("march17week2.csv")

week3 <- read_csv("march17week3.csv")

week4 <- read_csv("march17week4.csv")

------------------------------------------------------------------
# Data Clean up

## time machine data

week0$DISCHARGE_DATE <- as.Date(week0$DISCHARGE_DATE, format = "%d-%b-%y")
week1$DISCHARGE_DATE <- as.Date(week1$DISCHARGE_DATE, format = "%d-%b-%y")
week2$DISCHARGE_DATE <- as.Date(week2$DISCHARGE_DATE, format = "%d-%b-%y")
week3$DISCHARGE_DATE <- as.Date(week3$DISCHARGE_DATE, format = "%d-%b-%y")
week4$DISCHARGE_DATE <- as.Date(week4$DISCHARGE_DATE, format = "%d-%b-%y")
  
## trucking data

trucking$DATE <- ymd(trucking$ASSET_DATE)
warehouse$DATE <- ymd(warehouse$ASSET_DATE)

trucking_agg <- trucking %>%
  group_by(DATE) %>%
  summarise(total_tractors = sum(AVAILABLE_TRACTORS, na.rm = TRUE),
            total_20_chassis = sum(AVAILABLE_CHASSIS_20, na.rm = TRUE),
            total_40_chassis = sum(AVAILABLE_CHASSIS_40, na.rm = TRUE))

## PO data

SAV_PO_Agg$DISCHARGE_DATE <- ymd(SAV_PO_Agg$DATE_KEY)

SAV_PO_Agg <- SAV_PO_Agg %>%
  mutate(PO_TEUs = case_when(
    CONTAINER_SIZE == '20' ~ as.numeric(TOTAL_CONTAINER_COUNT * 1),
    CONTAINER_SIZE %in% c('40', '40HC') ~ as.numeric(TOTAL_CONTAINER_COUNT * 2),
    CONTAINER_SIZE == '45' ~ as.numeric(TOTAL_CONTAINER_COUNT * 2.25),
    TRUE ~ NA_real_))

## bookings

bookings$DISCHARGE_DATE <- as.Date(bookings$DISCHARGE_DATE, format = "%d-%b-%y")

SAV_dailyTEUs <- bookings %>% 
  group_by(DISCHARGE_DATE) %>% 
    summarize(total_TEUS_discharged = sum(TEUS))

bookings <- bookings %>%
  mutate(cont_count = case_when(
    CONTAINER_SIZE == '20' ~ as.numeric(TEUS / 1),
    CONTAINER_SIZE %in% c('40', '40HC') ~ as.numeric(TEUS / 2),
    CONTAINER_SIZE %in% c('45', '45HC') ~ as.numeric(TEUS / 2.25),
    TRUE ~ NA_real_)) 

SAV_dailybookings <- bookings %>% 
  group_by(DISCHARGE_DATE) %>% 
  summarize(total_containers_discharged = sum(cont_count))

## Daily Loading (throughput rate)

throughput$DATE_OCCURRED <- as.Date(throughput$DATE_OCCURRED)

SAV_dailyloading <- throughput %>% 
  group_by(DATE_OCCURRED) %>%
  summarize(total_loadedin = sum(as.numeric(LOAD_IN)), 
            total_loadedout = sum(as.numeric(LOAD_OUT)))

SAV_dailyloading$total_loadedin_TEUs = SAV_dailyloading$total_loadedin * 2
SAV_dailyloading$total_loadedout_TEUs = SAV_dailyloading$total_loadedout * 2

SAV_dailyloading <- SAV_dailyloading %>%
  mutate(change_throughput_TEUs = total_loadedin_TEUs - total_loadedout_TEUs,
         throughput_rate = total_loadedout / total_loadedin)

## Daily terminal (slot utilization)

terminal$DATE_KEY <- ymd(terminal$DATE_KEY)

terminal <- terminal %>%
  mutate(inventoried_import_containers = case_when(
    CONTAINER_SIZE == 20 ~ as.numeric(INVENTORIED_IMPORT / 1),
    CONTAINER_SIZE == 40 ~ as.numeric(INVENTORIED_IMPORT / 2),
    CONTAINER_SIZE == 45 ~ as.numeric(INVENTORIED_IMPORT / 2.25),
    TRUE ~ NA_real_),
    inventoried_export_containers = case_when(
      CONTAINER_SIZE == 20 ~ as.numeric(INVENTORIED_EXPORT / 1),
      CONTAINER_SIZE == 40 ~ as.numeric(INVENTORIED_EXPORT / 2),
      CONTAINER_SIZE == 45 ~ as.numeric(INVENTORIED_EXPORT / 2.25),
      TRUE ~ NA_real_),
    inventoried_empty_containers = case_when(
      CONTAINER_SIZE == 20 ~ as.numeric(INVENTORIED_EMPTY / 1),
      CONTAINER_SIZE == 40 ~ as.numeric(INVENTORIED_EMPTY / 2),
      CONTAINER_SIZE == 45 ~ as.numeric(INVENTORIED_EMPTY / 2.25),
      TRUE ~ NA_real_),
  ) 

total_container_TEUs = 206740

SAV_dailyterminal <- terminal %>%
  group_by(DATE_KEY) %>%
  summarise(total_inventoried_import_TEUs = sum(INVENTORIED_IMPORT, na.rm = TRUE),
            total_inventoried_export_TEUs = sum(INVENTORIED_EXPORT, na.rm = TRUE),
            total_inventoried_empty_TEUs = sum(INVENTORIED_EMPTY, na.rm = TRUE),
            
            total_inventoried_import_containers = sum(inventoried_import_containers, na.rm = TRUE),
            total_inventoried_export_containers = sum(inventoried_export_containers, na.rm = TRUE),
            total_inventoried_empty_containers = sum(inventoried_empty_containers, na.rm = TRUE),
            total_inventoried_containers = sum(ifelse(is.na(inventoried_import_containers), 0, inventoried_import_containers),
                                               ifelse(is.na(inventoried_export_containers), 0, inventoried_export_containers),
                                               ifelse(is.na(inventoried_empty_containers), 0, inventoried_empty_containers)),
            
            slot_utilization = sum(total_inventoried_import_TEUs,
                                   total_inventoried_export_TEUs,
                                   total_inventoried_empty_TEUs)/total_container_TEUs)

SAV_dailyterminal <- SAV_dailyterminal %>% 
  arrange(desc(DATE_KEY)) %>%
  mutate(change_in_import_inventory = total_inventoried_import_containers - lead(total_inventoried_import_containers),
         change_in_export_inventory = total_inventoried_export_TEUs - lead(total_inventoried_export_TEUs),
         change_in_empty_inventory = total_inventoried_empty_containers - lead(total_inventoried_empty_containers)) 

## average dwell

GPA_avg_dwell$DISCHARGE_DATE <- as.Date(GPA_avg_dwell$OUT_TIME, format = "%m/%d/%Y")

GPA_avg_dwell <- GPA_avg_dwell %>% 
  select(DISCHARGE_DATE, IMPORT_DWELL) %>%
  rename(date = DISCHARGE_DATE, avg_dwell = IMPORT_DWELL) 

## dwell distribution

dwell_dist_pivot <- GPA_dwell_dist %>%
  pivot_wider(names_from = 'DWELL', values_from = 'COUNT(IMPORT_CTRS)', names_prefix = "count_") %>%
  mutate(date = as.Date(TIME_OUT,  format = "%m/%d/%Y")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  select(date, starts_with("count_")) %>%
  select("date", "count_0", paste0("count_", 1:30), everything())

dwell_dist <- dwell_dist_pivot %>%
  mutate(total_count = rowSums(dwell_dist_pivot[, -1])) %>%
  mutate(across(starts_with("count_"), ~ ./total_count)) %>%
  select(-total_count) %>%
  mutate(prob_dist = list(as.list(as.numeric(unlist(select(., starts_with("count_")))))))


## merge dwell times, terminal utilization and throughput rate

terminal_throughput <- left_join(SAV_dailyterminal %>% select (DATE_KEY, slot_utilization), 
                                 SAV_dailyloading %>% select(DATE_OCCURRED, throughput_rate), 
                                 by = c("DATE_KEY" = "DATE_OCCURRED")) %>%
  filter(DATE_KEY >= as.Date("2023-01-01") & DATE_KEY <= as.Date("2024-05-01"))


SAV_output <- left_join(GPA_avg_dwell, terminal_throughput, by = c("date" = "DATE_KEY"))
SAV_output <- SAV_output %>%
  mutate(day_of_week = wday(date, label = TRUE),
         month = month(date, label = TRUE))
  
## ISO code

SAV_bookings_daily <- bookings %>%
  mutate(ISO_code = case_when(
    CONTAINER_SIZE == '20' & CONTAINER_TYPE == 'non-reefer' ~ '20GP',
    CONTAINER_SIZE == '20' & CONTAINER_TYPE == 'reefer' ~ '20RT',
    CONTAINER_SIZE == '40' & CONTAINER_TYPE == 'non-reefer' ~ '40GP',
    CONTAINER_SIZE == '40' & CONTAINER_TYPE == 'reefer' ~ '40RT',
    CONTAINER_SIZE == '40HC' & CONTAINER_TYPE == 'reefer' ~ '42RT',
    CONTAINER_SIZE == '40HC' & CONTAINER_TYPE == 'non-reefer' ~ '42GP',
    CONTAINER_SIZE == '45HC' & CONTAINER_TYPE == 'non-reefer' ~ '47GP',
    CONTAINER_SIZE == '45HC' & CONTAINER_TYPE == 'reefer' ~ '47RT',
    CONTAINER_SIZE == '45' & CONTAINER_TYPE == 'non-reefer' ~ '45GP',
    CONTAINER_SIZE == '45' & CONTAINER_TYPE == 'reefer' ~ '45RT',
    TRUE ~ NA_character_)) %>%
  group_by(DISCHARGE_DATE, ISO_code) %>%
  summarize(count_by_ISO = sum(cont_count)) %>%
  ungroup()

ISO_counts <- SAV_bookings_daily %>%
  pivot_wider(names_from = ISO_code, values_from = count_by_ISO, values_fill = 0) %>%
  filter(DISCHARGE_DATE >= as.Date("2023-01-01") & DISCHARGE_DATE <= as.Date("2024-05-01"))

## by company 

SAV_bookings_daily <- bookings %>%
  group_by(DISCHARGE_DATE, COMPANY) %>%
  summarize(count_by_company = sum(cont_count)) %>%
  ungroup()

company_counts <- SAV_bookings_daily %>%
  pivot_wider(names_from = COMPANY, values_from = count_by_company, values_fill = 0) %>%
  filter(DISCHARGE_DATE >= as.Date("2023-01-01") & DISCHARGE_DATE <= as.Date("2024-05-01"))

## by container movement
SAV_bookings_daily <- bookings %>%
  group_by(DISCHARGE_DATE, CONTAINER_MOVEMENT) %>%
  summarize(count_by_movement = sum(cont_count)) %>%
  ungroup()

movement_counts <- SAV_bookings_daily %>%
  pivot_wider(names_from = CONTAINER_MOVEMENT, values_from = count_by_movement, values_fill = 0)  %>%
  filter(DISCHARGE_DATE >= as.Date("2023-01-01") & DISCHARGE_DATE <= as.Date("2024-05-01"))

## by trucking type

trucking_counts <- trucking_agg %>%
  rename(DISCHARGE_DATE = DATE)

## merge them all together

#SAV_merged <- left_join(SAV, ISO_counts, by = c("date" = "DISCHARGE_DATE")) %>%
  #left_join(., company_counts, by = c("date" = "DISCHARGE_DATE")) %>%
  #left_join(., movement_counts, by = c("date" = "DISCHARGE_DATE")) %>%
  #left_join(., trucking_counts, by = c("date" = "DISCHARGE_DATE")) %>%
  #mutate_if(is.numeric, ~if_else(is.na(.), 0, .))

-----------------------------------------------------------------------------------------------
# Data exploration 
  
## import container average dwell times
  
summary(GPA_avg_dwell$avg_dwell) 

### identify outliers

mean_avg_dwell = mean(GPA_avg_dwell$avg_dwell)
sd_avg_dwell = sd(GPA_avg_dwell$avg_dwell)

SAV_output <- SAV_output %>%
 mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell )

GPA_avg_dwell$outlier <- abs(scale(GPA_avg_dwell$avg_dwell)) > 2.5

cleaned_SAV_output <- SAV_output[abs(SAV_output$zscores_dwell) < 2.5, ]

avg_dwell_low_outliers <- SAV_output %>%  # identify outliers with dwell times < 1 
  filter(avg_dwell < 1) 

nrow(avg_dwell_low_outliers)

avg_dwell_high_outliers <- SAV_output %>%  # identify outliers with dwell times > 5
  filter(avg_dwell > 5) 

nrow(avg_dwell_high_outliers)

### plot distribution

ggplot(data = GPA_avg_dwell, aes(x = date)) +
  geom_line(aes(y = avg_dwell), color = "blue", na.rm = TRUE) +
  geom_point(data = subset(GPA_avg_dwell, outlier == TRUE), aes(y = avg_dwell), color = "red", na.rm = TRUE) +
  labs(x = "Date", y = "Days") +
  ggtitle("Historical Average Import Container Dwell Times") +
  theme_minimal()

hist(GPA_avg_dwell$avg_dwell, main = "Distribution of Average Import Container Dwell Times",
     xlab = "Days",
     col = "darkblue", breaks = 30)

## gate throughput rate

numerical_columns <- setdiff(names(SAV_dailyloading), c('DATE_OCCURRED'))

SAV_dailyloading <- SAV_dailyloading %>%
  mutate(across(all_of(numerical_columns), ~ifelse(is.finite(.), ., NA)))

summary(SAV_dailyloading$throughput_rate) 

### identify outliers 
mean_throughput = mean(SAV_dailyloading$throughput_rate, na.rm = TRUE)
sd_throughput = sd(SAV_dailyloading$throughput_rate, na.rm = TRUE) 

SAV_output <- SAV_output %>%
  mutate(zscores_throughput = (throughput_rate - mean_throughput) / sd_throughput)

throughput_outliers <- SAV_output %>%  # identify outliers with dwell times > 5
  filter(abs(zscores_throughput) >= 2.5) 

View(throughput_outliers)

cleaned_SAV_output <- cleaned_SAV_output %>% 
  mutate(zscores_throughput = (throughput_rate - mean_throughput) / sd_throughput) %>%
  filter(abs(zscores_throughput) < 2.5) 

### plot distribution

ggplot(data = SAV_dailyloading, aes(x = DATE_OCCURRED)) +
  geom_line(aes(y = throughput_rate), color = "blue", na.rm = TRUE) +
  labs(x = "Date") +
  ggtitle("Historical Throughput Rates") +
  theme_minimal()

## slot utilization

summary(SAV_dailyterminal$slot_utilization)

### identify outliers 

mean_util = mean(SAV_dailyterminal$slot_utilization, na.rm = TRUE)
sd_util = sd(SAV_dailyterminal$slot_utilization, na.rm = TRUE) 

SAV_output <- SAV_output %>%
  mutate(zscores_util = (slot_utilization - mean_util) / sd_util)

util_outliers <- SAV_output %>%  # identify outliers with dwell times > 5
  filter(abs(zscores_util) >= 2.5) 

View(util_outliers)

cleaned_SAV_output <- cleaned_SAV_output %>% 
  mutate(zscores_util = (slot_utilization - mean_util) / sd_util) %>%
  filter(abs(zscores_util) < 2.5) 

### plot distribution

ggplot(data = SAV_dailyterminal, aes(x = DATE_KEY)) +
  geom_line(aes(y = slot_utilization), color = "blue", na.rm = TRUE) +
  labs(x = "Date") +
  ggtitle("Historical Slot Utilization Rates") +
  theme_minimal()

-----------------------------------------------------------------------------------------------
  
# Correlation Analysis between average dwell time, slot utilization, and throughput rate

cleaned_SAV_output <- na.omit(cleaned_SAV_output) %>%
  select(avg_dwell, slot_utilization, throughput_rate)

cor(cleaned_SAV_output) 

plot(cleaned_SAV_output$avg_dwell, cleaned_SAV_output$throughput_rate,
     xlab = "Average Dwell Time", ylab = "Throughput Rate")

plot(cleaned_SAV_output$avg_dwell, cleaned_SAV_output$slot_utilization,
     xlab = "Average Dwell Time", ylab = "Slot Utilization Rate")

plot(cleaned_SAV_output$slot_utilization, cleaned_SAV_output$throughput_rate,
     xlab = "Slot Utilization Rate", ylab = "Throughput Rate")

ggplot(cleaned_SAV_output, aes(x = date)) +
  geom_line(aes(y = throughput_rate, color = "Throughput Rate"), size = 1) +
  geom_line(aes(y = slot_utilization, color = "Slot Utilization Rate"), size = 1) +
  geom_line(aes(y = avg_dwell, color = "Average Dwell Time"), size = 1) +
  labs(title = "SAV Metrics Jan 2023 - May 2024",
       x = "Date",
       y = "Values",
       color = "Metrics") +
  scale_color_manual(values = c(
    "Throughput Rate" = "red",
    "Slot Utilization Rate" = "green",
    "Average Dwell Time" = "blue"
  )) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()
-----------------------------------------------------------------------------------------

# created Lagged dataset

# all data 
  
lag_data <- bookings %>%
  group_by(DISCHARGE_DATE) %>%
  rename(date = DISCHARGE_DATE) %>%
  summarise(inbound_TEUs = sum(TEUS))

lag_data <- left_join(lag_data, GPA_avg_dwell %>% select(date, avg_dwell), 
                      by = "date")

lagged_data <- lag_data %>%
  mutate(lag_1 = lag(inbound_TEUs)) %>%
  mutate(lag_2 = lag(lag_1)) %>%
  mutate(lag_3 = lag(lag_2)) %>%
  mutate(lag_4 = lag(lag_3)) %>%
  mutate(lag_5 = lag(lag_4)) %>%
  mutate(lag_6 = lag(lag_5)) %>%
  mutate(lag_7 = lag(lag_6)) %>%
  mutate(lag_8 = lag(lag_7)) %>%
  mutate(lag_9 = lag(lag_8)) %>%
  mutate(lag_10 = lag(lag_9)) %>%
  mutate(lag_11 = lag(lag_10)) %>%
  mutate(lag_12 = lag(lag_11)) %>%
  filter(date >= as.Date("2023-01-16") & date <= as.Date("2024-05-01")) %>%
  rename(lag_0 = inbound_TEUs) %>%
  select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

lagged_data <- lagged_data %>%
 mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell ) %>%
 filter(abs(zscores_dwell) < 2.5)

nrow(lagged_data) # after filtering for outliers, 433 entries instead of 441

# what discharge lag is most correlated with average container dwell times?

compute_correlation <- function(df) {
  lag_columns <- grep("^lag_", names(df), value = TRUE)
  correlations <- lapply(lag_columns, function(col) {
    filtered_df <- df[complete.cases(df$avg_dwell, df[[col]]), ]

    cor_value <- cor(filtered_df$avg_dwell, filtered_df[[col]])
    p_value <- cor.test(filtered_df$avg_dwell, filtered_df[[col]])$p.value
    return(list(correlation = cor_value, p_value = p_value))
  })
  names(correlations) <- lag_columns
  return(correlations)
}

correlations <- compute_correlation(lagged_data)
View(correlations)

correlation_df <- do.call(rbind, correlations)
correlation_df <- as.data.frame(do.call(rbind, correlations))
correlation_df$lag_days <- gsub("^lag_", "", rownames(correlation_df))
correlation_df <- correlation_df[, c("lag_days", "correlation", "p_value")]

correlation_df$correlation <- as.numeric(correlation_df$correlation)
correlation_df$p_value <- as.numeric(correlation_df$p_value)

write.csv(correlation_df, "correlation_results.csv", row.names = FALSE)

## control for weekend/weekday effects by taking out weekends

lagged_weekday <- lagged_data %>%
  mutate(weekday = weekdays(date)) %>%
  filter(!(weekday %in% c("Saturday", "Sunday"))) %>%
  select(-weekday)

correlations_weekday <- compute_correlation(lagged_weekday)

correlation_weekday_df <- do.call(rbind, correlations_weekday)
correlation_weekday_df <- as.data.frame(do.call(rbind, correlations_weekday))
correlation_weekday_df$lag_days <- gsub("^lag_", "", rownames(correlation_weekday_df))
correlation_weekday_df <- correlation_weekday_df[, c("lag_days", "correlation", "p_value")]

correlation_weekday_df$correlation <- as.numeric(correlation_weekday_df$correlation)
correlation_weekday_df$p_value <- as.numeric(correlation_weekday_df$p_value)

# Test Week

lagged_test_data <- lag_data %>%
  mutate(lag_1 = lag(inbound_TEUs)) %>%
  mutate(lag_2 = lag(lag_1)) %>%
  mutate(lag_3 = lag(lag_2)) %>%
  mutate(lag_4 = lag(lag_3)) %>%
  mutate(lag_5 = lag(lag_4)) %>%
  mutate(lag_6 = lag(lag_5)) %>%
  mutate(lag_7 = lag(lag_6)) %>%
  mutate(lag_8 = lag(lag_7)) %>%
  mutate(lag_9 = lag(lag_8)) %>%
  mutate(lag_10 = lag(lag_9)) %>%
  mutate(lag_11 = lag(lag_10)) %>%
  mutate(lag_12 = lag(lag_11)) %>%
  filter(date >= as.Date("2024-03-17") & date <= as.Date("2024-03-23")) %>%
  rename(lag_0 = inbound_TEUs) %>%
  select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

# Week 0

lag_data0 <- week0 %>%
  group_by(DISCHARGE_DATE) %>%
  summarise(inbound_TEUs = sum(TEUS))

lag_data0 <- left_join(lag_data0, GPA_avg_dwell %>% select(DISCHARGE_DATE, IMPORT_DWELL), 
                      by = "DISCHARGE_DATE")

lag_data0 <- lag_data0 %>% rename(avg_dwell = IMPORT_DWELL, date = DISCHARGE_DATE) 

lagged_data0 <- lag_data0 %>%
  mutate(lag_1 = lag(inbound_TEUs)) %>%
  mutate(lag_2 = lag(lag_1)) %>%
  mutate(lag_3 = lag(lag_2)) %>%
  mutate(lag_4 = lag(lag_3)) %>%
  mutate(lag_5 = lag(lag_4)) %>%
  mutate(lag_6 = lag(lag_5)) %>%
  mutate(lag_7 = lag(lag_6)) %>%
  mutate(lag_8 = lag(lag_7)) %>%
  mutate(lag_9 = lag(lag_8)) %>%
  mutate(lag_10 = lag(lag_9)) %>%
  mutate(lag_11 = lag(lag_10)) %>%
  mutate(lag_12 = lag(lag_11)) %>%
  filter(date >= as.Date("2023-01-13") & date <= as.Date("2024-03-17")) %>%
  rename(lag_0 = inbound_TEUs) %>%
  select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

lagged_data0 <- lagged_data0 %>%
  mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell ) %>%
  filter(abs(zscores_dwell) < 2.5) %>%
  filter(complete.cases(.))

nrow(lagged_data0) # 337 items

# Week 1

lag_data1 <- week1 %>%
  group_by(DISCHARGE_DATE) %>%
  summarise(inbound_TEUs = sum(TEUS))

lag_data1 <- left_join(lag_data1, GPA_avg_dwell %>% select(DISCHARGE_DATE, IMPORT_DWELL), 
                       by = "DISCHARGE_DATE")

lag_data1 <- lag_data1 %>% rename(avg_dwell = IMPORT_DWELL, date = DISCHARGE_DATE) 

lagged_data1 <- lag_data1 %>%
  mutate(lag_1 = lag(inbound_TEUs)) %>%
  mutate(lag_2 = lag(lag_1)) %>%
  mutate(lag_3 = lag(lag_2)) %>%
  mutate(lag_4 = lag(lag_3)) %>%
  mutate(lag_5 = lag(lag_4)) %>%
  mutate(lag_6 = lag(lag_5)) %>%
  mutate(lag_7 = lag(lag_6)) %>%
  mutate(lag_8 = lag(lag_7)) %>%
  mutate(lag_9 = lag(lag_8)) %>%
  mutate(lag_10 = lag(lag_9)) %>%
  mutate(lag_11 = lag(lag_10)) %>%
  mutate(lag_12 = lag(lag_11)) %>%
  filter(date >= as.Date("2023-01-13") & date <= as.Date("2024-03-17")) %>%
  rename(lag_0 = inbound_TEUs) %>%
  select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

lagged_data1 <- lagged_data1 %>%
  mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell ) %>%
  filter(abs(zscores_dwell) < 2.5) %>%
  filter(complete.cases(.))

nrow(lagged_data1) # 337 items

# Week 2

lag_data2 <- week2 %>%
  group_by(DISCHARGE_DATE) %>%
  summarise(inbound_TEUs = sum(TEUS))

lag_data2 <- left_join(lag_data2, GPA_avg_dwell %>% select(DISCHARGE_DATE, IMPORT_DWELL), 
                       by = "DISCHARGE_DATE")

lag_data2 <- lag_data2 %>% rename(avg_dwell = IMPORT_DWELL, date = DISCHARGE_DATE) 

lagged_data2 <- lag_data2 %>%
  mutate(lag_1 = lag(inbound_TEUs)) %>%
  mutate(lag_2 = lag(lag_1)) %>%
  mutate(lag_3 = lag(lag_2)) %>%
  mutate(lag_4 = lag(lag_3)) %>%
  mutate(lag_5 = lag(lag_4)) %>%
  mutate(lag_6 = lag(lag_5)) %>%
  mutate(lag_7 = lag(lag_6)) %>%
  mutate(lag_8 = lag(lag_7)) %>%
  mutate(lag_9 = lag(lag_8)) %>%
  mutate(lag_10 = lag(lag_9)) %>%
  mutate(lag_11 = lag(lag_10)) %>%
  mutate(lag_12 = lag(lag_11)) %>%
  filter(date >= as.Date("2023-01-13") & date <= as.Date("2024-03-17")) %>%
  rename(lag_0 = inbound_TEUs) %>%
  select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

lagged_data2 <- lagged_data2 %>%
  mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell ) %>%
  filter(abs(zscores_dwell) < 2.5) %>%
  filter(complete.cases(.))

nrow(lagged_data2) # 337 items

# Week 3

lag_data3 <- week3 %>%
  group_by(DISCHARGE_DATE) %>%
  summarise(inbound_TEUs = sum(TEUS))

lag_data3 <- left_join(lag_data3, GPA_avg_dwell %>% select(DISCHARGE_DATE, IMPORT_DWELL), 
                       by = "DISCHARGE_DATE")

lag_data3 <- lag_data3 %>% rename(avg_dwell = IMPORT_DWELL, date = DISCHARGE_DATE) 

lagged_data3 <- lag_data3 %>%
  mutate(lag_1 = lag(inbound_TEUs)) %>%
  mutate(lag_2 = lag(lag_1)) %>%
  mutate(lag_3 = lag(lag_2)) %>%
  mutate(lag_4 = lag(lag_3)) %>%
  mutate(lag_5 = lag(lag_4)) %>%
  mutate(lag_6 = lag(lag_5)) %>%
  mutate(lag_7 = lag(lag_6)) %>%
  mutate(lag_8 = lag(lag_7)) %>%
  mutate(lag_9 = lag(lag_8)) %>%
  mutate(lag_10 = lag(lag_9)) %>%
  mutate(lag_11 = lag(lag_10)) %>%
  mutate(lag_12 = lag(lag_11)) %>%
  filter(date >= as.Date("2023-01-13") & date <= as.Date("2024-03-17")) %>%
  rename(lag_0 = inbound_TEUs) %>%
  select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

lagged_data3 <- lagged_data3 %>%
  mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell ) %>%
  filter(abs(zscores_dwell) < 2.5) %>%
  filter(complete.cases(.))

nrow(lagged_data3) # 337 items

# Week 4

lag_data4 <- week4 %>%
  group_by(DISCHARGE_DATE) %>%
  summarise(inbound_TEUs = sum(TEUS))

lag_data4 <- left_join(lag_data4, GPA_avg_dwell %>% select(DISCHARGE_DATE, IMPORT_DWELL), 
                       by = "DISCHARGE_DATE")

lag_data4 <- lag_data4 %>% rename(avg_dwell = IMPORT_DWELL, date = DISCHARGE_DATE) 

lagged_data4 <- lag_data4 %>%
  mutate(lag_1 = lag(inbound_TEUs)) %>%
  mutate(lag_2 = lag(lag_1)) %>%
  mutate(lag_3 = lag(lag_2)) %>%
  mutate(lag_4 = lag(lag_3)) %>%
  mutate(lag_5 = lag(lag_4)) %>%
  mutate(lag_6 = lag(lag_5)) %>%
  mutate(lag_7 = lag(lag_6)) %>%
  mutate(lag_8 = lag(lag_7)) %>%
  mutate(lag_9 = lag(lag_8)) %>%
  mutate(lag_10 = lag(lag_9)) %>%
  mutate(lag_11 = lag(lag_10)) %>%
  mutate(lag_12 = lag(lag_11)) %>%
  filter(date >= as.Date("2023-01-13") & date <= as.Date("2024-03-17")) %>%
  rename(lag_0 = inbound_TEUs) %>%
  select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

lagged_data4 <- lagged_data4 %>%
  mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell ) %>%
  filter(abs(zscores_dwell) < 2.5) %>%
  filter(complete.cases(.))

nrow(lagged_data4) # 337 items

-------------------------------------------------------------------------------------------
# Examine Day of Week Effects

## plot gate moves by weekend vs weekday
  
SAV_weekends <- SAV_dailyloading %>%
  mutate(day = weekdays(DATE_OCCURRED)) %>%
  filter(day %in% c("Saturday", "Sunday")) %>%
  select(-day) 

SAV_weekdays <- SAV_dailyloading %>%
  mutate(day = weekdays(DATE_OCCURRED)) %>%
  filter(!(day %in% c("Saturday", "Sunday"))) %>%
  select(-day) 

summary(SAV_weekends$total_loadedin)
summary(SAV_weekends$total_loadedout)

hist(SAV_weekends$total_loadedout, main = "Weekend Distribution of Daily Import + Empty Container Gate Moves", 
     xlab = "Number of Containers Loaded Out", ylab = "Frequency", breaks = 35)

hist(SAV_weekdays$total_loadedout, main = "Weekday Distribution of Daily Import + Empty Container Gate Moves", 
     xlab = "Number of Containers Loaded Out", ylab = "Frequency", breaks = 40)

summary(SAV_weekdays$total_loadedin)
summary(SAV_weekdays$total_loadedout)

hist(SAV_weekends$total_loadedout, 
     main = "Loaded Container Gate Moves Density Distributions", 
     xlab = "Number of Import Containers Loaded Out", ylab = "Density", 
     breaks = 20, 
     col = rgb(0, 0, 1, alpha = 0.5),
     ylim = c(0,0.002),
     xlim = c(0, 6000),
     freq = FALSE)

hist(SAV_weekdays$total_loadedout, 
     breaks = 80, 
     col = rgb(1, 0, 0, alpha = 0.5), 
     add = TRUE, 
     border = "black",
     freq = FALSE)

legend("topright", legend = c("Weekdays", "Weekends"), fill = c("red", "blue"), title = "Day Type")


## incoming TEUs each day box plot

SAV_dailyTEUs <- SAV_dailyTEUs %>%
  mutate(day = weekdays(DISCHARGE_DATE)) %>%
  mutate(day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

ggplot(SAV_dailyTEUs, aes(x = factor(day), y = total_TEUS_discharged)) +
  geom_boxplot() +
  labs(title = "Total TEUs Discharged by Day of the Week",
       x = "Day of the Week",
       y = "Total TEUs Discharged") +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme_minimal()

## average container dwell time by day box plot

GPA_avg_dwell <- GPA_avg_dwell %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

mean_cl_normal <- function(x) {
  m <- mean(x)
  se <- sd(x) / sqrt(length(x))
  ci <- qnorm(0.975) * se
  ymin <- m - ci
  ymax <- m + ci
  return(c(y = m, ymin = ymin, ymax = ymax))
}

ggplot(GPA_avg_dwell, aes(x = day, y = avg_dwell)) +
  geom_boxplot() +
  labs(title = "Average Container Terminal Dwell Times by Day of the Week",
       x = "Day of the Week",
       y = "Average Dwell Time") +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme_minimal()

## histogram of TEUs by weekday vs weekend

weekend_TEUs <- SAV_dailyTEUs %>%
  filter(day %in% c("Saturday", "Sunday"))

weekday_TEUs <- SAV_dailyTEUs %>%
  filter(!(day %in% c("Saturday", "Sunday")))

hist(weekend_TEUs$total_TEUS_discharged, 
     main = "Probability Density Distribution of TEUs discharged", 
     xlab = "TEUs", ylab = "Density", 
     breaks = 40, 
     col = rgb(0, 0, 1, alpha = 0.5),
     ylim = c(0,0.0006),
     freq = FALSE)

hist(weekday_TEUs$total_TEUS_discharged, 
     breaks = 80, 
     col = rgb(1, 0, 0, alpha = 0.5), 
     add = TRUE, 
     border = "black",
     freq = FALSE)

legend("topright", legend = c("Weekdays", "Weekends"), fill = c("red", "blue"), title = "Day Type")

## conduct Mann-Whitney U Test to see if two distributions are the same

wilcox_test_result <- wilcox.test(weekday_TEUs$total_TEUS_discharged, weekend_TEUs$total_TEUS_discharged)
wilcox_test_result

----------------------------------------------------------------------------------------------
  # Month/Seasonality Analysis
  
jan_dwell <-  GPA_avg_dwell %>%
  mutate(month = month(date)) %>%
  filter(month == 1)

month_dwell <-  GPA_avg_dwell %>%
  mutate(month = month(date)) %>%
  filter(month != 1)

mean_jan_dwell = mean(jan_dwell$avg_dwell)
mean_month_dwell = mean(month_dwell$avg_dwell)

mean_jan_dwell - mean_month_dwell

hist(jan_dwell$IMPORT_DWELL, 
     main = "January Dwell Time Distribution", 
     xlab = "Average Daily Dwell Time", ylab = "Frequency", 
     breaks = 40)

hist(month_dwell$IMPORT_DWELL, 
     main = "All Other Months Dwell Time Distribution", 
     xlab = "Average Daily Dwell Time", ylab = "Frequency", 
     breaks = 40)

  ## probability density functions

hist(jan_dwell$avg_dwell, 
     main = "Probability Density Distribution of Container Dwell Times", 
     xlab = "Average Daily Dwell Time", ylab = "Density", 
     breaks = 40, 
     col = rgb(0, 0, 1, alpha = 0.5),
     ylim = c(0,1),
     freq = FALSE)

hist(month_dwell$avg_dwell, 
     breaks = 80, 
     col = rgb(1, 0, 0, alpha = 0.5), 
     add = TRUE, 
     border = "black",
     freq = FALSE)

legend("topright", legend = c("All other months", "January"), fill = c("red", "blue"), title = "Month")

  ## mann whitney U test 
  
wilcox_test_result <- wilcox.test(jan_dwell$avg_dwell, month_dwell$avg_dwell)
wilcox_test_result

----------------------------------------------------------------------------------------------
  # FLOW coverage analysis
  
  SAV_imports <- SAV_dailyterminal %>%
  select(DATE_KEY,change_in_import_inventory)

SAV_imports <- left_join(SAV_imports, SAV_dailybookings, by = c("DATE_KEY" = "DISCHARGE_DATE")) 
SAV_imports <- left_join(SAV_imports, SAV_dailyloading %>% select(DATE_OCCURRED, total_loadedout), by = c("DATE_KEY" = "DATE_OCCURRED")) 

SAV_imports<- SAV_imports[complete.cases(SAV_imports), ]

SAV_imports <- SAV_imports %>%
  mutate(FLOW_gap = change_in_import_inventory - total_containers_discharged + total_loadedout,
         FLOW_coverage = total_containers_discharged / (FLOW_gap + total_containers_discharged) * 100)

summary(SAV_imports$FLOW_gap)

ggplot(data = SAV_imports, aes(x = DATE_KEY)) +
  geom_line(aes(y = FLOW_gap), color = "blue", na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Number of Import Containers") +
  ggtitle("Gap in Imports Covered by FLOW") +
  theme_minimal()

## FLOW Coverage

summary(SAV_imports$FLOW_coverage)

remove_outliers <- function(data, column, threshold = 2.5) {
  mean_val <- mean(data[[column]], na.rm = TRUE)
  sd_val <- sd(data[[column]], na.rm = TRUE)
  
  lower_bound <- mean_val - threshold * sd_val
  upper_bound <- mean_val + threshold * sd_val
  
  filtered_data <- data %>%
    filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
  
  return(filtered_data)
}

filtered_SAV_imports <- remove_outliers(SAV_imports, "FLOW_coverage")

ggplot(data = filtered_SAV_imports, aes(x = DATE_KEY)) +
  geom_line(aes(y = FLOW_coverage), color = "blue", na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Percentage") +
  ggtitle("FLOW Coverage of Imports Discharged") +
  theme_minimal()

outside_range_count = sum(SAV_imports$FLOW_coverage < 0 | SAV_imports$FLOW_coverage > 100)
total = nrow(SAV_imports)

outside_range_count / total 

write.csv(SAV_imports, file = "SAV_FLOW_coverage.csv")
-------------------------------------------------------------------------------
  # compare PO data to OC data
  
  SAV_PO_Agg <- SAV_PO_Agg %>%
  group_by(DISCHARGE_DATE, CONTAINER_MOVEMENT) %>%
  summarise(total_PO_TEUs = sum(PO_TEUs))

OC_PO_comparison <- merge(SAV_dailyTEUs, SAV_PO_Agg[, c('DISCHARGE_DATE', 'CONTAINER_MOVEMENT', 'total_PO_TEUs')],
                          by = c('DISCHARGE_DATE', 'CONTAINER_MOVEMENT'),
                          all.x = TRUE)

OC_PO_comparison <- OC_PO_comparison %>%
  mutate(PO_percent_of_OC = total_PO_TEUs / total_TEUS_discharged * 100)

OC_PO_comparison_filtered <- OC_PO_comparison[!is.na(OC_PO_comparison$PO_percent_of_OC), ]

ggplot(data = OC_PO_comparison_filtered, aes(x = DISCHARGE_DATE)) +
  geom_line(aes(y = total_TEUS_discharged), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = total_PO_TEUs), color = "red", na.rm = TRUE) + 
  labs(x = "Date", y = "TEUs") +
  ggtitle("Purchasing Orders vs Ocean Carrier Bookings") +
  theme_minimal()

# check alignment with monthly BTS loaded import and export container data

monthly_LB_FLOW <- LB_OC_dailyTEUs %>%
  group_by(month = format(DISCHARGE_DATE, "%Y-%m")) %>%
  summarize(total_loaded_FLOWimports = sum(total_TEUS_discharged))
View(monthly_LB_FLOW)

FLOW_vs_BTS <- left_join(monthly_LB_FLOW, LB_BTS_Loaded_Import_Containers %>% select(month, Value), by = ("month" = "month")) %>%
  arrange(desc(month))
View(FLOW_vs_BTS)

FLOW_vs_BTS <- FLOW_vs_BTS %>%
  mutate(raw_diff = Value - total_loaded_FLOWimports,
         FLOW_fraction = total_loaded_FLOWimports/Value) # interestingly, FLOW volumes higher than BTS volumes in Jan 2024. Why? Maybe FLOW inaccurate?

--------------------------------------------------------------------------------------------------
# Split data into training, test, and validation sets 
  
full_model_data <- lagged_data %>% mutate(month = month(date))
full_model_data <- full_model_data[complete.cases(full_model_data$avg_dwell), ] # remove missing values
full_model_data <- full_model_data %>%
  mutate(january = ifelse(month == 1, 1, 0))

train_prop <- 0.8  # 80% for training, 20% for testing

set.seed(123) 

train_data <- full_model_data %>%
  group_by(month) %>%
  sample_frac(train_prop) %>%
  ungroup()

test_data <- anti_join(full_model_data, train_data, by = "date")

nrow(train_data)
nrow(test_data)
nrow(full_model_data)

train_data <- train_data[complete.cases(train_data$avg_dwell), ] # remove missing values from training set

train_data <- train_data %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

test_data <- test_data[complete.cases(test_data$avg_dwell), ] # remove missing values

test_data <- test_data %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

--------------------------------------------------------------------------------------------------
# Feature Selection
  
## Lasso Regression

predictors_train <- model.matrix(~ day + lag_1 + lag_2 + lag_3 + lag_4 +lag_5 + lag_6 + lag_7 + lag_8 + lag_9 +
                             lag_10 + lag_11 + lag_12, data = train_data)[, -1]
response_train <- train_data$avg_dwell

lasso_model <- glmnet(predictors_train, response_train, alpha = 1) # fit lasso model to training data

cv_model <- cv.glmnet(predictors_train, response_train, alpha = 1) # Perform cross-validation to select the optimal lambda value
best_lambda <- cv_model$lambda.min

lasso_model_optimal <- glmnet(predictors_train, response_train, alpha = 1, lambda = best_lambda)

lasso_coefs <- coef(lasso_model_optimal)

coefs_df <- as.data.frame(as.matrix(lasso_coefs))
coefs_df <- tibble::rownames_to_column(coefs_df, "Variable")
colnames(coefs_df)[2] <- "Coefficient"

View(coefs_df)

important_vars <- coefs_df %>% filter(Coefficient != 0) %>% pull(Variable)
important_vars <- important_vars[important_vars != "(Intercept)"]  

print(important_vars)

## evaluate on test set

predictors_test <- model.matrix(~ day + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 +
                                   lag_10 + lag_11 + lag_12, data = test_data)[, -1]
response_test <- test_data$avg_dwell

predictions <- predict(lasso_model_optimal, newx = predictors_test)

rmse <- sqrt(mean((response_test - predictions)^2))
rmse # root mean squared error = 0.606, which is lower than standard deviation, which is a good sign

----------------------------------------------------------------------------------------------
# Train Model to Predict Average Dwell Time
  
train_dummy <- model.matrix(~ day - 1, data = train_data)
train_data <- cbind(train_data, train_dummy)

## model 1a: full model, MLR

train_subset1a <- train_data[, c("avg_dwell", "dayMonday", "dayTuesday", "dayWednesday", "dayThursday", "dayFriday", "daySaturday",
                                   "lag_1", "lag_2", "lag_3", "lag_4", "lag_5", "lag_6", 
                               "lag_7","lag_8", "lag_9", "lag_10", "lag_11", "lag_12")] # subset data to only include important variables + response variable

lm_model1a <- lm(avg_dwell ~ ., data = train_subset1a)

summary(lm_model1a)

## model 1b: MLR, subset model
train_subset1b <- train_data[, c("avg_dwell", "dayMonday", "dayTuesday", "dayWednesday", "dayThursday", "dayFriday", "daySaturday",
                               "lag_1", "lag_2", "lag_6", "lag_7", "lag_8")] # subset data to only include important variables + response variable

lm_model1b <- lm(avg_dwell ~ ., data = train_subset1b)

summary(lm_model1b)

# model 1c: no lag days
train_subset1c <- train_data[, c("avg_dwell", "dayMonday", "dayTuesday", "dayWednesday", "dayThursday", "dayFriday", "daySaturday")] 

lm_model1c <- lm(avg_dwell ~ ., data = train_subset1c)

summary(lm_model1c)

# model 2a: Random forest regression

rf_model <- randomForest(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                           lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 + lag_10 +
                           lag_11 + lag_12, data = train_data, importance = TRUE)

  ## evaluate on training data
training_predictions <- predict(rf_model, newdata = train_data)

train_mse <- mean((train_data$avg_dwell - training_predictions)^2)

rmse = sqrt(train_mse)
rmse 

  ## plot variable importance
varImpPlot(rf_model)

  ## visualize trees

single_tree_model <- rpart(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                             lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 + lag_10 +
                             lag_11 + lag_12, data = train_data)

rpart.plot::rpart.plot(single_tree_model)
--------------------------------------------------------------------------------------------------------
# Validate Model

# Model 1b
  
  # LOO Cross Validation

months <- unique(train_data$month)

cv_metrics <- data.frame(month = character(), rmse = numeric(), stringsAsFactors = FALSE)

for (i in months) {

  train_subdata <- train_data %>% filter(month != i)
  validation_subdata <- train_data %>% filter(month == i)
  
  # Fit the model on training data
  dummy <- model.matrix(~ day - 1, data = train_subdata)
  train_subdata <- cbind(train_subdata, dummy)
  
  train_subset <- train_subdata[, c("avg_dwell", "dayMonday", "dayTuesday", "dayWednesday", "dayThursday", "dayFriday", "daySaturday",
                                    "lag_1", "lag_2", "lag_9", "lag_10", "lag_12")] 
  
  lm_model <- lm(avg_dwell ~ ., data = train_subset)
  
  # Predict on validation data
  validation_subdata <- validation_subdata[complete.cases(validation_subdata$avg_dwell), ]
  
  dummy <- model.matrix(~ day - 1, data = validation_subdata)
  validation_subdata <- cbind(validation_subdata, dummy)
  
  predictions <- predict(lm_model, newdata = validation_subdata)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((validation_subdata$avg_dwell - predictions)^2))
  
  # Store the performance metric
  cv_metrics <- rbind(cv_metrics, data.frame(month = i, rmse = rmse))
}

View(cv_metrics)

mean(cv_metrics$rmse) # average error = 0.47

  # Assess Learning Rate of Model

learning_metrics <- data.frame(month = character(), rmse = numeric(), stringsAsFactors = FALSE)

for (i in 1:11) {
  
  training_set <- train_data %>% filter(month %in% months[1:i])
  validation_set <- train_data %>% filter(month == i + 1)
  
  # Fit the model on training data
  dummy <- model.matrix(~ day - 1, data = training_set)
  training_set <- cbind(training_set, dummy)
  
  training_subset <- training_set[, c("avg_dwell", "dayMonday", "dayTuesday", "dayWednesday", "dayThursday", "dayFriday", "daySaturday",
                                    "lag_1", "lag_2", "lag_9", "lag_10", "lag_12")] 
  
  lm_model <- lm(avg_dwell ~ ., data = training_subset)
  
  # Predict on validation data
  dummy <- model.matrix(~ day - 1, data = validation_set)
  validation_set <- cbind(validation_set, dummy)
  
  predictions <- predict(lm_model, newdata = validation_set)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((validation_set$avg_dwell - predictions)^2))
  
  # Store the performance metric
  learning_metrics <- rbind(learning_metrics, data.frame(month = i, rmse = rmse))
}

View(learning_metrics)

ggplot(learning_metrics, aes(x = month, y = rmse)) + geom_line() + geom_point() + 
  labs(title = "Learning Rate of Model", x = "Month", y = "RMSE") +
  theme_minimal()

# Model 2a

  # LOO Cross Validation

rf_cv_metrics <- data.frame(month = character(), rmse = numeric(), stringsAsFactors = FALSE)

for (i in months) {
  
  train_subdata <- train_data %>% filter(month != i)
  validation_subdata <- train_data %>% filter(month == i)
  
  # Fit model on training data
  rf_model <- randomForest(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                             lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 + lag_10 +
                             lag_11 + lag_12, data = train_subdata, importance = TRUE)
  
  # Predict on validation data
  rf_predictions <- predict(rf_model, newdata = validation_subdata)
  
  # Calculate performance metric (RMSE)
  rmse <- sqrt(mean((validation_subdata$avg_dwell - rf_predictions)^2))
  
  # Store the performance metric
  rf_cv_metrics <- rbind(rf_cv_metrics, data.frame(month = i, rmse = rmse))
}

View(rf_cv_metrics)

mean(rf_cv_metrics$rmse) # average error = 0.679, not as good as MLR model

  # Assess Learning Rate of Model

rf_learning_metrics <- data.frame(month = character(), rmse = numeric(), stringsAsFactors = FALSE)

for (i in 1:11) {
  
  training_set <- train_data %>% filter(month %in% months[1:i])
  validation_set <- train_data %>% filter(month == i + 1)
  
  # Fit the model on training data
  rf_model <- randomForest(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                             lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 + lag_10 +
                             lag_11 + lag_12, data = training_set, importance = TRUE)
  
  
  # Predict on validation data
  rf_predictions <- predict(rf_model, newdata = validation_set)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((validation_set$avg_dwell - rf_predictions)^2))
  
  # Store the performance metric
  rf_learning_metrics <- rbind(rf_learning_metrics, data.frame(month = i, rmse = rmse))
}

View(rf_learning_metrics)

ggplot(rf_learning_metrics, aes(x = month, y = rmse)) + geom_line() + geom_point() + 
  labs(title = "Learning Rate of RF Model", x = "Month", y = "RMSE") +
  theme_minimal()

--------------------------------------------------------------------------------------------------
# Evaluate on test set

test_data <- test_data[complete.cases(test_data$avg_dwell), ] # remove missing values

dummy <- model.matrix(~ day - 1, data = test_data)
test_data <- cbind(test_data, dummy)

response_test = test_data$avg_dwell

# Model 1a

predictions <- predict(lm_model1a, newdata = test_data)

rmse <- sqrt(mean((response_test - predictions)^2))
rmse # root mean squared error = 0.628 

residuals <- response_test - predictions

residuals_data <- data.frame(
  fitted_values = predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)
View(residuals_data)

  # Plot residuals vs. fitted values
ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

  # Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

  # Plot predicted versus actual
ggplot(residuals_data, aes(x = date)) +
  geom_point(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = actual_values), color = "blue") +
  geom_point(aes(y = actual_values), color = "blue") +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  theme_minimal()


# Model 1b

predictions <- predict(lm_model1b, newdata = test_data)
View(predictions)

rmse <- sqrt(mean((response_test - predictions)^2))
rmse # root mean squared error = 0.579

residuals <- response_test - predictions

residuals_data <- data.frame(
  fitted_values = predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)
View(residuals_data)

  # Plot residuals vs. fitted values
ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

  # Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

  # Plot predicted versus actual
ggplot(residuals_data, aes(x = date)) +
  geom_point(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = actual_values), color = "blue") +
  geom_point(aes(y = actual_values), color = "blue") +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  theme_minimal()

  # Residual Analysis

high_residuals <- residuals_data %>%
  filter(abs(residuals) >= 1) 

View(high_residuals)

high_res_dates <- high_residuals$date

subset_avg_dwell <- avg_dwell %>%
  filter(DISCHARGE_DATE %in% high_res_dates) %>%
  select(DISCHARGE_DATE, day, IMPORT_DWELL, IMPORT_CNT)

View(subset_avg_dwell)

# Model 1c

predictions <- predict(lm_model1c, newdata = test_data)

rmse <- sqrt(mean((response_test - predictions)^2))
rmse # root mean squared error = 1.34 which is slightly higher than standard deviation,

residuals <- response_test - predictions

residuals_data <- data.frame(
  fitted_values = predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)
View(residuals_data)

  # Plot residuals vs. fitted values
ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

  # Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

  # Plot predicted versus actual
ggplot(residuals_data, aes(x = date)) +
  geom_point(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = actual_values), color = "blue") +
  geom_point(aes(y = actual_values), color = "blue") +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  theme_minimal()

# Model 2a

rf_predictions <- predict(rf_model, newdata = test_data)

rmse <- sqrt(mean((test_data$avg_dwell - rf_predictions)^2))
rmse

rsquared <- cor(test_data$avg_dwell, rf_predictions)^2
rsquared

residuals <- response_test - rf_predictions

residuals_data <- data.frame(
  fitted_values = rf_predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)
View(residuals_data)

  # Plot residuals vs. fitted values
ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

  # Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

  # Plot predicted versus actual
ggplot(residuals_data, aes(x = date)) +
  geom_point(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = actual_values), color = "blue") +
  geom_point(aes(y = actual_values), color = "blue") +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  theme_minimal()

  # Calculate variances
sd(rf_predictions)^2
sd(response_test)^2

  # Residual Analysis

high_residuals <- residuals_data %>%
  filter(abs(residuals) >= 1) 

View(high_residuals)

high_res_dates <- high_residuals$date

subset_SAV_merged <- SAV_merged %>%
  filter(date %in% high_res_dates) %>%
  select(date, day_of_week, month, slot_utilization, throughput_rate, avg_dwell)

View(subset_SAV_merged)

-----------------------------------------------------------------------------------------------------
# Train on 2023 Data, Test on 2024 Data 

full_model_data <- full_model_data[complete.cases(full_model_data$avg_dwell), ] # remove missing values

full_model_data <- lagged_data %>% mutate(day = weekdays(date), year = year(date))

train_data <- full_model_data %>%
  filter(year == 2023)

test_data <- anti_join(full_model_data, train_data, by = "date")

nrow(train_data)
nrow(test_data)
nrow(full_model_data)

dummy <- model.matrix(~ day - 1, data = train_data)
train_data <- cbind(train_data, dummy)

dummy <- model.matrix(~ day - 1, data = test_data)
test_data <- cbind(test_data, dummy)

# Model 1b

  # train MLR model
train_subset1b <- train_data[, c("avg_dwell", "dayMonday", "dayTuesday", "dayWednesday", "dayThursday", "dayFriday", "daySaturday",
                                "lag_1", "lag_2", "lag_6", "lag_7", "lag_8")] 

lm_model1b <- lm(avg_dwell ~ ., data = train_subset1b)

summary(lm_model1b)

test_data <- test_data[complete.cases(test_data$avg_dwell), ]

dummy <- model.matrix(~ day - 1, data = test_data)
test_data <- cbind(test_data, dummy)

predictions <- predict(lm_model1b, newdata = test_data)
View(predictions)

response_test <- test_data$avg_dwell

rmse <- sqrt(mean((response_test - predictions)^2))
rmse # root mean squared error = 0.633 which is pretty good!

residuals <- response_test - predictions

residuals_data <- data.frame(
  fitted_values = predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)
View(residuals_data)

  # Plot residuals vs. fitted values
ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

  # Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

  # Plot predicted versus actual
ggplot(residuals_data, aes(x = date)) +
  geom_point(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = actual_values), color = "blue") +
  geom_point(aes(y = actual_values), color = "blue") +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  theme_minimal()

# Model 2a

  # train RF model

train_data <- train_data[!is.na(train_data$avg_dwell),]
response_test = test_data$avg_dwell

rf_model <- randomForest(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                           lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9 + lag_10 +
                           lag_11 + lag_12, data = train_data, importance = TRUE)

  # evaluate on 2024 data
rf_predictions <- predict(rf_model, newdata = test_data)

rmse <- sqrt(mean((train_data$avg_dwell - training_predictions)^2))
rmse

residuals <- response_test - rf_predictions

residuals_data <- data.frame(
  fitted_values = rf_predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)
View(residuals_data)

  # Plot residuals vs. fitted values
ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

  # Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

  # Plot predicted versus actual
ggplot(residuals_data, aes(x = date)) +
  geom_point(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = actual_values), color = "blue") +
  geom_point(aes(y = actual_values), color = "blue") +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  theme_minimal()

---------------------------------------------------------------------------------------------------

# Model 1d: Weighted Least Squares Regression
  
  ## homoskedasticity test using Breusch-Pagan test
  bptest(lm_model1b)

  ## Calculate residuals and identify the variance of residuals for January
jan_residuals <- residuals_data %>%
  mutate(month = month(date)) %>%
  filter(month == 1)

View(jan_residuals)

january_variance <- var(jan_residuals$residuals)
january_variance

  ## Compute weights for WLSR
full_model_data <- full_model_data %>%
  mutate(month = month(date))

full_model_data$weights <- ifelse(full_model_data$month == 1, 1 / (january_variance*10)^2, 1)

## perform weighted least squares regression
train_data <- full_model_data %>%
  filter(year == 2023)

dummy <- model.matrix(~ day - 1, data = train_data)
train_data <- cbind(train_data, dummy)

wls_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                            lag_1 + lag_2 + lag_6 + lag_7 + lag_8, data = train_data, weights=weights)

summary(wls_model)

## evaluate on test set
test_data <- anti_join(full_model_data, train_data, by = "date")
test_data <- test_data[complete.cases(test_data$avg_dwell), ]

dummy <- model.matrix(~ day - 1, data = test_data)
test_data <- cbind(test_data, dummy)

predictions <- predict(wls_model, newdata = test_data)

response_test <- test_data$avg_dwell

rmse <- sqrt(mean((response_test - predictions)^2))
rmse # root mean squared error = 0.471 which is the same as before

residuals <- response_test - predictions

residuals_data <- data.frame(
  fitted_values = predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)

# Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

----------------------------------------------------------------------------
  # Model 1e: MLR with Month binary variable for January
  
full_model_data <- full_model_data %>%
  mutate(january = ifelse(month == 1, 1, 0))

sd(full_model_data$avg_dwell)

train_data <- full_model_data %>%
  mutate(year = year(date), day = weekdays(date)) %>%
  filter(year == 2023) 

dummy <- model.matrix(~ day - 1, data = train_data)
train_data <- cbind(train_data, dummy)

## train model on 2023 data 
lm_model1e <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                  lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = train_data)

summary(lm_model1e)

## Cross validate model

cv_metrics <- data.frame(month = character(), rmse = numeric(), stringsAsFactors = FALSE)

for (i in months) {
  
  train_subdata <- train_data %>% filter(month != i)
  validation_subdata <- train_data %>% filter(month == i)
  
  # Fit the model on training data
  dummy <- model.matrix(~ day - 1, data = train_subdata)
  train_subdata <- cbind(train_subdata, dummy)
  
  lm_model1e <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                     lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = train_subdata)
  
  # Predict on validation data
  validation_subdata <- validation_subdata[complete.cases(validation_subdata$avg_dwell), ]
  
  dummy <- model.matrix(~ day - 1, data = validation_subdata)
  validation_subdata <- cbind(validation_subdata, dummy)
  
  predictions <- predict(lm_model1e, newdata = validation_subdata)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((validation_subdata$avg_dwell - predictions)^2))
  
  # Store the performance metric
  cv_metrics <- rbind(cv_metrics, data.frame(month = i, rmse = rmse))
}

View(cv_metrics)

mean(cv_metrics$rmse) # average error = 0.47

## Assess Learning Rate of Model

learning_metrics <- data.frame(month = character(), rmse = numeric(), stringsAsFactors = FALSE)

for (i in 1:11) {
  
  training_set <- train_data %>% filter(month %in% months[1:i])
  validation_set <- train_data %>% filter(month == i + 1)
  
  # Fit the model on training data
  dummy <- model.matrix(~ day - 1, data = training_set)
  training_set <- cbind(training_set, dummy)
  
  lm_model1e <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                     lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = training_set)
  
  
  # Predict on validation data
  validation_set <- validation_set[complete.cases(validation_set$avg_dwell), ]
  dummy <- model.matrix(~ day - 1, data = validation_set)
  validation_set <- cbind(validation_set, dummy)
  
  predictions <- predict(lm_model1e, newdata = validation_set)
  
  # Calculate performance metric (e.g., RMSE)
  rmse <- sqrt(mean((validation_set$avg_dwell - predictions)^2))
  
  # Store the performance metric
  learning_metrics <- rbind(learning_metrics, data.frame(month = i, rmse = rmse))
}

View(learning_metrics)

ggplot(learning_metrics, aes(x = month, y = rmse)) + geom_line() + geom_point() + 
  labs(title = "Learning Rate of Model", x = "Month", y = "RMSE") +
  theme_minimal()


## evaluate on test set
test_data <- anti_join(full_model_data, train_data, by = "date")
test_data <- test_data[complete.cases(test_data$avg_dwell), ]

dummy <- model.matrix(~ day - 1, data = test_data)
test_data <- cbind(test_data, dummy)

predictions <- predict(lm_model1e, newdata = test_data)

response_test <- test_data$avg_dwell

rmse <- sqrt(mean((response_test - predictions)^2))
rmse # root mean squared error = 0.411 which is a small but significant improvement

residuals <- response_test - predictions

residuals_data <- data.frame(
  fitted_values = predictions,
  residuals = residuals,
  actual_values = response_test,
  date = test_data$date
)

# Plot residuals vs. fitted values
ggplot(residuals_data, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Plot residuals over time
ggplot(residuals_data, aes(x = date, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Date",
       y = "Residuals") +
  theme_minimal()

# Plot predicted versus actual
ggplot(residuals_data, aes(x = date)) +
  geom_point(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = fitted_values), color = "red") +
  geom_line(aes(y = actual_values), color = "blue") +
  geom_point(aes(y = actual_values), color = "blue") +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  theme_minimal()

------------------------------------------------------------------------------
# Use FLOW forward looking Data to Predict Average Dwell Times

## Set up test data
test_data <- lagged_test_data %>% mutate(month = month(date), day = weekdays(date)) %>%
  mutate(january = ifelse(month == 1, 1, 0))
dummy <- model.matrix(~ day - 1, data = test_data)
test_data <- cbind(test_data, dummy)
response_test <- test_data$avg_dwell

## prepare training data

### week 0
week0_data <- lagged_data0 %>% mutate(month = month(date))
week0_data <- week0_data[complete.cases(week0_data$avg_dwell), ] # remove missing values
week0_data <- week0_data %>%
  mutate(january = ifelse(month == 1, 1, 0)) %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
dummy <- model.matrix(~ day - 1, data = week0_data)
week0_data <- cbind(week0_data, dummy)

### week 1
week1_data <- lagged_data1 %>% mutate(month = month(date))
week1_data <- week1_data[complete.cases(week1_data$avg_dwell), ] # remove missing values
week1_data <- week1_data %>%
  mutate(january = ifelse(month == 1, 1, 0)) %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
dummy <- model.matrix(~ day - 1, data = week1_data)
week1_data <- cbind(week1_data, dummy)

### week 2
week2_data <- lagged_data2 %>% mutate(month = month(date))
week2_data <- week2_data[complete.cases(week2_data$avg_dwell), ] # remove missing values
week2_data <- week2_data %>%
  mutate(january = ifelse(month == 1, 1, 0)) %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
dummy <- model.matrix(~ day - 1, data = week2_data)
week2_data <- cbind(week2_data, dummy)

### week 3
week3_data <- lagged_data3 %>% mutate(month = month(date))
week3_data <- week3_data[complete.cases(week3_data$avg_dwell), ] # remove missing values
week3_data <- week3_data %>%
  mutate(january = ifelse(month == 1, 1, 0)) %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
dummy <- model.matrix(~ day - 1, data = week3_data)
week3_data <- cbind(week3_data, dummy)

### week 4
week4_data <- lagged_data4 %>% mutate(month = month(date))
week4_data <- week4_data[complete.cases(week4_data$avg_dwell), ] # remove missing values
week4_data <- week4_data %>%
  mutate(january = ifelse(month == 1, 1, 0)) %>%
  mutate(day = weekdays(date)) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
dummy <- model.matrix(~ day - 1, data = week4_data)
week4_data <- cbind(week4_data, dummy)

# train model on data

## week 0
week0_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                     lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = week0_data)
summary(week0_model)

## week 1
week1_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                    lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = week1_data)
summary(week1_model)

## week 2
week2_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                    lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = week2_data)
summary(week2_model)

## week 3
week3_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                    lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = week3_data)
summary(week3_model)

## week 4
week4_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                    lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = week4_data)
summary(week4_model)

# evaluate on test data

## week 0
week0_predictions <- predict(week0_model, newdata = test_data)

week0_rmse <- sqrt(mean((response_test - week0_predictions)^2))

week0_residuals <- response_test - week0_predictions

## week 1
week1_predictions <- predict(week1_model, newdata = test_data)

week1_rmse <- sqrt(mean((response_test - week1_predictions)^2))

week1_residuals <- response_test - week1_predictions

## week 2
week2_predictions <- predict(week2_model, newdata = test_data)

week2_rmse <- sqrt(mean((response_test - week2_predictions)^2))

week2_residuals <- response_test - week2_predictions

## week 3
week3_predictions <- predict(week3_model, newdata = test_data)

week3_rmse <- sqrt(mean((response_test - week3_predictions)^2))

week3_residuals <- response_test - week3_predictions

## week 4
week4_predictions <- predict(week4_model, newdata = test_data)

week4_rmse <- sqrt(mean((response_test - week4_predictions)^2))

week4_residuals <- response_test - week4_predictions

## combine results

results <- data.frame(
  week0_predictions = week0_predictions,
  week1_predictions = week1_predictions,
  week2_predictions = week2_predictions,
  week3_predictions = week3_predictions,
  week4_predictions = week4_predictions,
  actual_values = response_test,
  date = test_data$date
)

results_long <- results %>%
  pivot_longer(cols = starts_with("week"),
               names_to = "week",
               values_to = "predictions")

## Plot predicted versus actual
ggplot(results_long, aes(x = date)) +
  geom_point(aes(y = predictions, color = week), size = 2) +
  geom_line(aes(y = predictions, color = week), size = 1) +
  geom_line(data = results, aes(y = actual_values, color = "Actual Values"), size = 1) +
  geom_point(data = results, aes(y = actual_values, color = "Actual Values"), size = 2) +
  labs(title = "Fitted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  scale_color_manual(values = c(
    "week0_predictions" = "red",
    "week1_predictions" = "green",
    "week2_predictions" = "blue",
    "week3_predictions" = "purple",
    "week4_predictions" = "orange",
    "Actual Values" = "black"
  )) +
  theme_minimal()
  
--------------------------------------------------------------------------------------------------
# Predicting for all weeks from January 1st to May 1st, 2024

# 1 Week in Advance

results <- tibble(
    week = integer(),
    rmse = numeric(),
    training_observations = integer()
  )

all_predictions <- tibble(
  date = as.Date(character()),
  week = integer(),
  prediction = numeric()
)

initial_ref_date <- as.Date("2023-12-24")
initial_predict_date <- as.Date("2023-12-31")
initial_end_date <- as.Date("2024-01-06")

for (i in 1:17) {
  # Update dates
  ref_date <- initial_ref_date + weeks(i - 1)
  predict_date <- initial_predict_date + weeks(i - 1)
  end_date <- initial_end_date + weeks(i - 1)
  
  # Read the CSV file for the current week
  # This file contains confidential information and is not shared.
  week_data <- read_csv(paste0("1weekahead_2024_predictions/week", i, ".csv"))
  
  # Process data
  week_data$date <- as.Date(week_data$DISCHARGE_DATE, format = "%d-%b-%y")
  
  week_data <- week_data %>%
    group_by(date) %>%
    summarise(inbound_TEUs = sum(TEUS, na.rm = TRUE)) %>%
    select(date, inbound_TEUs) %>%
    left_join(., GPA_avg_dwell %>% select(date, avg_dwell), by = "date") %>%
    mutate(lag_1 = lag(inbound_TEUs)) %>%
    mutate(lag_2 = lag(lag_1)) %>%
    mutate(lag_3 = lag(lag_2)) %>%
    mutate(lag_4 = lag(lag_3)) %>%
    mutate(lag_5 = lag(lag_4)) %>%
    mutate(lag_6 = lag(lag_5)) %>%
    mutate(lag_7 = lag(lag_6)) %>%
    mutate(lag_8 = lag(lag_7)) %>%
    mutate(lag_9 = lag(lag_8)) %>%
    mutate(lag_10 = lag(lag_9)) %>%
    mutate(lag_11 = lag(lag_10)) %>%
    mutate(lag_12 = lag(lag_11)) %>%
    rename(lag_0 = inbound_TEUs)  %>%
    select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)
  
  week_test <- week_data %>%
    filter(date >= as.Date(predict_date) & date <= as.Date(end_date)) 
  
  week_train <- week_data %>%
    filter(date >= as.Date("2023-01-01") & date <= as.Date(ref_date)) 
  
  # Clean training data
  mean_avg_dwell = mean(week_train$avg_dwell, na.rm = TRUE)
  sd_avg_dwell = sd(week_train$avg_dwell, na.rm = TRUE)
  
  week_train_clean <- week_train %>%
    mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell) %>%
    filter(abs(zscores_dwell) < 2.5) %>%
    filter(complete.cases(.))
  
  training_observations <- nrow(week_train_clean) 
  
  # Train model
  
  week_train_data <- week_train_clean %>% mutate(month = month(date)) %>% drop_na() %>%
    mutate(january = ifelse(month == 1, 1, 0)) %>%
    mutate(day = weekdays(date)) %>%
    mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
  
  dummy <- model.matrix(~ day - 1, data = week_train_data)
  week_train_data <- cbind(week_train_data, dummy)
  
  week_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                     lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = week_train_data)
  
  # Test Model
  
  test_data <- week_test %>% mutate(month = month(date), day = weekdays(date)) %>%
    mutate(january = ifelse(month == 1, 1, 0))
  
  dummy <- model.matrix(~ day - 1, data = test_data)
  test_data <- cbind(test_data, dummy)
  response_test <- test_data$avg_dwell
  response_test_filtered <- response_test[!is.na(test_data$avg_dwell)]
  
  predictions <- predict(week_model, newdata = test_data)
  predictions_filtered <- predictions[!is.na(test_data$avg_dwell)]
  
  rmse <- sqrt(mean((response_test_filtered - predictions_filtered)^2))
  
  # Store Results
  results <- results %>%
    add_row(week = i, rmse = rmse, training_observations = training_observations)
  
  # Store predictions for week
  predictions_df <- tibble(
    date = test_data$date,
    week = i,
    prediction = predictions
  )
  
  all_predictions <- bind_rows(all_predictions, predictions_df)
  
}

all_predictions <- all_predictions %>% rename(week1_predictions = prediction)

combined_data <- merge(all_predictions, GPA_avg_dwell, by = "date")
combined_data <- combined_data %>%
  mutate(diff = avg_dwell - week1_predictions)

sum(combined_data$diff > 0) #54 underestimates = 45.8%
sum(combined_data$diff < 0) # 64 overestimates = 54.2%

# 4 Weeks in Advance

results <- tibble(
    week = integer(),
    rmse = numeric(),
    training_observations = integer()
  )

all_predictions <- tibble(
  date = as.Date(character()),
  week = integer(),
  prediction = numeric()
)
  
initial_ref_date <- as.Date("2023-12-03")
initial_predict_date <- as.Date("2023-12-31")
initial_end_date <- as.Date("2024-01-06")

for (i in 1:17) {
  # Update dates
  ref_date <- initial_ref_date + weeks(i - 1)
  predict_date <- initial_predict_date + weeks(i - 1)
  end_date <- initial_end_date + weeks(i - 1)
  
  # Read the CSV file for the current week
  # This file contains confidential data and is not shared.
  week_data <- read_csv(paste0("4weeksahead_2024predictions/week", i, ".csv"))
  
  # Process data
  week_data$date <- as.Date(week_data$DISCHARGE_DATE, format = "%d-%b-%y")

  week_data <- week_data %>%
    group_by(date) %>%
    summarise(inbound_TEUs = sum(TEUS, na.rm = TRUE)) %>%
    select(date, inbound_TEUs) %>%
    left_join(., GPA_avg_dwell %>% select(date, avg_dwell), by = "date") %>%
    mutate(lag_1 = lag(inbound_TEUs)) %>%
    mutate(lag_2 = lag(lag_1)) %>%
    mutate(lag_3 = lag(lag_2)) %>%
    mutate(lag_4 = lag(lag_3)) %>%
    mutate(lag_5 = lag(lag_4)) %>%
    mutate(lag_6 = lag(lag_5)) %>%
    mutate(lag_7 = lag(lag_6)) %>%
    mutate(lag_8 = lag(lag_7)) %>%
    mutate(lag_9 = lag(lag_8)) %>%
    mutate(lag_10 = lag(lag_9)) %>%
    mutate(lag_11 = lag(lag_10)) %>%
    mutate(lag_12 = lag(lag_11)) %>%
    rename(lag_0 = inbound_TEUs)  %>%
    select(date, avg_dwell, lag_0, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10, lag_11, lag_12)

  week_test <- week_data %>%
    filter(date >= as.Date(predict_date) & date <= as.Date(end_date)) 
  
  week_train <- week_data %>%
    filter(date >= as.Date("2023-01-01") & date <= as.Date(ref_date)) 

  # Clean training data
  mean_avg_dwell = mean(week_train$avg_dwell, na.rm = TRUE)
  sd_avg_dwell = sd(week_train$avg_dwell, na.rm = TRUE)
  
  week_train_clean <- week_train %>%
    mutate(zscores_dwell = (avg_dwell - mean_avg_dwell) / sd_avg_dwell) %>%
    filter(abs(zscores_dwell) < 2.5) %>%
    filter(complete.cases(.))

  training_observations <- nrow(week_train_clean) 
  
  # Train model

  week_train_data <- week_train_clean %>% mutate(month = month(date)) %>% drop_na() %>%
    mutate(january = ifelse(month == 1, 1, 0)) %>%
    mutate(day = weekdays(date)) %>%
    mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
  
  dummy <- model.matrix(~ day - 1, data = week_train_data)
  week_train_data <- cbind(week_train_data, dummy)
  
  week_model <- lm(avg_dwell ~ dayMonday + dayTuesday + dayWednesday + dayThursday + dayFriday + daySaturday +
                        lag_1 + lag_2 + lag_6 + lag_7 + lag_8 + january, data = week_train_data)
  
  # Test Model

  test_data <- week_test %>% mutate(month = month(date), day = weekdays(date)) %>%
    mutate(january = ifelse(month == 1, 1, 0))
  
  dummy <- model.matrix(~ day - 1, data = test_data)
  test_data <- cbind(test_data, dummy)
  response_test <- test_data$avg_dwell
  response_test_filtered <- response_test[!is.na(test_data$avg_dwell)]
  
  predictions <- predict(week_model, newdata = test_data)
  predictions_filtered <- predictions[!is.na(test_data$avg_dwell)]
  
  rmse <- sqrt(mean((response_test_filtered - predictions_filtered)^2))

  # Store Results
  results <- results %>%
    add_row(week = i, rmse = rmse, training_observations = training_observations)

  # Store predictions for week
  predictions_df <- tibble(
    date = test_data$date,
    week = i,
    prediction = predictions
  )
  
  all_predictions <- bind_rows(all_predictions, predictions_df)
    
}

all_predictions <- all_predictions %>%
  rename(week4_predictions = prediction)

actual_dwell <- GPA_avg_dwell %>%
  filter(date >= as.Date("2023-01-01") & date <= as.Date("2024-04-27"))

combined_data <- merge(all_predictions, actual_dwell, by = "date", all.x = TRUE)
combined_data <- combined_data %>%
  mutate(diff = avg_dwell - week4_predictions)

sum(combined_data$diff > 0, na.rm = TRUE) #53 underestimates = 44.9%
sum(combined_data$diff < 0, na.rm = TRUE) # 65 overestimates = 55%

# Plot predictions
ggplot(combined_data, aes(x=date)) +
  geom_line(aes(y = prediction, color = "Predicted"), size = 1) + 
  geom_point(aes(y = prediction, color = "Predicted"), size = 2) +
  geom_line(aes(y = avg_dwell, color = "Actual"), size = 1) +
  geom_point(aes(y = avg_dwell, color = "Actual"), size = 2) +
  labs(title = "Predicted vs Actual Values",
       x = "Date",
       y = "Average Daily Dwell Time") +
  scale_color_manual(values = c("Predicted" = "blue", "Actual" = "black")) +
  theme_minimal()

# Plot RMSE
  
  avg_rmse = mean(results$rmse)
  avg_rmse # average RMSE = 0.445
  
  ggplot(results, aes(x=week, y = rmse)) + 
    geom_line() + geom_point() +
    labs(title = "RMSE over each week",
         x = "Week",
         y = "RMSE") + theme_minimal()

  
# Plot 1 vs 4 week predictions on same graph
  
  ggplot(combined_data, aes(x=date)) +
    geom_line(aes(y = week4_prediction, color = "Predicted 4 Weeks in Advance"), size = 1) + 
    geom_point(aes(y = week4_prediction, color = "Predicted 4 Weeks in Advance"), size = 2) +
    geom_line(aes(y = week1_predictions, color = "Predicted 1 Week in Advance"), size = 1) + 
    geom_point(aes(y = week1_predictions, color = "Predicted 1 Week in Advance"), size = 2) +
    geom_line(aes(y = avg_dwell, color = "Actual"), size = 1) +
    geom_point(aes(y = avg_dwell, color = "Actual"), size = 2) +
    labs(title = "Predicted vs Actual Values",
         x = "Date",
         y = "Average Daily Dwell Time") +
    scale_color_manual(values = c("Predicted 4 Weeks in Advance" = "blue", 
                                  "Actual" = "black",
                                  "Predicted 1 Week in Advance" = "red")) +
    theme_minimal()

all_combined_predictions <- combined_data
