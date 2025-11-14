#Data Import----
# Task 1: Data Import
# Import CSV file
data <- read.csv("openaq_location_1214597_measurments.csv")

# Structure of dataset
str(data)

# Summary statistics
summary(data)

# Number of rows and columns
dim(data)

# Data types of each column
sapply(data, class)

# Missing values per column
colSums(is.na(data))
#Data Cleaning----
# Task 2: Data Cleaning

library(dplyr)

# Remove duplicate records
data_clean <- distinct(data)

# Rename columns for clarity
colnames(data_clean) <- c("location_id","location_name","parameter","value","unit",
                          "datetime_utc","datetime_local","timezone","latitude","longitude",
                          "country_iso","isMobile","isMonitor","owner_name","provider")

# Convert data types
data_clean$value <- as.numeric(data_clean$value)
data_clean$datetime_utc <- as.POSIXct(data_clean$datetime_utc, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
data_clean$datetime_local <- as.POSIXct(data_clean$datetime_local, format="%Y-%m-%dT%H:%M:%S", tz="Asia/Tokyo")

# Handle missing values (remove rows with NA)
data_clean <- na.omit(data_clean)

# Detect outliers using IQR method
Q1 <- quantile(data_clean$value, 0.25)
Q3 <- quantile(data_clean$value, 0.75)
IQR <- Q3 - Q1

# Filter out outliers
data_clean <- data_clean %>%
  filter(value >= (Q1 - 1.5*IQR) & value <= (Q3 + 1.5*IQR))

#Exploratory Data Analysis----
# Task 3: EDA
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)

# a) Descriptive Statistics
data_stats <- data_clean %>%
  group_by(parameter) %>%
  summarise(mean = mean(value),
            median = median(value),
            variance = var(value),
            sd = sd(value))

print(data_stats)
colnames(data_clean) <- c("location_id","location_name","parameter","value","unit",
                          "datetime_utc","datetime_local","timezone","latitude","longitude",
                          "country_iso","isMobile","isMonitor","owner_name","provider")


# Correlation matrix among pollutants
pollutant_wide <- data_clean %>%
  select(parameter, value, datetime_utc) %>%
  spread(parameter, value)

cor_matrix <- cor(pollutant_wide[, -1], use="complete.obs")
ggcorrplot(cor_matrix, lab=TRUE)

# b) Visual Analysis

# Histogram for pollutant distributions
ggplot(data_clean, aes(x=value, fill=parameter)) +
  geom_histogram(bins=30, alpha=0.6, position="identity") +
  facet_wrap(~parameter, scales="free") +
  theme_minimal()

# Boxplots across pollutants
ggplot(data_clean, aes(x=parameter, y=value, fill=parameter)) +
  geom_boxplot() +
  theme_minimal()

# Time-series line plot
ggplot(data_clean, aes(x=datetime_local, y=value, color=parameter)) +
  geom_line() +
  theme_minimal()

# c) Interpretation (example insights)-----
# 1. NOx values show higher variation compared to SO2 (mostly zero).
# 2. PM2.5 concentrations fluctuate more during daytime hours.
# 3. Correlation shows NO2 and NOx are strongly related.
