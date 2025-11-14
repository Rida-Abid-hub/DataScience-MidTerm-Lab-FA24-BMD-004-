

## 📥 Task 1: Data Import -----


# 📥 Data Import----
library(readr)
data <- read_csv("openaq-02.csv")

# 🧠 Data Structure----
str(data)
summary(data)

# 🔢 Rows, Columns, Data Types----
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")
sapply(data, class)

# ❓ Missing Values per Column----
colSums(is.na(data))


---
  
  
 ---
  
  ## 🧹 Task 2: Data Cleaning ----


# 🧹 Data Cleaning----
library(dplyr)
library(lubridate)


# 1️⃣ Remove rows with -1 values (invalid pollutant readings)----
data_clean <- data %>% filter(value != -1)

 # 2️⃣ Remove duplicates----
data_clean <- data_clean %>% distinct()

 # 3️⃣ Rename columns for clarity----
data_clean <- data_clean %>% rename(
  LocationID = location_id,
  LocationName = location_name,
  Pollutant = parameter,
  Concentration = value,
  Unit = unit,
  UTC_Time = datetimeUtc,
  Local_Time = datetimeLocal,
  Country = country_iso,
  Mobile = isMobile,
  Monitor = isMonitor,
  Owner = owner_name,
  Provider = provider
)

# 4️⃣ Convert data types----
data_clean$UTC_Time <- ymd_hms(data_clean$UTC_Time)
data-clean$Local_Time <- ymd_hms(data_clean$Local_Time)
data_clean$Pollutant <- as.factor(data_clean$Pollutant)
data_clean$Country <- as.factor(data_clean$Country)

# 5️⃣ Handle outliers using IQR method----
Q1 <- quantile(data_clean$Concentration, 0.25)
Q3 <- quantile(data_clean$Concentration, 0.75)
IQR <- Q3 - Q1
data_clean <- data_clean %>% filter(Concentration >= (Q1 - 1.5 * IQR) & Concentration <= (Q3 + 1.5 * IQR))


---
  

---  
  
  ## 📊 Task 3: Exploratory Data Analysis ----


# 📊 EDA (Exploring Data)----
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(tidyverse)

# a) Descriptive Statistics----

(i) # Compute mean,median,mode,variance,SD for Concentration by Pollutant

data_clean %>% group_by(Pollutant) %>%
  summarise(
    Mean = mean(Concentration),
    Median = median(Concentration),
    Variance = var(Concentration),
    SD = sd(Concentration)
  )

(ii) # Correlation Matrix (only numeric)

cor_data <- data_clean %>% select(Concentration) %>% na.omit()
cor_matrix <- cor(cor_data)
ggcorrplot(cor_matrix, lab = TRUE)

# b) Visual Analysis----

library(ggplot2)

(i) # Histogram
ggplot(data_clean, aes(x = Concentration, fill = Pollutant)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~Pollutant, scales = "free") +
  labs(title = "Pollution Distribution", x = "Concentration (µg/m³)", y = "Frequency")

(ii)_a. # Boxplot by Pollutant
ggplot(data_clean, aes(x = Pollutant, y = Concentration, fill = Pollutant)) +
  geom_boxplot() +
  labs(title = "Pollutant Levels by Type", x = "Pollutant", y = "Concentration")

(ii)_b. # Boxplot by Country
ggplot(data_clean, aes(x = Country, y = Concentration, fill = Country)) +
  geom_boxplot() +
  labs(title = "Pollutant Levels by Country", x = "Country", y = "Concentration")


(iii) # Time-series Plot
ggplot(data_clean, aes(x = UTC_Time, y = Concentration, color = Pollutant)) +
  geom_line() +
  labs(title = "Pollution Trend Over Time", x = "Time", y = "Concentration")


(iv) # Heatmap of Average Pollutant Levels by Country
avg_pollution <- data_clean %>%
  group_by(Country, Pollutant) %>%
  summarise(Avg_Concentration = mean(Concentration)) %>%
  ungroup()
ggplot(avg_pollution, aes(x = Country, y = Pollutant, fill = Avg_Concentration)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Average Pollutant Levels by Country", x = "Country", y
       = "Pollutant")
---




# c) Insights (write in README)----
  # - CO levels peaked around 18:00 UTC.
  # - SO2 remained relatively stable throughout the day.
  # - NO2 showed sharp fluctuations indicating traffic or industrial activity.
  

---
