# Install these packages if not already installed
install.packages(c("ggplot2", "dplyr"))
install.packages("tidyr")
install.packages("curl")
install.packages("xml2")
install.packages("systemfonts")





# Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Load the data
data <- read_csv("Desktop/Postpartum_Women_Participating.csv")

# Check the column names to ensure correct column selection
colnames(data)
head(data,2)


# Convert the data to a long format
data_long <- data %>%
  pivot_longer(
    cols = starts_with("2012") | starts_with("2013"),  # Select columns that start with the date format
    names_to = "Month", 
    values_to = "Avg_Fund_Allocation"
  ) %>%
  # Convert the 'Month' column from string to date format
  mutate(Month = as.Date(Month, format = "%Y-%m-%d %H:%M:%S")) %>%
  # Calculate the difference in days from the reference date
  mutate(Month = as.numeric(Month - as.Date("2012-10-01"))) %>%
  mutate(Month = (Month / 30) + 1)  # Convert days to months, starting from 1 for October 2012

# Create scatter plot with a linear trendline
ggplot(data_long, aes(x = Month, y = Avg_Fund_Allocation)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Trend of Average Monthly Fund Allocation Across States (Oct 2012 - Sep 2013)",
    x = "Months (1 = Oct 2012, 12 = Sep 2013)",
    y = "Average Fund Allocation (currency units)"
  ) +
  theme_minimal()







# Calculate the mean and standard deviation for the normal curve
mean_fund_allocation <- mean(data_long$Avg_Fund_Allocation, na.rm = TRUE)
sd_fund_allocation <- sd(data_long$Avg_Fund_Allocation, na.rm = TRUE)

# Plot the histogram with normal curve overlay
ggplot(data_long, aes(x = Avg_Fund_Allocation)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", bins = 10) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean_fund_allocation, sd = sd_fund_allocation),
    color = "red",
    lwd = 1.2
  ) +
  labs(
    title = "Distribution of Average Monthly Fund Allocation Across States (Oct 2012 - Sep 2013)",
    x = "Average Fund Allocation (currency units)",
    y = "Density"
  ) +
  theme_minimal()

