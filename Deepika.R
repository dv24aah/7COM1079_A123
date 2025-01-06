# Loading required libraries
library(tidyverse)   # For data manipulation and visualization
library(lubridate)   # For handling dates
library(ggplot2)     # For visualizations
library(corrplot)    # For correlation matrix visualization

# Loading the dataset
data <- read_csv("transportation-fuels-inventory-beginning-2004-1.csv")

# Cleaning column names to make them syntactically valid
data <- data %>% 
  rename_with(~make.names(.), everything())

# Converting the Date column to Date type
data <- data %>%
  mutate(Date = mdy(Date)) %>%  # Converting Date to proper date format
  arrange(Date)  # Arranging rows by Date

# Checking missing values in the data
sum(is.na(data))

# removing missing values from the data
data <- data %>% drop_na()

# Printing the first few rows of the cleaned data
head(data)

# Checking the structure of the data
str(data)

# Printing the summary statistics of the data
summary(data)

# Printing the column names of the data
colnames(data)

# Selecting fuel stocks columns
FuelData <- data %>% select(Date, `East.Coast.Ethanol.Stocks...Thousand.Barrels.`, `U.S..Gasoline.Stocks..Thousand.Barrels.`)

# Plotting a Histogram for the U.S. Gasoline Stocks with Bell Curve
ggplot(FuelData, aes(x = `U.S..Gasoline.Stocks..Thousand.Barrels.`)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(FuelData$`U.S..Gasoline.Stocks..Thousand.Barrels.`), 
                                         sd = sd(FuelData$`U.S..Gasoline.Stocks..Thousand.Barrels.`)), color = "black", size = 1) +
  labs(title = "Histogram of U.S. Gasoline Stocks", 
       x = "U.S. Gasoline Stocks (Thousand Barrels)", 
       y = "Density") +
  theme_minimal()

# Plotting a Histogram for the East Coast Ethanol Stocks with Bell Curve
ggplot(FuelData, aes(x = `East.Coast.Ethanol.Stocks...Thousand.Barrels.`)) +
  geom_histogram(aes(y = ..density..), binwidth = 500, fill = "lightgreen", color = "black") +
  geom_density(color = "red", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(FuelData$`East.Coast.Ethanol.Stocks...Thousand.Barrels.`), 
                                         sd = sd(FuelData$`East.Coast.Ethanol.Stocks...Thousand.Barrels.`)), color = "black", size = 1) +
  labs(title = "Histogram of East Coast Ethanol Stocks", 
       x = "East Coast Ethanol Stocks (Thousand Barrels)", 
       y = "Density") +
  theme_minimal()

# Plotting a Scatter Plot of East Coast Ethanol Stocks vs U.S. Gasoline Stocks with Regression Line
ggplot(FuelData, aes(x = `East.Coast.Ethanol.Stocks...Thousand.Barrels.`, 
                     y = `U.S..Gasoline.Stocks..Thousand.Barrels.`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of East Coast Ethanol Stocks vs U.S. Gasoline Stocks", 
       x = "East Coast Ethanol Stocks (Thousand Barrels)", 
       y = "U.S. Gasoline Stocks (Thousand Barrels)") +
  theme_minimal()
