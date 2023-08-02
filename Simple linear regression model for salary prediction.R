library(datasets)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, caret, lars, tidyverse)

# CSV
salary_csv <- read.csv("/Users/myyntiimac/Desktop/Salary_Data.csv")
head(salary_csv)
#install packagesfor split
install.packages("caTools")
library(caTools)

#split
split <- sample.split(salary_csv$Salary, SplitRatio = 2/3)
train_data <- subset(salary_csv, split == TRUE)
test_data <- subset(salary_csv, split == FALSE)
#Train the model
# Perform linear regression
model <- lm(Salary ~ YearsExperience, data = train_data)
#prediction
y_pred=predict(model,newdata=test_data )

#summary
summary(model)

#visualize
ggplot() +
  geom_point(data = train_data, aes(YearsExperience, Salary), color = "blue") +
  geom_point(data = test_data, aes(YearsExperience, Salary), color = "green") +
  geom_line(data = train_data, aes(YearsExperience, predict(model)), color = "red") +
  labs(title = "Simple Linear Regression: Training and Test Set",
       x = "X",
       y = "Y") +
  theme_minimal()








