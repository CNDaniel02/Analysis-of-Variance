#load library
library(ggplot2)
library(dplyr)

#load data
data <- read.csv("E:/Study/STA106/project1/sparrow.csv") #load data

#histogram
ggplot(data, aes(x = Weight, fill = Treatment)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = 'identity') +
  facet_wrap(~Treatment) +
  labs(title = "Histogram of Sparrow Weights by Nest Treatment",
       x = "Weight (grams)", y = "Count") +
  theme_minimal()

#boxplot
ggplot(data, aes(x = Treatment, y = Weight, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Boxplot of Sparrow Weights by Nest Treatment",
       x = "Nest Treatment", y = "Weight (grams)") +
  theme_minimal()

#summary table
summary <- data %>%
  group_by(Treatment) %>%
  summarise(
    Mean = mean(Weight),
    SD = sd(Weight),
    SampleSize = n()
  )
print(summary_table)

#overall summary table
overallSummary <- data %>%
  summarise(
    Mean = mean(Weight),
    SD = sd(Weight),
    sampleSize = n()
  )
print(overallSummary)

# fit ANOVA model
model <- aov(Weight ~ Treatment, data = data)

# qq plot for residuals
qqnorm(residuals(model))
qqline(residuals(model))

#shapiro-Wilk test
shapiro.test(residuals(model))


#identify outliers
outliers <- boxplot.stats(data$Weight)$out

# filter out outliers
dataNoOutliers <- data[!(data$Weight %in% outliers),]

# model no outliers
modelNoOutliers <- aov(Weight ~ Treatment, data = dataNoOutliers)

#qq plot no outliers
qqnorm(residuals(modelNoOutliers))
qqline(residuals(modelNoOutliers))
shapiro.test(residuals(modelNoOutliers))


install.packages("car")
library(car)

str(data)
str(dataNoOutliers)

# levenetest
leveneTest(Weight ~ Treatment, data = dataNoOutliers, center = median)


modelNoOutliers <- aov(Weight ~ Treatment, data = dataNoOutliers)


# residuals vs fitted
plot(fitted(modelNoOutliers), residuals(modelNoOutliers),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values (No Outliers)")
abline(h = 0, col = "red")





