library(ggplot2)
library(car)
library(dplyr)
library(MASS)

data <- read.csv("E:/Study/STA106/Project2/Helicopter.csv")

#boxplot
ggplot(data, aes(x = Shift, y = Count)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Helicopter Calls by Shift",
       x = "Shift",
       y = "Number of Calls") +
  theme_minimal()

anovaModel <- aov(Count ~ Shift, data = data)


#diagnostics
par(mfrow = c(2, 2)) 
plot(anovaModel, which = 1)
plot(anovaModel, which = 2)


#shapiro test
shapiOrigin <- shapiro.test(residuals(anovaModel))
print(shapiOrigin)

#brown test
brownOrigin <- leveneTest(Count ~ Shift, data = data, center = median)
print(brownOrigin)


#remove outliers
dataa <- boxplot(data$Count ~ data$Shift)
outliers <- dataa$out
datanoout <- data[!(data$Count %in% outliers), ]



#boxcox transformation and check 0
boxResult <- boxcox(lm(Count ~ Shift, data = data))
boxLamda <- boxResult$x[which.max(boxResult$y)]
print(boxLamda)

boxResultnoout <- boxcox(lm(Count ~ Shift, data = datanoout))
boxLamdanoout <- boxResultnoout$x[which.max(boxResultnoout$y)]
print(boxLamdanoout)

data$boxCount <- (data$Count^boxLamda - 1) / boxLamda
datanoout$boxCount <- (datanoout$Count^boxLamdanoout - 1) / boxLamdanoout



# Diag plots without outliers
anovanoout <- aov(Count ~ Shift, data = datanoout)
par(mfrow = c(2, 2)) 
plot(anovanoout, which = 1)
plot(anovanoout, which = 2)

# Shapiro test for no outliers
shapironoout <- shapiro.test(residuals(anovanoout))
print(shapironoout)

# Brown test for no out
brownnoout <- leveneTest(Count ~ Shift, data = datanoout, center = median)
print(brownnoout)

#diag plots for Box-Cox
anovaBox <- aov(boxCount ~ Shift, data = data)
par(mfrow = c(2, 2)) 
plot(anovaBox, which = 1)
plot(anovaBox, which = 2)

# Shapiro for boxcox
shapirobox <- shapiro.test(residuals(anovaBox))
print(shapirobox)

#brown for box
brownbox <- leveneTest(boxCount ~ Shift, data = data, center = median)
print(brownbox)


#diag on Box no out
anovaBoxnoout <- aov(boxCount ~ Shift, data = datanoout)
plot(anovaBoxnoout, which = 1)
plot(anovaBoxnoout, which = 2)

# Shapiro box no out
shapiroBoxnoout <- shapiro.test(residuals(anovaBoxnoout))
print(shapiroBoxnoout)

#brown box no out
brownBoxnoout <- leveneTest(boxCount ~ Shift, data = datanoout, center = median)
print(brownBoxnoout)




