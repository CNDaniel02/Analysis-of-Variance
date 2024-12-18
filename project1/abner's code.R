
```{r}

#load library
library(ggplot2)
library(dplyr)

#load data
data <- read.csv("~/STATS 108 R/sparrow.csv") 
Weight = sparrow$Weight
Treatment = sparrow$Treatment

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
summary_table <- data %>%
  group_by(Treatment) %>%
  summarise(
    Mean = mean(Weight),
    Std_Dev = sd(Weight),
    Sample_Size = n()
  )
print(summary_table)

#overall summary table
overall_summary <- data %>%
  summarise(
    Mean = mean(Weight),
    SD = sd(Weight),
    sampleSize = n()
  )
print(overall_summary)
```

```{r}
give.me.power = function(ybar,ni,MSE,alpha){
  a = length(ybar) # Finds a
  nt = sum(ni) #Finds the overall sample size
  overall.mean = sum(ni*ybar)/nt # Finds the overall mean
  phi = (1/sqrt(MSE))*sqrt( sum(ni*(ybar - overall.mean)^2)/a) 
  phi.star = a *phi^2 #Finds the value of phi we will use for R 
  Fc = qf(1-alpha,a-1,nt-a) #The critical value of F, use in R's function
  power = 1 - pf(Fc, a-1, nt-a, phi.star)# The power, calculated using a non-central F
  return(power)
}

group.means =  by(sparrow$Weight,sparrow$Treatment,mean)
group.nis = by(sparrow$Weight,sparrow$Treatment,length)
the.model = lm(Weight ~ Treatment, data = sparrow)
anova.table = anova(the.model)
MSE = anova.table[2,3]
```

```{r}
the.power = give.me.power(group.means,group.nis,MSE,0.05)
the.power
overall.mean = sum(group.means*group.nis)/sum(group.nis)
effect.size = sqrt( sum( group.nis/sum(group.nis) *(group.means -overall.mean)^2 )/MSE)
library(pwr)
pwr.anova.test(k = 3, f = effect.size, sig.level = 0.05, power = 0.95)
```
```{r}
give.me.CI = function(ybar,ni,ci,MSE,multiplier){
  if(sum(ci) != 0 & sum(ci !=0 ) != 1){
    return("Error - you did not input a valid contrast")
  } else if(length(ci) != length(ni)){
    return("Error - not enough contrasts given")
  }
  else{
    estimate = sum(ybar*ci)
    SE = sqrt(MSE*sum(ci^2/ni))
    CI = estimate + c(-1,1)*multiplier*SE
    result = c(estimate,CI)
    names(result) = c("Estimate","Lower Bound","Upper Bound")
    return(result)
  }
}

t.value = qt(1-0.05/2, sum(group.nis) - length(group.nis))
ci.1 = c(1,0,-1) 
ci.2 = c(1,-1,0) 
ci.3 = c(0,1,-1)

CI1 = give.me.CI(group.means,group.nis,ci.1,MSE,t.value)
CI2 = give.me.CI(group.means,group.nis,ci.2,MSE,t.value)
CI3 = give.me.CI(group.means,group.nis,ci.3,MSE,t.value)
CI1
CI2
CI3

gammai = group.means - mean(group.means)
```

```{r}
library(pwr)

k <- 3 #number of groups
n <- (45 + 45 + 26) / 3 # average number of group members
f <- sqrt((72.74 / (72.74 + 505.62))) # effect size (Cohen's f)
alpha <- 0.05 # significance level
power <- pwr.anova.test(k = k, n = n, f = f, sig.level = alpha)
print(power)
```
