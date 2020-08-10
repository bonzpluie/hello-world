setwd("C:/Users/Pluie/Desktop/Learn/hello-world/Tableau/W3 - Modelling")

### Simple Linear Regression

salarydata = read.csv(file = list.files(pattern = "SalaryData"))
head(salarydata)

lm1 = lm(Salary ~ YearsExperience, data = salarydata)
summary(lm1)
plot(Salary ~ YearsExperience, data = salarydata)
abline(lm1, col = 'red')

pred.int <- predict(lm1, interval = "prediction")

mydata = cbind(salarydata, pred.int)

library("ggplot2")

p <- ggplot(mydata, aes(YearsExperience, Salary)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

### Multiple Linear Regression
list.files(path = '.')
startupdata = read.csv(file = list.files(pattern = "Startups"))
head(startupdata)

### profit - dependent variable - response
lm2 = lm(Profit ~., data = startupdata)
summary(lm2)
summary(lm2)$adj.r.squared

## Backwards elimination, drop 'administration'
lm3 = lm(Profit ~ `R.D.Spend` + `Marketing.Spend` + `State`, data = startupdata)
summary(lm3)
summary(lm3)$adj.r.squared

# Drop State
lm4 = lm(Profit ~ `R.D.Spend` + `Marketing.Spend`, data = startupdata)
summary(lm4)
summary(lm4)$adj.r.squared


# Drop Marketing Spend
lm5 = lm(Profit ~ `R.D.Spend`, data = startupdata)
summary(lm5)
summary(lm5)$adj.r.squared

# Adjusted R squared decreased, so lm4 is better than lm5.

# Check the units of measurement!!! When interpreting, might make mistakes here
# to avoid that, use per unit of the underlying predicting variables

# Conclusion
# More spending on RDSpend, the more profit made. On averge, 
# an additional 1 dollar spend on RDSpend, the profit will increase 0.8 dollar.  


# Non-sig but insightful
# Compared to startups in California, startups in NY makes more profit. 
# The more admin fees spend, the less the profit made. 
 
## Binary outcome!!! Yes or NO, should we loan the money to this customer? Yes? No?


#### Logistic Regression
emaildata = read.csv(list.files(pattern = "Email"))

## The response, dependent variable is `TookAction`
?glm
?family
lm6 = glm(TookAction ~ ., data = emaildata, family = binomial(link = "logit"))
summary(lm6)


