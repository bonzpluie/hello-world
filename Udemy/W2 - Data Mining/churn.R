setwd("C:/Users/Pluie/Desktop/Tableau/W2 - Data Mining")
library(readxl)
churn = read_excel(list.files(pattern = ".xlsx"))
library(tidyverse)

chisq.test(churn$Exited, churn$HasCrCard)
class(churn)

# data validation
# run a chi-square test for Gender, Country, HasCrCard, IsActive, NoOfProducts, Age
gendertable = table(churn$Exited, churn$Gender)
chisq.test(gendertable)

# Stacked bar
barplot(hi, col = c("red", "blue"), legend = rownames(hi), 
        main = "Churn Distribution by Gender")
  
# Grouped Bar
barplot(hi, col = c("red", "blue"), legend = rownames(hi), 
        main = "Churn Distribution by Gender", beside = TRUE)

# country
countrytable = table(churn$Exited, churn$Geography)
chisq.test(countrytable)

# HasCrCard
HasCrCardtable = table(churn$Exited, churn$HasCrCard)
chisq.test(HasCrCardtable) #Not sig

# IsActive
IsActivetable = table(churn$Exited, churn$IsActiveMember)
chisq.test(IsActivetable)


# NoOfProducts
NoOfProductstable = table(churn$Exited, churn$NumOfProducts)
chisq.test(NoOfProductstable)

# Age
agetable = table(churn$Exited, cut(churn$Age, breaks = c(0, 20, 40, 60, 80, 100)))
chisq.test(agetable)

