library(statsr)
library(dplyr)
library(ggplot2)
data(nc)
nc
str(nc)
summary(nc$gained)

ggplot(data=nc, mapping = aes(x=habit,y=weight))+
  geom_boxplot()

inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical")

inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical", order = c("smoker","nonsmoker"))

predict(object = nc, newdata = newd2, interval = 'confidence', level=0.99)
