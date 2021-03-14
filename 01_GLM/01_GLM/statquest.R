rm(list = objects())
library(cowplot)
library(ggplot2)

data <- na.omit(subset(airquality, select = c(Ozone, Solar.R, Wind, Temp)))
data$Ozone <- data$Ozone > mean(data$Ozone)
head(data, n = 3)

sum(is.na(data))

logreg <- glm(Ozone~Wind, data = data, family = "binomial")
summary(logreg)

logreg <- glm(Ozone~., data = data, family = "binomial")
summary(logreg)

# overall effect 
ll_null <- logreg$null.deviance/-2
ll_proposed <- logreg$deviance/-2

(ll_null-ll_proposed)/ll_null
1-pchisq(2*(ll_proposed-ll_null), df = (length(logreg$coefficients)-1))


#visualize
prediction <- data.frame(prob_Ozone = logreg$fitted.values, Ozone = data$Ozone)
prediction <- prediction[order(prediction$prob_Ozone, decreasing = F),]
prediction$rank <- 1:nrow(prediction)

ggplot(data = prediction, aes(x = rank, y = prob_Ozone))+
  geom_point(aes(color = Ozone), alpha = 1, shape = 4, stroke = 2)+
  xlab("Index")+
  ylab("Predicted probability of Ozone above Mean")



