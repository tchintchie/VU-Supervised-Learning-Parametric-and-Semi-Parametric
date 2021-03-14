library(factoextra)
library(party)


data("USArrests")
data <- USArrests[, c("Murder", "Assault")]
data <- scale(na.omit(data))
cl <- kmeans(data, centers = 4, nstart = 25)



fviz_cluster(cl, data = data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#123456"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Conditional Tree
airq <- na.omit(airquality)
tree <- ctree(Ozone~., data = airq)
# Regression model
lmod <- glm(Ozone~., data = airq, family = "Gamma")

plot(airq$Ozone, airq$date, type = "l")
abline(lmod, col = "red")

plot(airq$Ozone, airq$date, type = "l")
abline(tree, col = "red")

#####
data("airquality")
data <- na.omit(airquality)
head(data, n = 3)

## estimate linear model
mod0 <- lm(Ozone~1, data = data)
mod1 <- lm(Ozone~Temp+Wind, data = data)
mod2 <- lm(Ozone~., data = data)

summary(mod1)
