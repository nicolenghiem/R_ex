#install.packages("car")
library("car")
###data
accident <- read.table(file.choose(),sep = ",",header = TRUE)
dim(accident)
accident[1:5,]
y = accident$y_i
summary(accident)

###stepAIC : stepwise regression proceeded
mod <- lm(y_i ~ ., data = accident)# Full model
modAIC <- MASS::stepAIC(mod, k = 2, direction = "backward", trace = FALSE) # With AIC  k = 2
summary(modAIC)

modBIC <- MASS::stepAIC(mod, k = log(nrow(accident)), direction = "backward", trace = FALSE) 
## With BIC k = log(nrow(accident))
summary(modBIC)

anova(modBIC, modAIC)

shapiro.test(residuals(modBIC))

op <- par(mfrow=c(2,2))
plot(modBIC)
