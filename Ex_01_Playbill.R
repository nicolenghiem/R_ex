setwd("E:\00. Cao hoc\01. Mo hinh hoa thong ke\01. Data - code R\01. Linear")

#dev.off ()


# box office ticket sales for plays on Broadway in New York
# consider the data for the week October 11-17, 2004 (current week).
# previous week (i.e., October 3-10, 2004)
Playbill<-read.csv(file.choose(), header = TRUE, sep = ",")
attach(Playbill)
dim(Playbill)
Playbill[1:5,]
summary(Playbill)
#pairplot
plot(Playbill)

y <- Playbill$CurrentWeek
x <- Playbill$LastWeek
M1 <- lm(y ~ x)
out = summary(M1)
summary(M1)
# Xem cac params in M1
names(M1)
coef(M1)

plot(x,y,lab="Gross box office results previous week",
     ylab="Gross box office results current week")

#Add Straight Lines to a Plot
abline(M1,lwd=3,col="red")

# a) Find a 95% CI for the slope of the regression model, b1. 
# Is 1 a plausible value for b1? Reason to support your answer?
# A: Yes, because we are 95% sure that the value of b1 is somewhere  
# between 0.95 and 1.01
# CI - Confident Interval 95%
confint(M1)

# b) Test the null hypothesis H0: b0=10000 against a two-sided H1.
# Interpret your result
# alpha = 0.05
# df = 18 - 2 = 16
# H0: b0 = 10000
# H1: b0 != 10000
tval <- qt(1-0.05/2,16)
tval
b0_head <- coef(M1)["(Intercept)"]
#extract 2nd column, row 1 from the coefficients object
se_b0_head = out$coefficients[1, 2] 
t_val_for_b0_head <- (b0_head - 10000)/se_b0_head
t_val_for_b0_head > tval
# Ta ko the bac bo H0: b0 = 10000

# c) Use the fitted regression model to estimate the gross box office results for
# the current week (in $) for a production with $400,000 in gross box office
# the previous week. 

# Find a 95% prediction interval for the gross box office results 
# for the current week (in $) for a production with $400,000 in gross
# box office the previous week.
predict(M1,data.frame(x = 400000),interval="confidence",level=0.95)

# Is $450,000 a feasible value for the gross box
# office results in the current week, for a production with $400,000 in gross
# box office the previous week? Give a reason to support your answer.
# A: 450k is not a plausible prediction. We are 95% confidence that 
# the gross box office results for the current week (in $) for a production with $400,000 
# in gross box office the previous week is between 388361.9 and 410913

# d) Some promoters of Broadway plays use the prediction rule that next week's
# gross box office results will be equal to this week's gross box office results.
# Comment on the appropriateness of this rule
predict(M1,data.frame(x = 400000),interval="prediction",level=0.95)

# par(mfrow=c(2,2))
detach(Playbill)

