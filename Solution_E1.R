setwd("#Input_your_environment_path")

# Box office ticket sales for plays on Broadway in New York
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

plot(x,y,xlab="Gross box office results previous week",
     ylab="Gross box office results current week")

#Add Straight Lines to a Plot
abline(M1,lwd=3,col="red")

# a) Find a 95% CI for the slope of the regression model, b1. 
# Is 1 a plausible value for b1? Reason to support your answer?
confint(M1)
#                 2.5 %        97.5 %
# (Intercept) -1.424433e+04    27854.099443
# x            9.514971e-01    1.012666

# b) Test the null hypothesis H0: b0=10000 against a two-sided H1.
# Interpret your result
# alpha = 0.05
# df = 18 - 2 = 16
# H0: b0 = 10000
# H1: b0 != 10000
tval <- qt(1-0.05/2,16)
tval
# 2.119905

b0_head <- coef(M1)["(Intercept)"]
#extract 2nd column, row 1 from the coefficients object
se_b0_head = out$coefficients[1, 2] 
t_val_for_b0_head <- (b0_head - 10000)/se_b0_head
t_val_for_b0_head > tval
# FALSE
# Ta ko the bac bo H0: b0 = 10000

# c) Use the fitted regression model to estimate the gross box office results for
# the current week (in $) for a production with $400,000 in gross box office
# the previous week. 

# Find a 95% prediction interval for the gross box office results 
# for the current week (in $) for a production with $400,000 in gross
# box office the previous week.

predict(M1,data.frame(x = 400000),interval="prediction",level=0.95)
# fit        lwr        upr
#  399637.5  359832.8   439442.2

par(mfrow = c(2, 2))
plot(M1)
detach(Playbill)
