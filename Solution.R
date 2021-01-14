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

plot(x,y,xlab="Gross box office results previous week",
     ylab="Gross box office results current week")

#Add Straight Lines to a Plot
abline(M1,lwd=3,col="red")

# a) Find a 95% CI for the slope of the regression model, b1. 
# Is 1 a plausible value for b1? Reason to support your answer?
# A: Yes, because we are 95% sure that the value of b1 is somewhere  
# between 0.95 and 1.01
# CI - Confident Interval 95%
confint(M1)
#                 2.5 %       97.5 %
# (Intercept) -1.424433e+04 27854.099443
# x            9.514971e-01     1.012666

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
# fit      lwr      upr
#  399637.5 359832.8 439442.2
detach(Playbill)

# Bai 3: Invoice
# ======
#   The manager of the purchasing department of a large company would like to
# develop a regression model to predict the average amount of time it takes to
# process a given number of invoices. Over a 30-day period, data are collected on
# the number of invoices processed and the total time taken (in hours). The data
# are available on the book web site in the file invoices.txt. The following model
# was fit to the data:
#   Y = b0+b1*x+e 
# where Y is the processing time and x is the number of invoices. 
# A plot of the data and the fitted model can be found in
# Figure 2.7 . Utilizing the output from the fit of this model provided below, 
# complete the following tasks.
#   ======

df<-read.csv(file.choose(), header = TRUE, sep = "\t")
attach(df)

dim(df)
df[1:5,]
summary(df)
#pairplot
plot(df)

y <- df$Time
x <- df$Invoices
M1 <- lm(y ~ x)
out = summary(M1)
summary(M1)
# Coefficients:
#             Estimate Std. Error       t value     Pr(>|t|)    
# (Intercept) 0.6417099     0.1222707   5.248       1.41e-05 ***
#   x         0.0112916     0.0008184   13.797      5.17e-14 ***

coef(M1)

plot(x,y)
abline(M1,lwd=3,col="red")

# Ta lay ket qua tu bang output:
b0 <- 0.6417099  
b1 <- 0.0112916
se_b0 = 0.122707
se_b1 = 0.0008184

beta0_t <- 5.248
beta0_margin <- 1.96 * beta0_se
(beta0_95 <- c(beta0 - beta0_margin, beta0 + beta0_margin))
# (a) Find a 95% confidence interval for the start-up time, i.e., b0.
# Compute the lower range of 95% confidence interval
tval <- qt(1-0.05/2,30-2)
tval
# 2.048407
b0 - tval*se_b0
# 0.390356

# Compute the upper range of 95% confidence interval
b0 + tval*se_b0
# 0.8930638
# Kiem tra lai bang ham confint()
CI_b0 = confint(M1)
CI_b0[1, ]
#   2.5 %     97.5 % 
#   0.3912496 0.8921701 

# b) Test the null hypothesis H0: b1=0.01 against a two-sided H1.
# Interpret your result
# alpha = 0.05
# df = 30 - 2 = 28
# H0: b1 = 0.01
# H1: b1 != 0.01

T_b1 <- (b1 - 0.01)/se_b1
T_b1 > tval
# False => T_b1 < tval
# Ket Luan: Ta ko the bac bo H0: b1 = 0.01

# c) Find a point estimate and a 95% prediction interval 
# for the time taken to process 130 invoices

# Find a point estimate: 
y_head = b0 + b1*130
y_head
# 2.109618

# Compute prediction intervals
# Lay ket qua tu bang:
s <- 0.3298 #Residual standard error

# Do x* = 130 = x_bar => x*-x_bar = 0
# => pred_intervals = yhead +/- t*s*(1+1/n)

# Compute the lower range of 95% prediction interval
y_head - tval*s*sqrt(1+1/30)
# 1.422886

# Compute the upper range of 95% prediction interval
y_head + tval*s*sqrt(1+1/30)
# 2.79635

# Kiem tra ket qua voi function in R:
predict(M1,data.frame(x = 130),interval="prediction",level=0.95)
# fit (yhead)   lwr       upr
# 2.109624      1.422947  2.7963

detach(df)
