setwd("E:\00. Cao hoc\01. Mo hinh hoa thong ke\01. Data - code R\01. Linear")

#dev.off ()
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
# Xem cac params in M1
# names(M1)
coef(M1)

plot(x,y)

#Add Straight Lines to a Plot
abline(M1,lwd=3,col="red")

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y)) #SXY
  denom <- sum((x - mean_x)^2) #SXX
  b1 <- nom / denom # SXY / SXX
  return(b1)
}

intercept <- function(x, y, b1){
  b0 <- mean(y) - (b1 * mean(x))
  return(b0)
}
my_slope <- slope(df$Invoices, df$Time)
my_intercept <- intercept(df$Invoices, df$Time, my_slope)

my_slope
# b1 = 0.0112916
my_intercept
# b0 = 0.6417099
# (a) Find a 95% confidence interval for the start-up time, i.e., b0.
CI_b0 = confint(M1)
CI_b0[1, ]

# b) Test the null hypothesis H0: b1=0.01 against a two-sided H1.
# Interpret your result
# alpha = 0.05
# df = 30 - 2 = 28
# H0: b1 = 0.01
# H1: b1 != 0.01
tval <- qt(1-0.05/2, 28)
tval # Hoac co the tra bang t
b1_head <- coef(M1)["x"]
se_b1_head = out$coefficients[2, 2] 
t_val_for_b1_head <- (b1_head - 0.01)/se_b1_head
t_val_for_b1_head > tval
# False => T_b1 < tval
# Ket Luan: Ta ko the bac bo H0: b1 = 0.01

# Cach 2: Lay ket qua tu bang:
b0 <- 0.6417099  
b1 <- 0.0112916
se_b1 = 0.0008184
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
RSS<- function(x,y){
  b0 <- 0.6417099  
  b1 <- 0.0112916  
  sum_y_head <- sum((y - (b0 + b1*x))^2)  
}
rss_val <- RSS(df$Invoices, df$Time)
rss_val
# 3.045013
s <- sqrt(rss_val/(30-2)) # 
s 
# 0.3297733

# Do x* = 130 = x_bar => x*-x_bar = 0
# => pred_intervals = yhead +/- t*s*(1+1/n)

# Compute the lower range of 95% prediction interval
y_head - tval*s*sqrt(1+1/30)
# 1.422942

# Compute the upper range of 95% prediction interval
y_head + tval*s*sqrt(1+1/30)
# 2.796294

# Kiem tra ket qua voi function in R:
predict(M1,data.frame(x = 130),interval="prediction",level=0.95)
# fit (yhead)   lwr       upr
# 2.109624      1.422947  2.7963

