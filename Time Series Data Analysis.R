# Read-in Data
temp = read.table("Temperature.txt",header = T)
temp[temp == -99.9] = NA # Substitue -99.9s with NAs
set.caption("Summary of the Dataset")
pander(summary(temp[,2:5]))


# Vectorize the matrix
temp00 = as.vector(t(temp[,2:5]))
temp0 = temp00[-c(1,length(temp00),length(temp00)-1)]


# Polynomial Regression on temperature with respect to time
Q = factor(c(2,3,4,rep(1:4, 357),1,2))
t = seq(length(temp0))
fit11 = lm(temp0 ~ t + I(t^2) + I(t^3) + Q - 1)
round(summary(fit11)$coef[,c(1,4)],6)


# Plots of the entire sequence
year = rep(1659:2017, each = 4)[-c(1, 1435, 1436)]
plot(ksmooth(time(temp0), temp0, "normal", bandwidth = 40), cex.axis = 0.7, type = "l",
     xaxt = "n", xlab = "Year", ylab = "Temperature", main = "Annual")
axis(1, year, labels = year[seq(1, 1433, 200)], at = time(temp0)[seq(1, 1433, 200)],
     cex.axis = .7)
abline(lm(temp0 ~ t), lty = 2)


# Plots of different seasons
par(mfrow=c(2,2))
year_sep = 1659:2017
name = c("Winter", "Spring", "Summer", "Autumn")
plot_1 = function(index)
{
  plot(ksmooth(time(temp[, index]), temp[, index], "normal", bandwidth = 10), type = "l",
       xlab = "Year", cex.axis = .7, ylab = "Temperature", xaxt = "n",
       main = name[index - 1])
  axis(1, year_sep, labels = year_sep[seq(1, 359, 50)], 
       at = time(year_sep)[seq(1, 359, 50)], cex.axis = .7)
  abline(lm(temp[, index] ~ seq(length(temp[, index]))), lty = 2)
  return(0)
}
result = sapply(2:5, plot_1)


# Split `years`` into 3 subintervals
# Linear models and their plots
temp1 = temp[1:241,]; temp2 = temp[242:341,]; temp3 = temp[342:359,]
temp10 = as.vector(t(temp1[,2:5]))[-1]
temp20 = as.vector(t(temp2[,2:5]))
temp30 = as.vector(t(temp3[,2:5]))[-c(71,72)]

## 3 linear models
coef1 = round(summary(lm(temp10 ~ seq(length(temp10))))$coef[2,1],5)
coef2 = round(summary(lm(temp20 ~ seq(length(temp20))))$coef[2,1],5)
coef3 = round(summary(lm(temp30 ~ seq(length(temp30))))$coef[2,1],5)

## Comparison to the original plot
plot(ksmooth(time(temp0), temp0, "normal", bandwidth = 40), cex.axis = 0.7, type = "l",
     xaxt = "n", xlab = "Year", ylab = "Temperature", main = "Annual")
axis(1, year, labels = year[seq(1, 1433, 200)], at = time(temp0)[seq(1, 1433, 200)],
     cex.axis = .7)
abline(lm(temp0 ~ t), lty = 2)
lines(c(1,963),c(8.905,9.2117),lty=3,col="red")
lines(c(964,1363),c(9.122,9.8569),lty=3,col="red")
lines(c(1364,1433),c(10.236,10.0720),lty=3,col="red")


# Global warming trend in 21st century?
Q1 = factor(c(rep(1:4, 17),1,2))
st21 = cbind(temp30,Q1)

## Allocate data into corresponding season
st211 = subset(st21,Q1==1); st212 = subset(st21,Q1==2)
st213 = subset(st21,Q1==3); st214 = subset(st21,Q1==4)

## Build linear models according to different seasons
winter = summary(lm(st211[,1] ~ seq(18)))$coef[2,1]
spring = summary(lm(st212[,1] ~ seq(18)))$coef[2,1]
summer = summary(lm(st213[,1] ~ seq(17)))$coef[2,1]
autumn = summary(lm(st214[,1] ~ seq(17)))$coef[2,1]

## Table of coefficients
set.caption("Coefficients for different seasons")
pander(data.frame(winter=winter,spring=spring,summer=summer,autumn=autumn))

## Plots
par(mfrow=c(2,2))
plot(seq(length(temp[342:359,2])) + 2000, cex.axis = 0.7,
     temp[342:359,2],type="l",xlab="Year", ylab="Temperature", main="Winter")
plot(seq(length(temp[342:359,3])) + 2000, cex.axis = 0.7,
     temp[342:359,3],type="l",xlab="Year", ylab="Temperature", main="Spring")
plot(seq(length(temp[342:359,4])) + 2000, cex.axis = 0.7,
     temp[342:359,4],type="l",xlab="Year", ylab="Temperature", main="Summer")
plot(seq(length(temp[342:359,5])) + 2000, cex.axis = 0.7,
     temp[342:359,5],type="l",xlab="Year", ylab="Temperature", main="Autumn")


# Detrending
tempn = temp0 - 0.00068*t
round(summary(lm(tempn ~ t + Q - 1))$coef[,c(1,4)],2)


# Differencing
## Lag = 1
tempd = diff(temp0, lag=1)
dQ = Q[-length(Q)]
t1 = t[-length(t)]
round(summary(lm(tempd ~ t1 + dQ - 1))$coef[,c(1,4)],2)

## Lag = 4
tempd2 = diff(temp0,lag=4)
t2 = t[-(1:4)]
round(summary(lm(tempd2 ~ t2))$coef[,c(1,4)],5)


# Seasonal Arima Models
sarima(temp0,1,1,1,0,1,1,4,details=F)$ttable
sarima(temp0,1,0,1,0,1,1,4,details=F)$ttable


# Forecast
## The reason why I write down all these predictions is that the `sarima.for` function
## is highly wrapped up, so there is no method to force the function not to build plots
## and change the parameters to make the plot elegent. Thus I have to write down 
## all predictions
Temperature=temp0
result1=sarima.for(Temperature,14,1,1,1,0,1,1,4)
result2=sarima.for(Temperature,14,1,0,1,0,1,1,4)
y1 = c(16.354269,10.993774,4.949687,9.251636,16.178089,10.971972,4.955211,
     9.261996,16.189305,10.983340,4.966605,9.273395,16.200706,10.994740)
y2 = c(16.155738, 10.933240,4.783233,9.034639,15.876340,10.745123,
       4.656939,8.950218,15.820279,10.708269,4.633094,8.935184,15.811212,10.703244)

mat1 = matrix(c(5.4,10.3,y1), ncol = 4, nrow = 4, byrow = T)
mat2 = matrix(c(5.4,10.3,y2), ncol = 4, nrow = 4, byrow = T)
rownames(mat1) = c("2017","2018","2019","2020")
colnames(mat1) = c("Winter","Spring","Summer","Autumn")
rownames(mat2) = c("2017","2018","2019","2020")
colnames(mat2) = c("Winter","Spring","Summer","Autumn")
set.caption("Forecast with `sarima(1,1,1,0,1,1)`")
pander(mat1)
set.caption("Forecast with `sarima(1,0,1,0,1,1)`")
pander(mat2)