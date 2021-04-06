#tut 9 
rm(list=ls())	
setwd("C:/Users/27671/Documents/Honours/Stats module/Tutorials")
fruitq1 <- read.csv("tut9 q1.csv")
plot(fruitq1$Nodes, fruitq1$fruit.density)
length(fruit.density)

windows()
par(las = 1, mex = 1.5, mai = c(1.5,1.5,0.1,0.1))
plot(fruitq1$Nodes, fruitq1$fruit.density, xlab="number of branching nodes (No. nodes)", 
     ylab = "Mean Fruit densities (numbers per 25cm2) ", 
     cex = 1.5, 
     cex.lab = 1.5, cex.axis = 1.5, 
     xlim = c(0,20))

# Semi-log transform - Exponential curve
windows(width=8,height=4)
par(mfrow=c(1,2),mex=0.8, las = 1)
plot(fruitq1$Nodes, log(fruitq1$fruit.density), xlab="number of branching nodes (No. nodes)", 
     ylab = "Mean Fruit densities (numbers per 25cm2)")
legend ("topleft", expression(bold("a")), x.intersp = 0, 
        bty = "n", cex = 2)
#Log log transform - Power curve
plot(log(fruitq1$Nodes), log(fruitq1$fruit.density), xlab="number of branching nodes (No. nodes)", 
     ylab = "Mean Fruit densities (numbers per 25cm2)")
legend ("topleft", expression(bold("b")), x.intersp = 0, 
        bty = "n", cex = 2)

attach(fruitq1)

f <- function(Nodes,a,b) {a * exp(b * Nodes)}

Fruit = nls(fruit.density ~ f(Nodes,a,b), data = fruitq1,
            start = c(a=0.1, b=0.1))

## Model Validation 
xlab1="Predicted values (Mean Fruit densities (numbers per 25cm2))"
ylab1="Residuals (Mean Fruit densities (numbers per 25cm2))"
res = residuals(Fruit)
range.res = max(res) - min(res)
yfit <- fitted.values(Fruit)
windows(width = 8, height = 3)
par(mfrow = c(1,3), mex = 0.8, cex = 0.9, las = 1)
# Check spread of residuals above and below the line for equal variance
plot(yfit, res, xlab = xlab1, ylab = ylab1)
abline(0, 0, lty = 3, col = "red")
legend ("topleft", expression(bold("a")), x.intersp = 0, 
        bty = "n", cex = 2)
# Quantile quantile plot - Normal Q-Q plot
qqnorm(res, main = "")
qqline(res)
legend ("topleft", expression(bold("b")), x.intersp = 0, 
        bty = "n", cex = 2)
#histogram residuals
hist(res,  breaks = 5, 
     xlab = ylab1, main = "")
legend ("topleft", expression(bold("c")), x.intersp = 0, 
        bty = "n", cex = 2)

summary(Fruit)

logFruit = lm(log(fruit.density)~Nodes, data = fruitq1)

## Model Validation
xlab1="Predicted values (log of the Mean Fruit densities (numbers per 25cm2))"
ylab1="Residuals (log of the Mean Fruit densities (numbers per 25cm2) )"
res = residuals(logFruit)
range.res = range(res)
yfit <- fitted.values(logFruit)
windows(width = 8, height = 3)
par(mfrow = c(1,3), mex = 1.2, cex = 0.8, las = 1)
# Check spread of residuals above and below the line 
#       for equal variance
plot(yfit, res, xlab = xlab1, ylab = ylab1)
abline(0, 0, lty = 3, col = "red")
legend ("topleft", expression(bold("a")), x.intersp = 0, 
        bty = "n", cex = 1.3)
# Quantile quantile plot - Normal Q-Q plot
qqnorm(res, main="Model Validation")
qqline(res)
legend ("topleft", expression(bold("b")), x.intersp = 0, 
        bty = "n", cex = 1.3)
#histogram residuals
hist(res, main = "", breaks = 5, xlab = xlab1)
legend ("topleft", expression(bold("c")), x.intersp = 0, 
        bty = "n", cex = 1.3)


summary(logFruit)

NODE = 10
pred.Y = data.frame(Nodes = NODE)
Ypred = predict(logFruit, pred.Y)
Ypred
exp(Ypred)

# wrong a <- round(summary(Fruit)$coefficients[1, 1], 3)
#b <- round(summary(Fruit)$coefficients[2, 1], 3)
#a
#b

#NODE = 10
#pred.Y = data.frame(Nodes = NODE)
#Ypred = predict(Fruit, pred.Y)
#Ypred


lines(Nodes, predict(Fruit), col = "red")

detach(fruitq1)
# Q2

rm(list=ls())	

attach(phytoq2)
phytoq2 <- read.csv("tut9 q2.csv")
plot(phytoq2$nitrate.con, phytoq2$uptake.rate, xlab = "nitrate concentration (pg at.L-1)", ylab = "Uptake rate (µg at N.L-1h-1)")



f2 <- function(nitrate.con,a,b) {(a * nitrate.con) / (b + nitrate.con)}

uptake = nls(uptake.rate ~ f2(nitrate.con,a,b), data = phytoq2,
            start = c(a=0.1, b=0.1))
summary(uptake)
a2 <- round(summary(uptake)$coefficients[1, 1], 3)
b2 <- round(summary(uptake)$coefficients[2, 1], 3)
a2
b2

## Model Validation 
xlab1="Predicted values Uptake rate (µg at N.L-1h-1)"
ylab1="Residuals (Uptake rate (µg at N.L-1h-1))"
res = residuals(uptake)
range.res = max(res) - min(res)
yfit <- fitted.values(uptake)
windows(width = 8, height = 3)
par(mfrow = c(1,3), mex = 0.8, cex = 0.9, las = 1)
# Check spread of residuals above and below the line for equal variance
plot(yfit, res, xlab = xlab1, ylab = ylab1)
abline(0, 0, lty = 3, col = "red")
legend ("topleft", expression(bold("a")), x.intersp = 0, 
        bty = "n", cex = 2)
# Quantile quantile plot - Normal Q-Q plot
qqnorm(res, main = "")
qqline(res)
legend ("topleft", expression(bold("b")), x.intersp = 0, 
        bty = "n", cex = 2)
#histogram residuals
hist(res,  breaks = 5, 
     xlab = ylab1, main = "")
legend ("topleft", expression(bold("c")), x.intersp = 0, 
        bty = "n", cex = 2)
