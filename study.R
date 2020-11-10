# Regression: modeling and model diagnostics
# NY municipalities expenditures data

# Load libraries
library(MASS)
library(corrplot)
library(car)
library(leaps)
library(googledrive)

rm(list=ls(all=TRUE)) # remove all previous objects from memory
options(warn=-1)  # forces R to ignore all warning messages

# Read in data, remove missing data
drive_download(as_id("1yFp7yiD3ZPsN6fV4yuu2Dup-6ThIzNU4"), path = "~/irp_study.dat")
ny<-read.table("~/irp_study.dat",header=T); dim(ny)
ny2<-na.omit(ny); dim(ny2) # 914  11
attach(ny2)

## review data set: expen is response
names(ny2)
head(ny2, n=5)

## Explore log-transformation of response variable:
par(mfrow=c(2,1))
hist(expen, xlab="Expenditure Per Person (Dollars)", main="")
hist(log(expen), xlab="Log-Expenditure Per Person (Dollars)", main="")

## Scatter of each var & its log transform w/ Lowess smooth
# Wealth
plot(wealth, log(expen), xlab="Wealth", ylab="Log-Expenditure")  ## linear
lines(lowess(wealth,log(expen)), col="green")
plot(log(wealth), log(expen), xlab="Log-Wealth", ylab="Log-Expenditure")
lines(lowess(log(wealth), log(expen)), col="green")

# Population
plot(pop, log(expen), xlab="Population", ylab="Log-Expenditure")  ## linear
lines(lowess(pop,log(expen)), col="green")
plot(log(pop), log(expen), xlab="Log-Population", ylab="Log-Expenditure")
lines(lowess(log(pop), log(expen)), col="green")
lines(c(8.3,8.3),c(0,6), col="grey", lwd=3)

# Percent Intergovernmental
plot(pint, log(expen), xlab="Percent Intergovernmental",
     ylab="Log-Expenditure")
lines(lowess(pint, log(expen)), col="green")
plot(log(pint), log(expen), xlab="Log-Percent Intergovernmental", 
     ylab="Log-Expenditure")
lines(lowess(log(pint), log(expen)), col="green")

# Density
plot(dens, log(expen), xlab="Density", ylab="Log-Expenditure")  ## linear
lines(lowess(dens,log(expen)), col="green")
plot(log(dens), log(expen), xlab="Log-Density", ylab="Log-Expenditure")
lines(lowess(log(dens), log(expen)), col="green")
lines(c(4.5,4.5),c(0,6), col="grey", lwd=3)

# Income
plot(income, log(expen), xlab="Income", ylab="Log-Expenditure")  ## linear
lines(lowess(income,log(expen)), col="green")
plot(log(income), log(expen), xlab="Log-Income", ylab="Log-Expenditure")
lines(lowess(log(income), log(expen)), col="green")

# Growth rate
lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))
plot(growr, log(expen), xlab="Growth", ylab="Log-Expenditure")  ## linear
lines(lowess(growr, log(expen)), col="green")
plot(lgrowr, log(expen), xlab="Log-Growth", ylab="Log-Expenditure")
lines(lowess(lgrowr, log(expen)), col="green")

# New Predictions
warwick_2005 = data.frame(wealth=85000, pop=20442, pint=24.7, dens= 214, 
                          income=19500, lgrowr=log(35+1))
warwick_2025 = data.frame(wealth=89000, pop=31033, pint=26.0, dens= 325, 
                          income=20000, lgrowr=log(40+1))
monroe_2005 = data.frame(wealth=58000, pop=10496, pint=8.8, dens= 695, 
                         income=17100, lgrowr=log(35+1))
monroe_2025 = data.frame(wealth=60000, pop=13913, pint=10.1, dens= 959, 
                         income=1800, lgrowr=log(35+1))

# Choose subset of data containing desired predictions where log-population and 
# log-density are linear 
pred_subset = (log(pop) > 8.3 & log(dens) > 4.5)

pairs(~log(expen)+log(wealth)+log(pop)+log(pint)+
        log(dens)+log(income)+lgrowr,data=ny2,
      main="Simple Scatterplot Matrix", subset=pred_subset)


ny2_subset <- subset(ny2, log(pop) > 8.3 & log(dens) > 4.5)
ny2_log_subset = data.frame(lwealth=matrix(nrow=228))
ny2_log_subset$lwealth <- log(ny2_subset$wealth)
ny2_log_subset$lpop <- log(ny2_subset$pop)
ny2_log_subset$lpint <- log(ny2_subset$pint)
ny2_log_subset$ldens <- log(ny2_subset$dens)
ny2_log_subset$lincome <- log(ny2_subset$income)
ny2_log_subset$lgrowr <- ny2_subset$lgrowr

cor(ny2_log_subset)

fit_full<-lm(log(expen)~log(wealth)+log(pop)+log(pint)+
               log(dens)+log(income)+lgrowr, subset = pred_subset)
summary(fit_full)

# Try AIC model selection
fit_aic <- stepAIC(fit_full, direction="both")
summary(fit_aic)

# leaps: exhaustive best subsets search
regfit = regsubsets(log(expen)~log(wealth)+log(pop)+log(pint)+log(dens)+
                      log(income)+lgrowr, data = ny2, subset = pred_subset)

par(mfrow=c(2,1))
plot(summary(regfit)$cp, xlab="Number of Variables", ylab="Cp")
plot(summary(regfit)$bic, xlab="Number of Variables", ylab="BIC")
which.min(summary(regfit)$cp); which.min(summary(regfit)$bic)

reduced_fit <- lm(log(expen)~log(wealth)+log(pop)+log(pint)+log(income), 
                  subset=pred_subset)
summary(reduced_fit)

par(mfrow=c(1,1))


# Influential Observations
# Cook's D plot
# identify D values > 4/(n-p-1) as a guide; 
# Cook and Weisberg recommend 0.5 and 1 
#(R uses these guides in default diagnostic plots below)
cutoff <- 4/((nrow(ny2_subset)-length(reduced_fit$coefficients)-2))
plot(reduced_fit, which=4, cook.levels=cutoff) # influence Plot

# VIF: don't want to put in polynomial terms since they are correlated!
vif(reduced_fit) # closer to 1 the better; 5-10 is moderate

# All encompansing R default regression model diagnostics
par(mfrow=c(2,2))
plot(reduced_fit)
confint(reduced_fit,level = 0.95)

# Predictions

# Account for sigma^2
sdfit=sd(reduced_fit$resid)

w2005 <- as.integer(exp(predict(reduced_fit, warwick_2005, 
                                interval="prediction", level=0.95)+sdfit^2/2))
w2025 <- as.integer(exp(predict(reduced_fit, warwick_2025, 
                                interval="prediction", level=0.95)+sdfit^2/2))

m2005 <- as.integer(exp(predict(reduced_fit, monroe_2005, 
                                interval="prediction", level=0.95)+sdfit^2/2))
m2025 <- as.integer(exp(predict(reduced_fit, monroe_2025, 
                                interval="prediction", level=0.95)+sdfit^2/2))




