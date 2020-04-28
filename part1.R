
lambda <- 0.2
n <- 40
sim <- 1000

# calculate mean of exponential simulations
set.seed(2020)
mean_sim <- replicate(sim, mean(rexp(n, lambda)), simplify = TRUE)
summary(mean_sim)

# Sample mean vs theoretical mean

## obtain actual mean from summary
summary(mean_sim)[4]

## plot histogram if distribution
hist(mean_sim, breaks = 40, xlim = c(2, 8),
     main = "Exponential distribution means", col = "skyblue", xlab = "Mean")
abline(v = mean(mean_sim), lwd = 2, col = "blue", lty = 4)
abline(v = 1 / lambda, lwd = 2, col = "red", lty = 2)
legend("topright", legend = c("actual mean", "theoretic mean"), 
       col = c("blue", "red"), lty = 4:2)

# Sample variance vs theoretical variance

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x - 1)

data("mtcars")
lm(mpg ~ wt, data = mtcars)

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x - mean(x))/sd(x)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y ~ x)
coef(summary(fit))
summary(fit)$sigma

x <- mtcars$wt
y <- mtcars$mpg
fit <- lm(y ~ x)
predict(fit, newdata = data.frame(x = mean(x)), interval = ("confidence"))
predict(fit, newdata = data.frame(x = 3), interval = ("prediction"))

fit2 <- lm(y ~ I(x/2))
predict(fit2, newdata = data.frame(x - mean(x)), interval = ("prediction"))
