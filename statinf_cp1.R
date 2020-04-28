lambda <- 0.2
n <- 40
sim <- 1000
set.seed(2020)
mean_sim <- replicate(sim, mean(rexp(n, lambda)), simplify = TRUE)
summary(mean_sim)

sd_t <- round((1 / lambda) / sqrt(n), 3) # theoretical sd
sd_o <- round(sd(mean_sim), 3) # observed sd
var_t <- round(((1 / lambda) / sqrt(n))^2, 3) # theoretical variance
var_o <- round(sd(mean_sim)^2, 3) # observed variance
std_dev <- c(sd_t, sd_o)
variance <- c(var_t, var_o)
q2 <- cbind(std_dev, variance)
rownames(q2) <- c("theoretical", "observed")
q2

x_norm <- seq(min(mean_sim), max(mean_sim), length = 2*n)
y_norm <- dnorm(x_norm, mean = 1 / lambda, sd = (1 / lambda) / sqrt(n))

hist(mean_sim, prob = TRUE, breaks = 40, xlim = c(2, 8), 
     main = "Distribution comparison", col = "skyblue", xlab = "Mean")
lines(x_norm, y_norm, col = "red", lwd = 2, lty = 2)
lines(density(mean_sim), col = "blue", lwd = 2, lty = 4)
legend("topright", legend = c("sample", "normal"), col = c("blue", "red"),
       lty = 4:2)





