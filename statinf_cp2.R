data("ToothGrowth")
summary(ToothGrowth)

ToothGrowth$dose1 <- as.factor(ToothGrowth$dose)
unique(ToothGrowth$dose1)

hist(ToothGrowth$len, breaks = 5, xlim = c(0, 35), main = "Tooth growth",
     col = "skyblue", xlab = "Tooth length")

boxplot(len ~ supp*dose1, data = ToothGrowth, col = (c("gold", "skyblue")), 
        xlab = "Supplement and growth", main = "Tooth growth", ylab = "Length")

t.test(len ~ supp, data = ToothGrowth)
t.test(len ~ supp, data = ToothGrowth[ToothGrowth$dose %in% c(0.5, 1.0),])
t.test(len ~ dose, data = ToothGrowth[ToothGrowth$dose %in% c(0.5, 2),])

