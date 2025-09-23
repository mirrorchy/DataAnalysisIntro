# For slides p54 p58 p64 p67: PM2.5 density plots and quantile function plots - 2015 vs 2023
# Output: pm25_density_2015.png, pm25_density_2023.png, pm25_quantile_2015.png, pm25_quantile_2023.png, pm25_beijing_hist_density_2015.png, pm25_beijing_hist_density_2023.png,pm25_beijing_quantile_2015_2023.png

# 将工作路径设置为现文件夹所在目录
setwd("D:/GitHub/DataAnalysisIntro(Public)/")

df <- read.csv("Input/Lec2/Beijing_Aotizhongxin.csv", header = TRUE)


library(dplyr)


beijing_2015 <- df$PM2.5[ df$year == 2015 & df$season == 4]
beijing_2023 <- df$PM2.5[ df$year == 2023 & df$season == 4]

# 均值和中位数
mean_2015 <- mean(beijing_2015, na.rm = TRUE)
median_2015 <- median(beijing_2015, na.rm = TRUE)
mean_2023 <- mean(beijing_2023, na.rm = TRUE)
median_2023 <- median(beijing_2023, na.rm = TRUE)
sd_2015 <- sd(beijing_2015, na.rm = TRUE)
sd_2023 <- sd(beijing_2023, na.rm = TRUE)

# 密度对象
dens_2015 <- density(beijing_2015, na.rm = TRUE)
dens_2023 <- density(beijing_2023, na.rm = TRUE)

# 统一x/y轴范围
xlim_density <- range(c(beijing_2015, beijing_2023), na.rm = TRUE)
ymax_density <- max(dens_2015$y, dens_2023$y)
ylim_density <- c(0, ymax_density * 1.05)


# 2015密度曲线，均值和中位数
png(filename = "Output/Lec2/pm25_beijing_density_2015.png", width = 800, height = 800)
par(pty = "s")
plot(dens_2015, main = "北京2015年", xlab = "", ylab = "Density", col = "black", cex.main = 3, cex.lab = 2, cex.axis = 2, lwd = 3, xlim = xlim_density, ylim = ylim_density)
abline(v = mean_2015, col = "red", lwd = 3, lty = 2)
abline(v = median_2015, col = "blue", lwd = 3, lty = 3)
legend("topright", legend = c("Mean", "Median"), col = c("red", "blue"), lty = c(2,3), lwd = 2, cex = 2)
text(mean_2015, max(dens_2015$y)*0.95, paste0("mean=", round(mean_2015,2)), col = "red", pos = 4, cex = 2.2)
text(median_2015, max(dens_2015$y)*0.85, paste0("median=", round(median_2015,2)), col = "blue", pos = 4, cex = 2.2)
dev.off()

# 2023密度曲线，均值和中位数
png(filename = "Output/Lec2/pm25_beijing_density_2023.png", width = 800, height = 800)
par(pty = "s")
plot(dens_2023, main = "北京2023年", xlab = "", ylab = "Density", col = "black", cex.main = 3, cex.lab = 2, cex.axis = 2, lwd = 3, xlim = xlim_density, ylim = ylim_density)
abline(v = mean_2023, col = "red", lwd = 3, lty = 2)
abline(v = median_2023, col = "blue", lwd = 3, lty = 3)
legend("topright", legend = c("Mean", "Median"), col = c("red", "blue"), lty = c(2,3), lwd = 2, cex = 2)
text(mean_2023, max(dens_2023$y)*0.95, paste0("mean=", round(mean_2023,2)), col = "red", pos = 4, cex = 2.2)
text(median_2023, max(dens_2023$y)*0.85, paste0("median=", round(median_2023,2)), col = "blue", pos = 4, cex = 2.2)
dev.off()

# 2015直方图+密度
xlim_hist <- xlim_density
ylim_hist <- range(0, dens_2015$y, dens_2023$y)
png(filename = "Output/Lec2/pm25_beijing_hist_density_2015.png", width = 800, height = 800)
par(pty = "s")
hist(beijing_2015, breaks = 10, freq = FALSE, col = rgb(0,0,1,0.2), border = NA, main = "北京2015年", xlab = "", ylab = "", cex.main = 2.5, cex.lab = 2.5, cex.axis = 2.5, xlim = xlim_hist, ylim = ylim_hist, axes = FALSE)
lines(dens_2015, col = "blue", lwd = 3)
abline(v = mean_2015, col = "red", lwd = 3, lty = 2)
text(mean_2015, max(dens_2015$y)*0.95, paste0("mean=", round(mean_2015,2)), col = "red", pos = 4, cex = 2.2)
axis(1, at = pretty(xlim_hist), lwd = 2, cex.axis = 2.5)
axis(2, at = pretty(ylim_hist), lwd = 2, cex.axis = 2.5)
box(lwd = 2)
segments(xlim_hist[1], 0, xlim_hist[2], 0, lwd = 2)
dev.off()

# 2023直方图+密度
png(filename = "Output/Lec2/pm25_beijing_hist_density_2023.png", width = 800, height = 800)
par(pty = "s")
hist(beijing_2023, breaks = 10, freq = FALSE, col = rgb(1,0,0,0.2), border = NA, main = "北京2023年", xlab = "", ylab = "", cex.main = 2.5, cex.lab = 2.5, cex.axis = 2.5, xlim = xlim_hist, ylim = ylim_hist, axes = FALSE)
lines(dens_2023, col = "red", lwd = 3)
abline(v = mean_2023, col = "red", lwd = 3, lty = 2)
text(mean_2023, max(dens_2023$y)*0.95, paste0("mean=", round(mean_2023,2)), col = "red", pos = 4, cex = 2.2)
axis(1, at = pretty(xlim_hist), lwd = 2, cex.axis = 2.5)
axis(2, at = pretty(ylim_hist), lwd = 2, cex.axis = 2.5)
box(lwd = 2)
segments(xlim_hist[1], 0, xlim_hist[2], 0, lwd = 2)
dev.off()


# 2015百分位数密度图
q10_2015 <- quantile(beijing_2015, 0.1, na.rm = TRUE)
q90_2015 <- quantile(beijing_2015, 0.9, na.rm = TRUE)
png(filename = "Output/Lec2/pm25_beijing_density_2015_percentile.png", width = 800, height = 800)
par(pty = "s")
percentiles <- seq(0, 1, by = 0.1)
percentile_vals <- quantile(beijing_2015, percentiles, na.rm = TRUE)
plot(dens_2015, main = "北京2015年冬季PM2.5浓度", xlab = "Percentile", ylab = "Density", col = "red", cex.main = 3.5, cex.lab = 2, cex.axis = 2, lwd = 3, xaxt = "n", xlim = xlim_density, ylim = ylim_density)
axis(1, at = percentile_vals, labels = paste0(as.integer(percentiles*100), "%"), cex.axis = 2)
abline(v = q10_2015, col = "green", lwd = 3, lty = 2)
abline(v = q90_2015, col = "purple", lwd = 3, lty = 2)
text(q10_2015, max(dens_2015$y)*0.75, paste0(round(q10_2015,2)), col = "green", pos = 4, cex = 2.2)
text(q90_2015, max(dens_2015$y)*0.65, paste0(round(q90_2015,2)), col = "purple", pos = 4, cex = 2.2)
dev.off()

# 2023百分位数密度图
q10_2023 <- quantile(beijing_2023, 0.1, na.rm = TRUE)
q90_2023 <- quantile(beijing_2023, 0.9, na.rm = TRUE)
png(filename = "Output/Lec2/pm25_beijing_density_2023_percentile.png", width = 800, height = 800)
par(pty = "s")
percentiles <- seq(0, 1, by = 0.1)
percentile_vals <- quantile(beijing_2023, percentiles, na.rm = TRUE)
plot(dens_2023, main = "北京2023年冬季PM2.5浓度", xlab = "Percentile", ylab = "Density", col = "red", cex.main = 3.5, cex.lab = 2, cex.axis = 2, lwd = 3, xaxt = "n", xlim = xlim_density, ylim = ylim_density)
axis(1, at = percentile_vals, labels = paste0(as.integer(percentiles*100), "%"), cex.axis = 2)
abline(v = q10_2023, col = "green", lwd = 3, lty = 2)
abline(v = q90_2023, col = "purple", lwd = 3, lty = 2)
text(q10_2023, max(dens_2023$y)*0.75, paste0(round(q10_2023,2)), col = "green", pos = 4, cex = 2.2)
text(q90_2023, max(dens_2023$y)*0.65, paste0(round(q90_2023,2)), col = "purple", pos = 4, cex = 2.2)
dev.off()



# 箱线图
box_data <- data.frame(
  Year = rep(c("2015", "2023"), times = c(length(beijing_2015), length(beijing_2023))),
  PM25 = c(beijing_2015, beijing_2023)
)
png(filename = "Output/Lec2/pm25_beijing_boxplot.png", width = 800, height = 800)
par(pty = "s")
boxplot(PM25 ~ Year, data = box_data, main = "北京2015年和2023年PM2.5箱线图对比", ylab = "PM2.5浓度(μg/m^3)", col = c("lightblue", "lightgreen"), cex.main = 3, cex.lab = 2.5, cex.axis = 2.5, lwd = 2)
dev.off()


# 2015和2023分位数函数
# 2015分位数
q_2015 <- quantile(beijing_2015, probs = probs, na.rm = TRUE)
q10_2015 <- quantile(beijing_2015, 0.1, na.rm = TRUE)
q90_2015 <- quantile(beijing_2015, 0.9, na.rm = TRUE)
# 2023分位数
probs <- seq(0, 1, by = 0.001)
q_2023 <- quantile(beijing_2023, probs = probs, na.rm = TRUE)
q10_2023 <- quantile(beijing_2023, 0.1, na.rm = TRUE)
q90_2023 <- quantile(beijing_2023, 0.9, na.rm = TRUE)
ylim_qf <- range(quantile(beijing_2015, probs = probs, na.rm = TRUE), q_2023, na.rm = TRUE)

png(filename = "Output/Lec2/pm25_beijing_quantile_2015_2023.png", width = 1600, height = 800)
par(mfrow = c(1,2), pty = "s")

# 左：2015
plot(probs, q_2015, type = "l", main = "2015", xlab = "Cumulative Probability", ylab = "PM2.5浓度(μg/m^3)", col = "blue", cex.main = 2.5, cex.lab = 2, cex.axis = 2, lwd = 3, ylim = ylim_qf)
abline(v = 0.1, col = "green", lwd = 2, lty = 2)
abline(v = 0.9, col = "purple", lwd = 2, lty = 2)
points(0.1, q10_2015, col = "green", pch = 19, cex = 2)
points(0.9, q90_2015, col = "purple", pch = 19, cex = 2)
text(0.1, q10_2015, paste0("10%: ", round(q10_2015,2)), col = "green", pos = 4, cex = 1.5)
text(0.8, q90_2015, paste0("90%: ", round(q90_2015,2)), col = "purple", pos = 4, cex = 1.5)

# 右：2023
plot(probs, q_2023, type = "l", main = "2023", xlab = "Cumulative Probability", ylab = "PM2.5浓度(μg/m^3)", col = "red", cex.main = 2.5, cex.lab = 2, cex.axis = 2, lwd = 3, ylim = ylim_qf)
abline(v = 0.1, col = "green", lwd = 2, lty = 2)
abline(v = 0.9, col = "purple", lwd = 2, lty = 2)
points(0.1, q10_2023, col = "green", pch = 19, cex = 2)
points(0.9, q90_2023, col = "purple", pch = 19, cex = 2)
text(0.1, q10_2023, paste0("10%: ", round(q10_2023,2)), col = "green", pos = 4, cex = 1.5)
text(0.8, q90_2023, paste0("90%: ", round(q90_2023,2)), col = "purple", pos = 4, cex = 1.5)

dev.off()
