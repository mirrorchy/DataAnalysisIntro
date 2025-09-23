# Example adapted from David Spiegelhalter's "The Art of Statistics"

# 设置工作目录，确保后续文件路径正确
setwd("D:/GitHub/DataAnalysisIntro(Public)/")


# 1. 数据准备：分组区间、中点、每组人数、总人数、直方图面积
weights = c(1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)  # 分组下界
mids = weights + 250  # 每组中点
n = c(5+48+308+1130, 12679, 124209, 442891, 389275, 108886, 14936, 1345)  # 各组人数
N = sum(n)  # 总人数
area = N * 500  # 直方图总面积（人数*组宽）



# 2. 低出生体重儿数量及比例（<2500g）
lbw = sum(n[1:2])
lbw.percent = 100 * lbw / N  # 约1.3%

birth.mean=sum(n*mids/N)
birth.sd=sqrt( sum(n*(mids-birth.mean)^2)/N)

# 3. 计算总体均值和标准差（可用Sheppard修正，这里未用）
birth.mean = sum(n * mids / N)
birth.sd = sqrt(sum(n * (mids - birth.mean)^2) / N)


# 4. 用正态分布近似估算低体重儿比例
lbw.est = 100 * pnorm(2500, birth.mean, birth.sd)  # 约1.7%，与实际接近

xw = 2910  
pnorm(xw, birth.mean,birth.sd)

# 5. 计算25%和75%分位点，以及某一体重的累积概率
qnorm(0.25, birth.mean, birth.sd)
qnorm(0.75, birth.mean, birth.sd)
xw = 2910
pnorm(xw, birth.mean, birth.sd)


### 6. 计算四分位距（IQR）
q1 <- qnorm(0.25, birth.mean, birth.sd)
q3 <- qnorm(0.75, birth.mean, birth.sd)
IQR <- q3 - q1


# 7. 绘图参数设置
par(mfrow=c(2,2))
xrange <- c(1500, 5500)  # x轴范围
yrange <- range(c(n, area * dnorm(birth.mean, birth.mean, birth.sd), 0))  # y轴范围
scale = 3
par(mar = c(5, 0, 1, 0) + 0.1)

### modified

# 8. 绘制分位数函数（逆累积分布函数），展示不同概率下的出生体重
png("Output/Lec2/babyweight_percentiles.png", width=500, height=500)

# 百分数分位数函数图
p_seq <- seq(0, 1, length.out=200)
x_seq <- qnorm(p_seq, birth.mean, birth.sd)
plot(p_seq*100, x_seq, type="l", lwd=3, col="blue", xlab="Percentile (%)", ylab="Birthweight (gms)", main="Quantile Function (Inverse CDF)", cex.axis=1.2)
# 标注均值和常用分位点
abline(h=birth.mean, col="red", lty=2)
text(60, birth.mean+50, paste0('mean=', round(birth.mean, 2)), col="red", cex=1)
q10 <- qnorm(0.10, birth.mean, birth.sd)
q80 <- qnorm(0.80, birth.mean, birth.sd)
abline(h=q10, col="purple", lty=3)
abline(h=q80, col="orange", lty=3)
text(20, q10+50, paste0("10%:", round(q10, 2)), col="purple", cex=1)
text(90, q80+50, paste0("80%:", round(q80, 2)), col="orange", cex=1)
dev.off()





# 9. 绘制出生体重分布的直方图和正态密度曲线
png("Output/Lec2/babyweight_hist.png", width=500, height=500)
plot(xrange, yrange, type = "n", xlab = "", ylab = "",
     bty="n", axes=F, main=" Distribution of birthweights (g)", cex=scale)
axis(1, cex=scale)
# 绘制直方图
rect(weights, 0, weights + 500, n, col = "lightblue")
# 叠加正态分布密度曲线
curve(area * dnorm(x, birth.mean, birth.sd), min(xrange), max(xrange), add = TRUE, lwd=3, col="blue")
# 标注均值位置
x_mean <- birth.mean
y_mean <- area * dnorm(x_mean, birth.mean, birth.sd)
lines(c(x_mean, x_mean), c(0, y_mean), col = "red", lwd = 2)
text(x_mean, y_mean + 10000, paste0('mean=', round(x_mean,2)), col = "red", cex = 1.5)
dev.off()




# 10. 绘制带分位点标注的出生体重分布图，x轴为分位点
png("Output/Lec2/babyweight_IQR.png", width=500, height=500)
plot(xrange, yrange, type = "n", xlab = "", ylab = "",
     bty="n", axes=F, main=" Distribution of birthweights", cex=scale)
# 计算常用百分位对应的体重
percentiles <- seq(0, 100, by=10)
percentile_weights <- qnorm(percentiles/100, birth.mean, birth.sd)
# 画x轴为体重，但标签为百分位
axis(1, at=percentile_weights, labels=paste0(percentiles, "%"), cex.axis=1.2)
# 绘制直方图和正态密度曲线
rect(weights, 0, weights + 500, n, col = "lightblue")
curve(area * dnorm(x, birth.mean, birth.sd), min(xrange), max(xrange), add = TRUE, lwd=3, col="blue")
# 标注Q1和Q3位置
q1 <- qnorm(0.25, birth.mean, birth.sd)
q3 <- qnorm(0.75, birth.mean, birth.sd)
yq1 <- area * dnorm(q1, birth.mean, birth.sd)
yq3 <- area * dnorm(q3, birth.mean, birth.sd)
lines(c(q1, q1), c(0, yq1), col = "red", lwd = 2, lty = 2)
lines(c(q3, q3), c(0, yq3), col = "red", lwd = 2, lty = 2)
text(q1, yq1 + 10000, paste0('Q1=', round(q1, 2)), col = "red", cex = 1.2)
text(q3, yq3 + 27000, paste0('Q3=', round(q3, 2)), col = "red", cex = 1.2)
dev.off()



# 11. 再次绘制分位数函数，标注Q1和Q3
png("Output/Lec2/babyweight_percentiles_Q.png", width=500, height=500)

# 百分数分位数函数图（Q1、Q3标注）
p_seq <- seq(0, 1, length.out=200)
x_seq <- qnorm(p_seq, birth.mean, birth.sd)
plot(p_seq*100, x_seq, type="l", lwd=3, col="blue", xlab="Percentile (%)", ylab="Birthweight (gms)", main="Quantile Function (Inverse CDF)", cex.axis=1.2)
# 标注均值和常用分位点
abline(h=birth.mean, col="red", lty=2)
text(60, birth.mean+50, paste0('mean=', round(birth.mean, 2)), col="red", cex=1)
q1 <- qnorm(0.25, birth.mean, birth.sd)
q3 <- qnorm(0.75, birth.mean, birth.sd)
abline(h=q1, col="purple", lty=3)
abline(h=q3, col="orange", lty=3)
text(25, q1+50, paste0("25%:", round(q1, 2)), col="purple", cex=1)
text(75, q3+50, paste0("75%:", round(q3, 2)), col="orange", cex=1)
dev.off()