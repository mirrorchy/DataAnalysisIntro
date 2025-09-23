# For slide p55: distribution of birthweight - Birthweight example
# Output: babyweight_overall_sd.png, babyweight_overall_percentile.png, babyweight_overall_density.png

# Example adapted from David Spiegelhalter's "The Art of Statistics"
# The data are for 1,096,277 full-term births to non-hispanic women in the United States for 2013, and are taken from Table 23 of [http://www.cdc.gov/nchs/data/nvsr/nvsr64/nvsr64_01.pdf](http://www.cdc.gov/nchs/data/nvsr/nvsr64/nvsr64_01.pdf),


# 设置工作目录，确保后续文件路径正确
setwd("D:/GitHub/DataAnalysisIntro(Public)/")

# --- 数据准备 --- 
# white
weights_w = c(1500, 2000, 2500, 3000, 3500, 4000, 4500,5000)
mids_w = weights_w + 250
n_w = c(5+48+308+1130, 12679, 124209, 442891, 389275, 108886, 14936,1345)
N_w = sum(n_w)
area_w = N_w * 500
birth.mean_w = sum(n_w * mids_w / N_w)
birth.sd_w = sqrt(sum(n_w * (mids_w - birth.mean_w)^2) / N_w)
xw = 2910 # low birthweight cutoff for whites

# black
weights_b = c(1500, 2000, 2500, 3000, 3500, 4000, 4500,5000)
mids_b = weights_b + 250
n_b = c(1+22+149+639, 6728, 54111, 119307, 63910, 12648, 1718,223)
N_b = sum(n_b)
area_b = N_b * 500
birth.mean_b = sum(n_b * mids_b / N_b)
birth.sd_b = sqrt(sum(n_b * (mids_b - birth.mean_b)^2) / N_b)


# 画图参数
xrange <- c(1500,5500)
# 统一y轴范围
yrange <- range(c(n_w, n_b, area_w*dnorm(birth.mean_w, birth.mean_w, birth.sd_w), area_b*dnorm(birth.mean_b, birth.mean_b, birth.sd_b), 0))
scale=2

par_mar = c(14,3,7,1)+0.1  
par_mgp = c(6,5,0)


# Mean +/- 1,2,3 SDs
png(filename = "Output/Lec2/babyweight_overall_sd.png", width = 2800, height = 1200)
par(mfrow=c(1,2))
par(mar=par_mar)
par(mgp=par_mgp)
par(xpd=TRUE)
I=-3:3
label=c("-3 SDs", "-2 SDs", "-1 SD", "mean", "+1 SD","+2 SDs", "+3 SDs")
bit=10000
xx=250
shift=c(-xx,-xx,-xx,0,xx,xx,xx)
plot(xrange, yrange, type = "n", xlab = "", ylab = "", bty="n",axes=F,main="White", cex=3.5, cex.main=5, cex.lab=4, cex.axis=4)
axis(1,cex.axis=4)
# 添加直方图
rect(weights_w, 0, weights_w + 500, n_w, col = "lightblue")
curve(area_w*dnorm(x, birth.mean_w, birth.sd_w), min(xrange), max(xrange), add = TRUE, lwd=5, col="blue")
x1=birth.mean_w+I*birth.sd_w
y1=area_w*dnorm(x1,birth.mean_w, birth.sd_w)
for(i in 1:7){
  lines(c(x1[i],x1[i]), c(0,y1[i]),lwd=4)
  text(x1[i]+shift[i],y1[i]+bit,label[i],cex=3)
}
lines(c(xw,xw),yrange,col="red",lwd=4)
# right: black
plot(xrange, yrange, type = "n", xlab = "", ylab = "", bty="n",axes=F,main="Black", cex=3.5, cex.main=5, cex.lab=4, cex.axis=4)
axis(1,cex.axis=4)
# 添加直方图
rect(weights_b, 0, weights_b + 500, n_b, col = "lightblue")
curve(area_b*dnorm(x, birth.mean_b, birth.sd_b), min(xrange), max(xrange), add = TRUE, lwd=5, col="blue")
x1=birth.mean_b+I*birth.sd_b
y1=area_b*dnorm(x1,birth.mean_b, birth.sd_b)
for(i in 1:7){
  lines(c(x1[i],x1[i]), c(0,y1[i]),lwd=4)
  text(x1[i]+shift[i],y1[i]+bit,label[i],cex=3)
}
lines(c(xw,xw),yrange,col="red",lwd=4)
dev.off()

# Percentiles
png(filename = "Output/Lec2/babyweight_overall_percentile.png", width = 2800, height = 1200)
par(mfrow=c(1,2))
par(mar=par_mar)
par(mgp=par_mgp)
par(xpd=TRUE)
I=c(1,5,25,50,75,95,99)
label=c("1%", "5%", "25%", "50%","75%","95%","99%")
bit=5000
plot(xrange, yrange, type = "n", xlab = "", ylab = "", bty="n",axes=F,main="White", cex=3.5, cex.main=5, cex.lab=4, cex.axis=4)
axis(1,cex.axis=4)
# 添加直方图
rect(weights_w, 0, weights_w + 500, n_w, col = "lightblue")
curve(area_w*dnorm(x, birth.mean_w, birth.sd_w), min(xrange), max(xrange), add = TRUE, lwd=5, col="blue")
x1=qnorm(I/100, birth.mean_w,birth.sd_w)
y1=area_w*dnorm(x1,birth.mean_w, birth.sd_w)
for(i in 1:7){
  lines(c(x1[i],x1[i]), c(0,y1[i]),lwd=4,lty=2)
  text(x1[i],-bit,label[i],cex=2.8)
}
lines(c(xw,xw),yrange,col="red",lwd=4)
# right: black
plot(xrange, yrange, type = "n", xlab = "", ylab = "", bty="n",axes=F,main="Black", cex=3.5, cex.main=5, cex.lab=4, cex.axis=4)
axis(1,cex.axis=4)
# 添加直方图
rect(weights_b, 0, weights_b + 500, n_b, col = "lightblue")
curve(area_b*dnorm(x, birth.mean_b, birth.sd_b), min(xrange), max(xrange), add = TRUE, lwd=5, col="blue")
x1=qnorm(I/100, birth.mean_b,birth.sd_b)
y1=area_b*dnorm(x1,birth.mean_b, birth.sd_b)
for(i in 1:7){
  lines(c(x1[i],x1[i]), c(0,y1[i]),lwd=4,lty=2)
  text(x1[i],-bit,label[i],cex=2.8)
}
lines(c(xw,xw),yrange,col="red",lwd=4)
dev.off()


# 同一张图中画出white和black的分布密度
png(filename = "Output/Lec2/babyweight_overall_density.png", width = 2800, height = 1200)

# 计算概率密度的y轴范围
x_seq <- seq(min(xrange), max(xrange), length.out=500)
y_white <- dnorm(x_seq, birth.mean_w, birth.sd_w)
y_black <- dnorm(x_seq, birth.mean_b, birth.sd_b)
yrange_density <- range(c(y_white, y_black), 0)
# 添加x轴坐标
plot(xrange, yrange_density, type = "n", xlab = "Birthweight (gms)", ylab = "Probability Density", bty="n", axes=F, main="White & Black Birthweight Density", cex.main=4, cex.lab=3, cex.axis=3)
axis(1, cex.axis=3)
# white概率密度
curve(dnorm(x, birth.mean_w, birth.sd_w), min(xrange), max(xrange), add = TRUE, lwd=6, col="blue")
# black概率密度
curve(dnorm(x, birth.mean_b, birth.sd_b), min(xrange), max(xrange), add = TRUE, lwd=6, col="orange")
legend("topright", legend=c("White", "Black"), col=c("blue", "orange"), lwd=6, cex=2.5, bty="n")
# 添加xw处的红色垂线
lines(c(xw, xw), yrange_density, col="red", lwd=4)
dev.off()
