# 将工作路径设置为现文件夹所在目录
setwd("D:/GitHub/DataAnalysisIntro(Public)/")

# 安装HistData包（如果尚未安装）
if (!require(alr4)) install.packages("alr4")

library(alr4)
data(jevons)  # 加载jevons数据集

# 查看数据结构
str(jevons)
# ---- 以Age为横坐标，画近似box plot（竖着画，每列为一组） ----
# 使用jevons$SD直接作为标准差，Age -1 和 5留空白，放大字体，保存图片
jevons$Weight <- as.numeric(jevons$Weight)
jevons$SD <- as.numeric(jevons$SD)

# Age=1和Age=5两侧加边框空白
ages_valid <- sort(unique(jevons$Age))
ages_all <- c("", ages_valid, "")
means <- rep(NA, length(ages_all))
sd <- rep(NA, length(ages_all))
minw <- rep(NA, length(ages_all))
maxw <- rep(NA, length(ages_all))
for(i in seq_along(ages_valid)){
	idx <- which(jevons$Age == ages_valid[i])
	if(length(idx) == 1) {
		means[i+1] <- jevons$Weight[idx]
		sd[i+1] <- jevons$SD[idx]
		if("Min" %in% names(jevons)) minw[i+1] <- jevons$Min[idx] else minw[i+1] <- means[i+1]
		if("Max" %in% names(jevons)) maxw[i+1] <- jevons$Max[idx] else maxw[i+1] <- means[i+1]
	}
}
mid <- seq_along(ages_all)

# 保存图片
png('Output/Lec2/jevons_boxplot.png', width=1800, height=1200, res=180)
par(mar=c(7,7,5,2))
plot(mid, means, ylim=range(c(minw, maxw, means-sd, means+sd), na.rm=TRUE), xaxt='n', xlab='Age', ylab='Weight', main="'Box' Plot by Age", cex.lab=1.8, cex.main=1.8, type='n', cex.axis=1.8)
	axis(1, at=mid, labels=ages_all, cex.axis=1.8, padj=0.5)
	axis(2, cex.axis=1.8)
	for(i in seq_along(mid)){
		if(!is.na(means[i])){
			# 竖线：min-max
			segments(mid[i], minw[i], mid[i], maxw[i], lwd=4, col='gray40')
			# box：mean±1.96SD
			rect(mid[i]-0.2, means[i]-sd[i], mid[i]+0.2, means[i]+sd[i], col=rgb(0.2,0.6,1,0.3), border='blue', lwd=4)
			# caps
			segments(mid[i]-0.15, minw[i], mid[i]+0.15, minw[i], lwd=4, col='gray40')
			segments(mid[i]-0.15, maxw[i], mid[i]+0.15, maxw[i], lwd=4, col='gray40')
			# 均值点
			points(mid[i], means[i], pch=19, cex=1.8, col='red')
		}
	}

dev.off()

# 储存barplot图片
png('Output/Lec2/jevons_barplot.png', width=2000, height=1200, res=180)
barplot(jevons$n, names.arg=jevons$Age, main="Bar Plot of Age (by n)", col="skyblue", xlab="Age", ylab="Count", cex.axis=1.8, cex.names=1.8, cex.lab=1.8, cex.main=2.2)
dev.off()

# Pie chart (for categorical variable)，增大标签和标题字体
png('Output/Lec2/jevons_piechart.png', width=2000, height=1200, res=180)
pie(jevons$n, labels=jevons$Age,  col=rainbow(length(jevons$Age)), cex=1.8)
title(main="Pie Chart of Age (by n)", cex.main=2.2)
dev.off()
