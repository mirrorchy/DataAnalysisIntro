# For slide p14: PRES vs DEWP vs TEMP scatter plots - Beijing 2015 vs 2023
# Output: pm25_weather_scatter_DEWP_vs_PRES_2015_2023.png, pm25_weather_scatter_DEWP_vs_TEMP_2015_2023.png, pm25_weather_scatter_PRES_vs_TEMP_2015_2023.png, pm25_weather_density_DEWP_PRES_TEMP_2015_2023.png

# 将工作路径设置为现文件夹所在目录
setwd("D:/GitHub/DataAnalysisIntro(Public)/")

df <- read.csv("Input/Lec2/Beijing_Aotizhongxin.csv", header = TRUE)

n_col <- 200

# 筛选出冬季
d<- df[df$season == 4, ]

# 并排画2015和2023年，y轴共用气压，x轴共用露点温度
png(filename="Output/Lec2/pm25_weather_scatter_DEWP_vs_PRES_2015_2023.png", width=1600, height=900, res=180)
par(mfrow=c(1,2), oma=c(4,2,0,6), mar=c(4,4,2,1))


# 2015和2023数据
df2015 <- df[df$year==2015 & !is.na(df$DEWP) & !is.na(df$PRES) & !is.na(df$PM2.5), c('DEWP','PRES','PM2.5')]
df2023 <- df[df$year==2023 & !is.na(df$DEWP) & !is.na(df$PRES) & !is.na(df$PM2.5), c('DEWP','PRES','PM2.5')]
all_pm25 <- c(df2015$PM2.5, df2023$PM2.5)
n_col <- 200
colormap <- colorRampPalette(c('green','yellow','orange','red'))(n_col)
pm25_breaks <- seq(min(all_pm25), max(all_pm25), length.out = n_col + 1)
col_idx1 <- as.numeric(cut(df2015$PM2.5, breaks=pm25_breaks, include.lowest=TRUE))
col_idx2 <- as.numeric(cut(df2023$PM2.5, breaks=pm25_breaks, include.lowest=TRUE))
plot(df2015$DEWP, df2015$PRES, col=colormap[col_idx1], pch=19, cex=0.28, xlab="", ylab="", main="2015", cex.main=1, cex.lab=0.9, cex.axis=0.8)
plot(df2023$DEWP, df2023$PRES, col=colormap[col_idx2], pch=19, cex=0.28, xlab="", ylab="", main="2023", cex.main=1, cex.lab=0.9, cex.axis=0.8)



# 添加连续光谱图例（右侧）
par(new=TRUE, plt=c(0.92,0.96,0.2,0.8), las=1, cex.axis=0.8)
all_pm25 <- c(df2015$PM2.5, df2023$PM2.5)
image(1, seq(min(all_pm25), max(all_pm25), length.out=n_col), matrix(seq(n_col), nrow=1), col=colormap, axes=FALSE, xlab="", ylab="")
axis(4, at=seq(min(all_pm25), max(all_pm25), length.out=5), labels=round(seq(min(all_pm25), max(all_pm25), length.out=5),1))
mtext("PM2.5", side=4, line=2.5, cex=0.9, outer=TRUE)
dev.off()


# 并排画2015和2023年 DEWP vs TEMP（露点温度 vs 气温）
png(filename="Output/Lec2/pm25_weather_scatter_DEWP_vs_TEMP_2015_2023.png", width=1600, height=900, res=180)
par(mfrow=c(1,2), oma=c(4,2,0,6), mar=c(4,4,2,1))

# 2015和2023数据
df2015_temp <- df[df$year==2015 & !is.na(df$DEWP) & !is.na(df$TEMP) & !is.na(df$PM2.5), c('DEWP','TEMP','PM2.5')]
df2023_temp <- df[df$year==2023 & !is.na(df$DEWP) & !is.na(df$TEMP) & !is.na(df$PM2.5), c('DEWP','TEMP','PM2.5')]
all_pm25_temp <- c(df2015_temp$PM2.5, df2023_temp$PM2.5)
colormap_temp <- colorRampPalette(c('green','yellow','orange','red'))(n_col)
pm25_breaks_temp <- seq(min(all_pm25_temp), max(all_pm25_temp), length.out = n_col + 1)
col_idx1_temp <- as.numeric(cut(df2015_temp$PM2.5, breaks=pm25_breaks_temp, include.lowest=TRUE))
col_idx2_temp <- as.numeric(cut(df2023_temp$PM2.5, breaks=pm25_breaks_temp, include.lowest=TRUE))
plot(df2015_temp$DEWP, df2015_temp$TEMP, col=colormap_temp[col_idx1_temp], pch=19, cex=0.28, xlab="", ylab="", main="2015", cex.main=1, cex.lab=0.9, cex.axis=0.8)
plot(df2023_temp$DEWP, df2023_temp$TEMP, col=colormap_temp[col_idx2_temp], pch=19, cex=0.28, xlab="", ylab="", main="2023", cex.main=1, cex.lab=0.9, cex.axis=0.8)


# 添加连续光谱图例（右侧）
par(new=TRUE, plt=c(0.92,0.96,0.2,0.8), las=1, cex.axis=0.8)
image(1, seq(min(all_pm25_temp), max(all_pm25_temp), length.out=n_col), matrix(seq(n_col), nrow=1), col=colormap_temp, axes=FALSE, xlab="", ylab="")
axis(4, at=seq(min(all_pm25_temp), max(all_pm25_temp), length.out=5), labels=round(seq(min(all_pm25_temp), max(all_pm25_temp), length.out=5),1))
mtext("PM2.5", side=4, line=2.5, cex=0.9, outer=TRUE)
dev.off()

## 并排画2015和2023年 PRES vs TEMP（气压 vs 气温）
png(filename="Output/Lec2/pm25_weather_scatter_PRES_vs_TEMP_2015_2023.png", width=1600, height=900, res=180)
par(mfrow=c(1,2), oma=c(4,2,0,6), mar=c(4,4,2,1))

# 2015和2023数据
df2015_pres <- df[df$year==2015 & !is.na(df$PRES) & !is.na(df$TEMP) & !is.na(df$PM2.5), c('PRES','TEMP','PM2.5')]
df2023_pres <- df[df$year==2023 & !is.na(df$PRES) & !is.na(df$TEMP) & !is.na(df$PM2.5), c('PRES','TEMP','PM2.5')]
all_pm25_pres <- c(df2015_pres$PM2.5, df2023_pres$PM2.5)
colormap_pres <- colorRampPalette(c('green','yellow','orange','red'))(n_col)
pm25_breaks_pres <- seq(min(all_pm25_pres), max(all_pm25_pres), length.out = n_col + 1)
col_idx1_pres <- as.numeric(cut(df2015_pres$PM2.5, breaks=pm25_breaks_pres, include.lowest=TRUE))
col_idx2_pres <- as.numeric(cut(df2023_pres$PM2.5, breaks=pm25_breaks_pres, include.lowest=TRUE))
plot(df2015_pres$PRES, df2015_pres$TEMP, col=colormap_pres[col_idx1_pres], pch=19, cex=0.28, xlab="", ylab="", main="2015", cex.main=1, cex.lab=0.9, cex.axis=0.8)
plot(df2023_pres$PRES, df2023_pres$TEMP, col=colormap_pres[col_idx2_pres], pch=19, cex=0.28, xlab="", ylab="", main="2023", cex.main=1, cex.lab=0.9, cex.axis=0.8)

# 添加连续光谱图例（右侧）
par(new=TRUE, plt=c(0.92,0.96,0.2,0.8), las=1, cex.axis=0.8)
image(1, seq(min(all_pm25_pres), max(all_pm25_pres), length.out=n_col), matrix(seq(n_col), nrow=1), col=colormap_pres, axes=FALSE, xlab="", ylab="")
axis(4, at=seq(min(all_pm25_pres), max(all_pm25_pres), length.out=5), labels=round(seq(min(all_pm25_pres), max(all_pm25_pres), length.out=5),1))
mtext("PM2.5", side=4, line=2.5, cex=0.9, outer=TRUE)
dev.off()



# 画出DEWP PRES TEMP分布密度（2015和2023两条线）

png(filename="Output/Lec2/pm25_weather_density_DEWP_PRES_TEMP_2015_2023.png", width=1200, height=900, res=180)
par(mfrow=c(2,2), oma=c(2,2,2,2), mar=c(5,5,3,1))

# 取2015和2023数据
df2015_all <- df[df$year==2015, ]
df2023_all <- df[df$year==2023, ]


# 1. DEWP
dens_2015_dewp <- density(na.omit(df2015_all$DEWP))
dens_2023_dewp <- density(na.omit(df2023_all$DEWP))
plot(dens_2015_dewp, col="forestgreen", lwd=2, main="DEWP", xlab="", ylab="", cex.main=1, cex.lab=1, cex.axis=0.9)
lines(dens_2023_dewp, col="red", lwd=2, lty=1)

# 2. PRES
dens_2015_pres <- density(na.omit(df2015_all$PRES))
dens_2023_pres <- density(na.omit(df2023_all$PRES))
plot(dens_2015_pres, col="forestgreen", lwd=2, main="PRES", xlab="", ylab="", cex.main=1, cex.lab=1, cex.axis=0.9)
lines(dens_2023_pres, col="red", lwd=2, lty=1)

# 3. TEMP
dens_2015_temp <- density(na.omit(df2015_all$TEMP))
dens_2023_temp <- density(na.omit(df2023_all$TEMP))
plot(dens_2015_temp, col="forestgreen", lwd=2, main="TEMP", xlab="", ylab="", cex.main=1, cex.lab=1, cex.axis=0.9)
lines(dens_2023_temp, col="red", lwd=2, lty=1)

plot.new()
legend("center", legend=c("2015", "2023"), col=c("forestgreen", "red"), lwd=2, lty=1, bty="n", cex=1.3)
dev.off()