# For slides p16 p27: scatter plots comparing PM2.5 with PRES and DEWP in Beijing, 2015 vs 2023
# Output: pm25_PM25_cmp_scatter_8plots_PM25_vs_PRES.png, pm25_PM25_cmp_scatter_8plots_PM25_vs_DEWP.png


# 将工作路径设置为现文件夹所在目录
setwd("D:/GitHub/DataAnalysisIntro(Public)/")

df <- read.csv("Input/Lec2/Beijing_Aotizhongxin.csv", header = TRUE)

# 筛选2015和2023年第四季度PM2.5，PRES，DEWP
subset_df_2015 <- df[df$year == 2015,c('season','PM2.5','PRES','DEWP')]
subset_df_2023 <- df[df$year == 2023, c('season','PM2.5','PRES','DEWP')]



# 计算每一列（同季度）2015和2023的联合坐标轴范围
get_col_axis_limits <- function(season, df1, df2, xvar, yvar) {
  x_all <- c(df1[df1$season==season, xvar], df2[df2$season==season, xvar])
  y_all <- c(df1[df1$season==season, yvar], df2[df2$season==season, yvar])
  xlim <- range(x_all, na.rm=TRUE)
  ylim <- range(y_all, na.rm=TRUE)
  list(xlim=xlim, ylim=ylim)
}

# PM2.5 vs PRES
png(filename="Output/Lec2/pm25_PM25_cmp_scatter_8plots_PM25_vs_PRES.png", width=2400, height=1200, res=180)
par(mfrow=c(2,4), mar=c(5,5,4,2))
for (s in 1:4) {
  axis_limits <- get_col_axis_limits(s, subset_df_2015, subset_df_2023, 'PRES', 'PM2.5')
  for (year in c(2015, 2023)) {
    if (year == 2015) {
      subset <- subset_df_2015[subset_df_2015$season == s & !is.na(subset_df_2015$PM2.5) & !is.na(subset_df_2015$PRES), ]
      main_title <- paste0("2015 Q", s)
    } else {
      subset <- subset_df_2023[subset_df_2023$season == s & !is.na(subset_df_2023$PM2.5) & !is.na(subset_df_2023$PRES), ]
      main_title <- paste0("2023 Q", s)
    }
    plot(subset$PRES, subset$PM2.5,
         main=main_title,
         xlab="PRES (Pa)", ylab="PM2.5",
         pch=19, col=rgb(ifelse(year==2015,0,1),0,ifelse(year==2015,1,0),0.3),
         xlim=axis_limits$xlim, ylim=axis_limits$ylim)
    if (nrow(subset) > 3) {
      # 使用kernel regression (LOESS)
      fit <- loess(PM2.5 ~ PRES, data=subset, span=0.7)
      x_seq <- seq(min(subset$PRES, na.rm=TRUE), max(subset$PRES, na.rm=TRUE), length.out=200)
      y_pred <- predict(fit, newdata=data.frame(PRES=x_seq))
      lines(x_seq, y_pred, col="black", lwd=2)
    }
  }
}
dev.off()

# PM2.5 vs DEWP
png(filename="Output/Lec2/pm25_PM25_cmp_scatter_8plots_PM25_vs_DEWP.png", width=2400, height=1200, res=180)
par(mfrow=c(2,4), mar=c(5,5,4,2))
for (s in 1:4) {
  axis_limits <- get_col_axis_limits(s, subset_df_2015, subset_df_2023, 'DEWP', 'PM2.5')
  for (year in c(2015, 2023)) {
    if (year == 2015) {
      subset <- subset_df_2015[subset_df_2015$season == s & !is.na(subset_df_2015$PM2.5) & !is.na(subset_df_2015$DEWP), ]
      main_title <- paste0("2015 Q", s)
    } else {
      subset <- subset_df_2023[subset_df_2023$season == s & !is.na(subset_df_2023$PM2.5) & !is.na(subset_df_2023$DEWP), ]
      main_title <- paste0("2023 Q", s)
    }
    plot(subset$DEWP, subset$PM2.5,
         main=main_title,
         xlab="DEWP (Pa)", ylab="PM2.5",
         pch=19, col=rgb(ifelse(year==2015,0,1),0,ifelse(year==2015,1,0),0.3),
         xlim=axis_limits$xlim, ylim=axis_limits$ylim)
    if (nrow(subset) > 3) {
      # 使用kernel regression (LOESS)
      fit <- loess(PM2.5 ~ DEWP, data=subset, span=0.7)
      x_seq <- seq(min(subset$DEWP, na.rm=TRUE), max(subset$DEWP, na.rm=TRUE), length.out=200)
      y_pred <- predict(fit, newdata=data.frame(DEWP=x_seq))
      lines(x_seq, y_pred, col="black", lwd=2)
    }
  }
}
dev.off()