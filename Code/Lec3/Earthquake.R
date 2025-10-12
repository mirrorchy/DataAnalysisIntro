# plot for slide 42 43 61: weekly earthquake in China
# data collected from https://data.earthquake.cn/datashare/report.shtml?PAGEID=earthquake_subao
# Output: Earthquake_2015_2024_above4level.png Earthquake_2015_2024_above3level.png
# Earthquake_2015_2024_gaptime.png



library(tidyverse)  # %>% dplyr ggplot2
library(readxl)
library(cowplot)
library(ggrepel)
library(scales)
library(dplyr)
library(reshape2)

rm(list = ls())

# 更改当前工作目录
setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")

#  读取数据
alldata <- read_excel("./Input/Lec3/earthquake_2015_2024.xlsx")  # 请替换为实际文件路径
colnames(alldata) <- c("id", "time", "lon", "lat", "center_depth", "level", "location","type")

alldata$level = as.numeric(alldata$level)


############################ 4级以上 ############################
# select data
data = alldata[alldata$level >= 4,] # 选取4级以上的地震

# 数据预处理
# 将时间列转换为POSIXct格式
data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")

data = data[order(data$time),]

# 提取日期（去掉时分秒）
data$date <- format(data$time,format = "%Y-%m")

# 每周的地震次数
weekly_counts <- data %>%
  mutate(
    year = year(time),          # 提取年份
    week = week(time)           # 提取周数（一年中的第几周）
  ) %>%
  group_by(year, week) %>%      # 按年和周分组
  summarise(
    number = n(),               # 每周地震次数
    start_date = min(date(time)), # 每周开始日期
    .groups = 'drop'
  ) %>%
  mutate(
    week_label = paste0(year, "-W", sprintf("%02d", week))  # 创建周标签
  )


quantile_num <- quantile(weekly_counts$number, probs = 0.99)


# 清洗数据
weekly_counts <- weekly_counts[weekly_counts$number <= quantile_num, ]



lambda_weekly <- mean(weekly_counts$number)
mean(weekly_counts$number)
var(weekly_counts$number)


#  绘制每周地震次数的直方图与泊松分布拟合曲线
x_values_weekly <- 0:max(weekly_counts$number)
theoretical_freq_weekly <- dpois(x_values_weekly, lambda_weekly) * nrow(weekly_counts)
theory_df_weekly <- data.frame(num = x_values_weekly, freq = theoretical_freq_weekly)




# 创建图形
weekly_plot <- ggplot(weekly_counts, aes(x = number)) +
  geom_histogram(
    aes(y = after_stat(count)),  # 使用计数而不是密度
    binwidth = 1,                # 每个整数一个柱子
    fill = "orange",          
    color = "white",             
    alpha = 0.7                  
  ) +
  labs(
    title = "Weekly Earthquake 周地震(4级以上)次数",
    subtitle = "2015-2024年 中国",
    x = "每周地震次数", 
    y = "频数"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # 坐标轴线设置
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.8),  # 单独设置y轴
    
    # 坐标轴刻度线
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    
    # 确保移除其他边框
    panel.border = element_blank(),
    
    # 其他样式设置保持不变
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))  # 左边留2%空隙，右边留5%空隙


mean_val <- round(mean(weekly_counts$number), 2)
var_val <- round(var(weekly_counts$number), 2)

plot_data <- ggplot_build(weekly_plot)$data[[1]]
max_count <- max(plot_data$count)



weekly_plot_all = weekly_plot + 
  annotate("label", 
           x = max(weekly_counts$number)  * 0.75, 
           y = max_count * 0.9,
           label = paste0("均值 = ", mean_val, "\n方差 = ", var_val),
           hjust = 0, vjust = 1,
           fill = "white", alpha = 0.8,
           color = "black", size = 5,
           fontface = "bold")

print(weekly_plot_all)

ggsave("./Output/Lec3/Earthquake_2015_2024_above4level.png",weekly_plot,width = 6,height = 4,dpi = 300,bg = "white")


# 画图  相邻地震间隔时间是否符合指数分布
# 指数分布的参数rate为间隔时间的平均值的倒数

# 计算相邻地震的时间间隔（单位：周）
data$gap_week <- c(NA, diff(data$time)) / (60*60*24*7)  # 转换为小时，第一行为NA

# 移除NA值
intervals <- na.omit(data$gap_week)

rate <- 1 / mean(intervals)


# 先计算密度值
density_data <- density(intervals,from = 0)
density_df <- data.frame(x = density_data$x, y = density_data$y)

gap_time_simple <- ggplot() +
  geom_line(data = density_df, aes(x = x, y = y, color = "经验密度函数"), 
            linewidth = 1.2) +
  stat_function(aes(color = "指数分布拟合"), 
                fun = function(x) dexp(x, rate = rate), 
                size = 1.2) +
  labs(title = "地震间隔时间", 
       x = "间隔时间（周）", 
       y = "密度",
       subtitle = paste0("拟合指数分布 λ = ", round(rate, 3)),
       color = "分布类型") +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_color_manual(
    values = c("经验密度函数" = "green", "指数分布拟合" = "red")
  )

gap_time_simple
ggsave("./Output/Lec3/Earthquake_2015_2024_gaptime.png",gap_time_simple,width = 6,height = 4,dpi = 300,bg = "white")


############################ 3级以上 ############################
# select data
data = alldata[alldata$level >= 3,] # 选取4级以上的地震

# 数据预处理
# 将时间列转换为POSIXct格式
data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")

data = data[order(data$time),]

# 提取日期（去掉时分秒）
data$date <- format(data$time,format = "%Y-%m")

# 每周的地震次数
weekly_counts <- data %>%
  mutate(
    year = year(time),          # 提取年份
    week = week(time)           # 提取周数（一年中的第几周）
  ) %>%
  group_by(year, week) %>%      # 按年和周分组
  summarise(
    number = n(),               # 每周地震次数
    start_date = min(date(time)), # 每周开始日期
    .groups = 'drop'
  ) %>%
  mutate(
    week_label = paste0(year, "-W", sprintf("%02d", week))  # 创建周标签
  )


quantile_num <- quantile(weekly_counts$number, probs = 0.95)


# 清洗数据
weekly_counts <- weekly_counts[weekly_counts$number <= quantile_num, ]



lambda_weekly <- mean(weekly_counts$number)
mean(weekly_counts$number)
var(weekly_counts$number)


#  绘制每周地震次数的直方图与泊松分布拟合曲线
x_values_weekly <- 0:max(weekly_counts$number)
theoretical_freq_weekly <- dpois(x_values_weekly, lambda_weekly) * nrow(weekly_counts)
theory_df_weekly <- data.frame(num = x_values_weekly, freq = theoretical_freq_weekly)




# 创建图形
weekly_plot <- ggplot(weekly_counts, aes(x = number)) +
  geom_histogram(
    aes(y = after_stat(count)),  # 使用计数而不是密度
    binwidth = 1,                # 每个整数一个柱子
    fill = "orange",          
    color = "white",             
    alpha = 0.7                  
  ) +
  labs(
    title = "Weekly Earthquake 周地震(3级以上)次数",
    subtitle = "2015-2024年 中国",
    x = "每周地震次数", 
    y = "频数"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # 坐标轴线设置
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.8),  # 单独设置y轴
    
    # 坐标轴刻度线
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    
    # 确保移除其他边框
    panel.border = element_blank(),
    
    # 其他样式设置保持不变
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))  # 左边留2%空隙，右边留5%空隙


mean_val <- round(mean(weekly_counts$number), 2)
var_val <- round(var(weekly_counts$number), 2)

plot_data <- ggplot_build(weekly_plot)$data[[1]]
max_count <- max(plot_data$count)



weekly_plot_all = weekly_plot + 
  annotate("label", 
           x = max(weekly_counts$number)  * 0.75, 
           y = max_count * 0.9,
           label = paste0("均值 = ", mean_val, "\n方差 = ", var_val),
           hjust = 0, vjust = 1,
           fill = "white", alpha = 0.8,
           color = "black", size = 5,
           fontface = "bold")

print(weekly_plot_all)

ggsave("./Output/Lec3/Earthquake_2015_2024_above3level.png",weekly_plot,width = 6,height = 4,dpi = 300,bg = "white")


