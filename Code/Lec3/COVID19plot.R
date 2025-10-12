# plot for slide 41: COVID-19 daily increment in 2021 spr
# data from OWID
# Output: COVID19_2021spr_daily_increment.png


library(ggplot2)

rm(list = ls())

setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")


mydata = read.csv("./Input/Lec3/owid-covid-data.csv",fileEncoding = 'UTF-8')
mydata = mydata[mydata$location == "China",]

data = data.frame(cbind(date = mydata$date, incre = mydata$new_cases))

data$incre[is.na(data$incre)] = 0

data$incre = round(as.numeric(data$incre),0)

mydata_2021 = data[data$date >= "2021-03-01" & data$date <= "2021-05-31", ]

quantile_num <- quantile(mydata_2021$incre, probs = 0.95)

data_cleaned <- mydata_2021[mydata_2021$incre <= quantile_num, ]

plot_data = data_cleaned
mean_val <- round(mean(plot_data$incre), 2)
var_val <- round(var(plot_data$incre), 2)

p = ggplot(plot_data, aes(x = incre)) +
  geom_histogram(
    aes(y = after_stat(count)), 
    binwidth = 4,                # 设置柱子宽度（与bins二选一）
    # bins = 30,                 # 或者直接设置柱子数量
    fill = "red",          # 填充色
    color = "white",             # 边框色
    alpha = 0.7                  # 透明度（0-1）
  ) +
  # geom_density(color = "red", linewidth = 1.2) + # 添加密度曲线
  labs(
    title = "COVID-19 日新增感染数",
    subtitle = "2021年春季(3-5月) 中国",
    x = "新增感染数",
    y = "频数"
  ) +
  theme_classic() +              # 使用简洁的主题
  theme(
    # 坐标轴线设置
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.line.x = element_line(color = "black", linewidth = 0.6),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.6),  # 单独设置y轴
    
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


p


plot_object <- ggplot_build(p)$data[[1]]
max_count <- max(plot_object$count)



p_all = p + 
  annotate("label", 
           x = max(plot_data$incre)  * 0.75, 
           y = max_count * 0.9,
           label = paste0("均值 = ", mean_val, "\n方差 = ", var_val),
           hjust = 0, vjust = 1,
           fill = "white", alpha = 0.8,
           color = "black", size = 5,
           fontface = "bold")





p_all #显示图片
ggsave("./Output/Lec3/COVID19_2021spr_daily_increment.png",p_all,width = 6,height = 4,dpi = 300,bg = "white")



