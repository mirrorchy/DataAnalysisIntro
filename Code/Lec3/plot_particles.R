# plot for slide 34: number of particles arrived in a given time interval
# Output: Particles_poisson.png
# data from Cramér, H. (1945). Mathematical Methods of Statistics. Uppsala and Princeton.
# see also: An Introduction to Probability Theory and Its Applications, Vol. 1, 3rd Edition by William Feller

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")

Nk = c(57,203,383,525,532,408,273,139,45,27,16  )
k  = 0:10


total_obs <- sum(Nk)  # 总观测数
mean_val <- round(sum(k * Nk) / total_obs,2)
var_val <- round(sum(Nk * (k - mean_val)^2) / (total_obs - 1) ,2)




# 创建数据框（ggplot2需要使用数据框格式）
plot_data <- data.frame(
  Num_of_particle = k,
  Observed_Freq = Nk
)

x_labels <- 0:10
x_labels[length(x_labels)] <- "≥10"  # 将最后一个标签改为≥10




p = ggplot(plot_data, aes(x = Num_of_particle,y = Observed_Freq)) +
  geom_col(  fill = "mediumaquamarine",          # 填充色
    color = "white",             # 边框色
    alpha = 0.7                  # 透明度（0-1）
  ) +
  # geom_density(color = "red", linewidth = 1.2) + # 添加密度曲线
  labs(
    title = "到达指定区域的粒子数",
    x = "到达粒子数",
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
  scale_x_continuous(
    # breaks = scales::pretty_breaks(),
                     breaks = 0:10,
                     labels = x_labels,
                     expand = expansion(mult = c(0.02, 0.05)))  # 左边留2%空隙，右边留5%空隙

p



p_all = p + 
  annotate("label", 
           x = max(k)  * 0.75, 
           y = max(Nk) * 0.9,
           label = paste0("均值 = ", mean_val, "\n方差 = ", var_val),
           hjust = 0, vjust = 1,
           fill = "white", alpha = 0.8,
           color = "black", size = 5,
           fontface = "bold")

p_all #显示图片
ggsave("./Output/Lec3/Particles_poisson.png",p_all,width = 6,height = 4,dpi = 300,bg = "white")



