# plots for slides 13 18 36: density for discrete random variables
# Output: bernoulli_pmf.png  binomial_bar_plot.png poisson_bar_plot.png
# optional plot: point plot for the PMF 


library(ggplot2)
library(dplyr)
library(tidyr)

setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")


#Bernoulli

p <- 0.6

# 创建数据框：定义所有可能的取值 (0 和 1) 及其对应的概率
bernoulli_data <- data.frame(
  x = c(0, 1),                # 随机变量的可能取值
  pmf = c(1-p, p)             # 对应的概率质量
)

# 创建图形
ggplot(bernoulli_data, aes(x = factor(x), y = pmf)) +
  # 绘制柱状图（条形图），stat = "identity"表示直接使用提供的y值
  geom_col(fill = "#3498db", width = 0.4, alpha = 0.8, color = "black") +
  # 在每个柱子上方添加概率值标签
  geom_text(aes(label = sprintf("%.2f", pmf)), vjust = -0.5, size = 5) +
  # 设置标题和坐标轴标签
  labs(
    title = paste("伯努利分布的概率质量函数 ( p =", p, ")"),
    x = "随机变量 X",
    y = "概率 P(X = x)"
  ) +
  # 调整y轴范围，为顶部的标签留出空间
  ylim(0, 1.1 * max(bernoulli_data$pmf)) +
  # 使用经典主题（看起来更简洁）
  theme_classic(base_size = 14) +
  # 可以进一步自定义主题
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # 标题居中并加粗
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# 将图形保存为PNG文件（高分辨率）
ggsave("./Output/Lec3/bernoulli_pmf.png", width = 6, height = 4.5, dpi = 300)




################ Binomial ####################

params <- list(
  "n=20, p=0.5" = c(n = 20, p = 0.5),
  "n=20, p=0.7" = c(n = 20, p = 0.7),
  "n=40, p=0.5" = c(n = 40, p = 0.5)
)

# 计算每种情况的概率质量函数
binom_data <- lapply(names(params), function(param_name) {
  n <- params[[param_name]]["n"]
  p <- params[[param_name]]["p"]
  
  data.frame(
    x = 0:n,
    pmf = dbinom(0:n, size = n, prob = p),
    Parameters = param_name
  )
}) %>% bind_rows()

colors <- c("#E69F00", "#56B4E9", "#009E73") # 美观的配色

# 绘制点图
point_plot <- ggplot(binom_data, aes(x = x, y = pmf, color = Parameters, shape = Parameters)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line(alpha = 0.6, linewidth = 0.7) +
  
  # 设置颜色和形状
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(16, 17, 15)) + # 不同的点形状
  
  # 标题和标签
  labs(
    title = "二项分布概率质量函数 - 点图",
    x = "成功次数 (k)",
    y = "概率 P(X = k)",
    color = "参数",
    shape = "参数"
  ) +
  
  # 主题美化
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

# 显示点图
print(point_plot)

# 保存点图
ggsave("./Output/Lec3/binomial_point_plot.png", point_plot, width = 10, height = 6, dpi = 300)


all_x_values <- 0:40

# 为每个参数创建完整的数据（缺失值设为0）
uniform_data <- lapply(names(params), function(param_name) {
  n <- params[[param_name]]["n"]
  p <- params[[param_name]]["p"]
  
  data.frame(
    x = all_x_values,
    pmf = ifelse(all_x_values <= n, 
                 dbinom(all_x_values, size = n, prob = p), 
                 0),
    Parameters = param_name
  )
}) %>% bind_rows()

# 使用统一数据绘制柱状图
bar_plot <- ggplot(uniform_data, aes(x = x, y = pmf, fill = Parameters)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  scale_fill_manual(values = colors) +
  labs(
    title = "二项分布概率质量函数",
    x = "成功次数 (k)",
    y = "概率 P(X = k)"
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
  scale_x_continuous(breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0.02, 0.05)))  # 左边留2%空隙，右边留5%空隙

print(bar_plot)

ggsave("./Output/Lec3/binomial_bar_plot.png", bar_plot, width = 10, height = 5, dpi = 300,bg = "white")





########################### Poisson ########################
# 定义三种情况的λ参数
lambdas <- c(1, 4, 10)
param_names <- c("λ = 1", "λ = 4", "λ = 10")
lambda_levels <- c("λ = 1", "λ = 4", "λ = 10")
# 计算每种情况的概率质量函数
poisson_data <- map2_df(lambdas, param_names, ~ {
  # 确定x的范围：0到3倍λ（覆盖主要概率质量）
  # x_max <- ceiling(3 * .x)
  x_max <- 20
  x_vals <- 0:x_max
  
  data.frame(
    x = x_vals,
    pmf = dpois(x_vals, lambda = .x),
    Lambda = factor(.y, levels = lambda_levels),
    Lambda_value = .x
  )
})

# 查看数据结构
head(poisson_data)
# 定义美观的颜色方案
poisson_colors <- c("#FF6B6B", "purple", "lightblue") # 红、青绿、蓝色

# 绘制点图+线图
poisson_point_plot <- ggplot(poisson_data, aes(x = x, y = pmf, color = Lambda, 
                                               group = Lambda)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  
  # 设置颜色
  scale_color_manual(values = poisson_colors) +
  
  # 标题和标签
  labs(
    title = "泊松分布概率质量函数",
    x = "事件发生次数 (k)",
    y = "概率 P(X = k)",
    color = "参数λ"
  ) +
  
  # 调整x轴刻度
  scale_x_continuous(breaks = function(x) seq(0, max(x), by = 2)) +
  
  # 主题美化
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# 显示图形
print(poisson_point_plot)

# 保存图形
ggsave("./Output/Lec3/poisson_pmf_point_plot.png", plot = poisson_point_plot, 
       width = 10, height = 7, dpi = 300, bg = "white")



# 使用统一数据绘制柱状图
bar_plot <- ggplot(poisson_data, aes(x = x, y = pmf, fill = Lambda)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  scale_fill_manual(values = poisson_colors) +
  labs(
    title = "泊松分布概率质量函数",
    x = "事件发生次数 (k)",
    y = "概率 P(X = k)",
    fill = "参数λ"
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
  scale_x_continuous(breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0.02, 0.05)))  # 左边留2%空隙，右边留5%空隙


bar_plot
ggsave("./Output/Lec3/poisson_bar_plot.png", bar_plot, width = 7, height = 5, dpi = 300,bg = "white")






