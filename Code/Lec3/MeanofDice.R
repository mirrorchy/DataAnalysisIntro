# plot for CLT slides

# Output: MeanofDice.png  MeanofDice_standard.png 
# MeanofDice_sqrt.png  MeanofDice_sqrt_standard.png
# MeanofDice_sigmoid.png MeanofDice_sigmoid_standard.png


library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)

rm(list = ls())

setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")

#设置随机种子 保证结果可复现
set.seed(123)


n_sims <- 1000  # 模拟1000次
n_values <- c(5, 10, 20, 50)  # 不同的投掷次数n
results <- list()

for (n in n_values) {
  means <- numeric(n_sims)
  
  for (i in 1:n_sims) {
    rolls <- sample(1:6, size = n, replace = TRUE)
    means[i] <- mean(rolls)
  }
  
  results[[paste0("n =", n)]] <- means
}


df <- as.data.frame(results)
colnames(df) = paste0("n =", n_values)
df_long <- df %>% pivot_longer(cols = everything(), 
                               names_to = "n", 
                               values_to = "mean_value")
df_long$n<-factor(df_long$n, 
                  levels = paste0("n =", n_values))

# 绘制密度曲线图

unique_n <- unique(df_long$n)
n_values <- as.numeric(gsub("n =", "", unique_n)) 

theory_data <- map_dfr(n_values, ~ {
  sd_val <- sqrt((35/12)/.x)
  data.frame( n = paste0("n =", .x),
    x = seq(0, 7, length.out = 1000),
    y = dnorm(seq(0, 7, length.out = 1000), mean = 3.5, sd = sd_val) )
})

theory_data$n <- factor(theory_data$n, levels = paste0("n =", n_values))



theory_data$n <- factor(theory_data$n, levels = paste0("n =", n_values))

ggplot(df_long, aes(x = mean_value)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "red",
                 binwidth = 0.15, 
                 color = "white",
                 alpha = 0.5,
                 show.legend = FALSE) +
  geom_line(data = theory_data, aes(x = x, y = y), 
            color = "blue", 
            linewidth = 1.2,
            linetype = "dashed") +
  facet_wrap(~ n, ncol = 2, scales = "free_y") +  # 分面显示，2列
  labs(title = "投n次骰子，均值的分布",
       subtitle = "蓝色: 理论正态分布   红色: 模拟数据分布直方图",
       x = "样本均值", y = "密度（频率）") +
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
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))+  # 左边留2%空隙，右边留5%空隙
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))


ggsave("./Output/Lec3/MeanofDice.png",dpi = 300, width = 6,height = 4, bg = "white")



######## standardized Version, use population level parameter ######


df_long <- df %>% pivot_longer(cols = everything(), 
                               names_to = "n", 
                               values_to = "mean_value")
df_long$n <- factor(df_long$n, levels = paste0("n =", n_values))


df_standardized <- df_long %>%
  group_by(n) %>%
  mutate(
    sample_mean = 3.5,
    sample_sd = sqrt(35/12),
    standardized_value = sqrt(as.numeric(gsub("n =","",n ) )) * (mean_value - sample_mean) / sample_sd
  ) %>%
  ungroup()



x_min = min(df_standardized$standardized_value)
x_max = max(df_standardized$standardized_value)

x_lim = max(abs(x_min),x_max)


theory_data <- map_dfr(n_values, ~ {
  sd_val <- 1
  data.frame(
    n = paste0("n =", .x),
    x = seq(-x_lim, x_lim, length.out = 1000),
    y = dnorm(seq(-x_lim, x_lim, length.out = 1000), mean =0, sd = sd_val)
  )
})

theory_data$n <- factor(theory_data$n, levels = paste0("n =", n_values))

ggplot(df_standardized, aes(x = standardized_value)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "purple",
                 binwidth = 0.6, 
                 color = "white",
                 alpha = 0.5,
                 show.legend = FALSE) +
  geom_line(data = theory_data, aes(x = x, y = y), 
            color = "blue", 
            linewidth = 1.2,
            linetype = "dashed") +
  facet_wrap(~ n, ncol = 2, scales = "free_y") +  # 分面显示，2列
  labs(title = "投n次骰子，标准化后均值的分布",
       subtitle = "蓝色: 理论正态分布   紫色: 模拟数据分布直方图",
       x = "样本均值", y = "密度（频率）") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.8),  # 单独设置y轴
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))+  # 左边留2%空隙，右边留5%空隙
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))


ggsave("./Output/Lec3/MeanofDice_standard.png",dpi = 300, width = 6,height = 4, bg = "white")


# delta method
# \var = sigma^2 g'(\mu)^2

############### g(x) = sqrt(x) ###########
df_long <- df %>% pivot_longer(cols = everything(), 
                               names_to = "n", 
                               values_to = "mean_value")
df_long$n <- factor(df_long$n, levels = paste0("n =", n_values))


df_long$mean_value = sqrt(df_long$mean_value)


theory_data <- map_dfr(n_values, ~ {
  sd_val <- sqrt( (35/12)*(1/4/3.5)/.x  )
  data.frame(
    n = paste0("n =", .x),
    x = seq(0, 2*sqrt(3.5), length.out = 1000),
    y = dnorm(seq(0, 2*sqrt(3.5), length.out = 1000), mean = sqrt(3.5), sd = sd_val)
  )
})

theory_data$n <- factor(theory_data$n, levels = paste0("n =", n_values))

ggplot(df_long, aes(x = mean_value)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "red",
                 binwidth = 0.05, 
                 color = "white",
                 alpha = 0.5,
                 show.legend = FALSE) +
  geom_line(data = theory_data, aes(x = x, y = y), 
            color = "blue", 
            linewidth = 1.2,
            linetype = "dashed") +
  facet_wrap(~ n, ncol = 2, scales = "free_y") +  # 分面显示，2列
  labs(title = "投n次骰子，g(均值)的分布",
       subtitle = "蓝色: 理论正态分布   红色: 模拟数据分布直方图",
       x = "样本均值", y = "密度（频率）") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.8),  # 单独设置y轴
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))+  # 左边留2%空隙，右边留5%空隙
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))

ggsave("./Output/Lec3/MeanofDice_sqrt.png",dpi = 300, width = 6,height = 4, bg = "white")



########## standardized version ############
df_long <- df %>% pivot_longer(cols = everything(), 
                               names_to = "n", 
                               values_to = "mean_value")
df_long$n <- factor(df_long$n, levels = paste0("n =", n_values))


df_long$mean_value = sqrt(df_long$mean_value)


df_standardized <- df_long %>%
  group_by(n) %>%
  mutate(
    sample_mean = sqrt(3.5),
    sample_sd = sqrt(35/12*(1/4/3.5)),
    standardized_value = sqrt(as.numeric(gsub("n =","",n ) )) * (mean_value - sample_mean) / sample_sd
  ) %>%
  ungroup()



x_min = min(df_standardized$standardized_value)
x_max = max(df_standardized$standardized_value)

x_lim = max(abs(x_min),x_max)


theory_data <- map_dfr(n_values, ~ {
  sd_val <- 1
  data.frame(
    n = paste0("n =", .x),
    x = seq(-x_lim, x_lim, length.out = 1000),
    y = dnorm(seq(-x_lim, x_lim, length.out = 1000), mean =0, sd = sd_val)
  )
})

theory_data$n <- factor(theory_data$n, levels = paste0("n =", n_values))

ggplot(df_standardized, aes(x = standardized_value)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "purple",
                 binwidth = 0.65, 
                 color = "white",
                 alpha = 0.5,
                 show.legend = FALSE) +
  geom_line(data = theory_data, aes(x = x, y = y), 
            color = "blue", 
            linewidth = 1.2,
            linetype = "dashed") +
  facet_wrap(~ n, ncol = 2, scales = "free_y") +  # 分面显示，2列
  labs(title = "投n次骰子，标准化g(均值)的分布",
       subtitle = "蓝色: 理论正态分布   紫色: 模拟数据分布直方图",
       x = "样本均值", y = "密度（频率）") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.8),  # 单独设置y轴
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))+  # 左边留2%空隙，右边留5%空隙
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))


ggsave("./Output/Lec3/MeanofDice_sqrt_standard.png",dpi = 300, width = 6,height = 4, bg = "white")






##########################################
############### g(x) = 1/(1+e^(-x) )   ###########
# g'(x) = e^(-x)/(1+e^(-x))^2


g = function(x){
  1/(1+exp(-x))
}

g_deri = function(x){
  exp(-x)/(1+exp(-x))^2
}


df_long <- df %>% pivot_longer(cols = everything(), 
                               names_to = "n", 
                               values_to = "mean_value")
df_long$n <- factor(df_long$n, levels = paste0("n =", n_values))

df_long$mean_value = g(df_long$mean_value)


x_min = min(df_long$mean_value)
x_max = max(df_long$mean_value)

max_diff = max(abs(g(3.5) - x_min),abs(g(3.5)-x_max)  )

x_lim = max(abs(x_min),x_max)

theory_data <- map_dfr(n_values, ~ {
  sd_val <- sqrt( (35/12)*(g_deri(3.5)^2 )/.x  )
  data.frame(
    n = paste0("n =", .x),
    x = seq(g(3.5)- max_diff, g(3.5)+ max_diff, length.out = 1000),
    y = dnorm(seq(g(3.5)- max_diff, g(3.5)+ max_diff, length.out = 1000), mean = g(3.5), sd = sd_val)
  )
})

theory_data$n <- factor(theory_data$n, levels = paste0("n =", n_values))

ggplot(df_long, aes(x = mean_value)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "red",
                 binwidth = 0.01, 
                 color = "white",
                 alpha = 0.5,
                 show.legend = FALSE) +
  geom_line(data = theory_data, aes(x = x, y = y), 
            color = "blue", 
            linewidth = 1.2,
            linetype = "dashed") +
  facet_wrap(~ n, ncol = 2, scales = "free_y") +  # 分面显示，2列
  labs(title = "投n次骰子，g(均值)的分布",
       subtitle = "蓝色: 理论正态分布   红色: 模拟数据分布直方图",
       x = "样本均值", y = "密度（频率）") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.8),  # 单独设置y轴
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))+  # 左边留2%空隙，右边留5%空隙
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))


ggsave("./Output/Lec3/MeanofDice_sigmoid.png",dpi = 300, width = 6,height = 4, bg = "white")


############## standardized version ########################3

df_long <- df %>% pivot_longer(cols = everything(), 
                               names_to = "n", 
                               values_to = "mean_value")
df_long$n <- factor(df_long$n, levels = paste0("n =", n_values))


df_long$mean_value = g(df_long$mean_value)


df_standardized <- df_long %>%
  group_by(n) %>%
  mutate(
    sample_mean = g(3.5),
    sample_sd = sqrt(35/12)*g_deri(3.5),
    standardized_value = sqrt(as.numeric(gsub("n =","",n ) )) * (mean_value - sample_mean) / sample_sd
  ) %>%
  ungroup()




x_min = min(df_standardized$standardized_value)
x_max = max(df_standardized$standardized_value)

x_lim = max(abs(x_min),x_max)


theory_data <- map_dfr(n_values, ~ {
  sd_val <- 1
  data.frame(
    n = paste0("n =", .x),
    x = seq(-x_lim, x_lim, length.out = 1000),
    y = dnorm(seq(-x_lim, x_lim, length.out = 1000), mean =0, sd = sd_val)
  )
})

theory_data$n <- factor(theory_data$n, levels = paste0("n =", n_values))

ggplot(df_standardized, aes(x = standardized_value)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "purple",
                 binwidth = 0.5, 
                 color = "white",
                 alpha = 0.5,
                 show.legend = FALSE) +
  geom_line(data = theory_data, aes(x = x, y = y), 
            color = "blue", 
            linewidth = 1.2,
            linetype = "dashed") +
  facet_wrap(~ n, ncol = 2, scales = "free_y") +  # 分面显示，2列
  labs(title = "投n次骰子，标准化g(均值)的分布",
       subtitle = "蓝色: 理论正态分布   紫色: 模拟数据分布直方图",
       x = "样本均值", y = "密度（频率）") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.8),  # 单独设置y轴
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.02, 0.05)))+  # 左边留2%空隙，右边留5%空隙
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))


ggsave("./Output/Lec3/MeanofDice_sigmoid_standard.png",dpi = 300, width = 6,height = 4, bg = "white")







