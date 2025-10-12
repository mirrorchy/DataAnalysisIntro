# plot for slide 16: US elective in California

# Output: us_election_California_barplot.png
# data collected from BBC news: https://www.bbc.com/news/election/2024/us/results


library(ggplot2)

rm(list = ls())
setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")


state = "California"
harris_vote = 9276179
trump_vote = 6081697

# 创建数据框
data <- data.frame(
  Candidate = c("Harris", "Trump"),
  votes = c(9276179, 6081697),
  state = "California"
)

# 画图
ggplot(data, aes(x = Candidate, y = votes, fill = Candidate)) +
  geom_col(width = 0.3) +
  geom_text(aes(label = format(votes, big.mark = ",")), vjust = -0.5) +
  labs(title = "Vote Count in California",
       subtitle = " US election 2024",
       x = "Candidate", 
       y = "Votes") +
  scale_fill_manual(values = c("blue", "red"))+
  theme_classic() +              # 使用简洁的主题
  theme(
    # 坐标轴线设置
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.line.x = element_line(color = "black", linewidth = 0.6),  # 单独设置x轴
    axis.line.y = element_line(color = "black", linewidth = 0.6),  # 单独设置y轴
    axis.text.x = element_text(size = 12,face = "bold"),
    axis.title.y = element_text(size = 12),
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) 

ggsave(paste0("./Output/Lec3/us_election_",state,"_barplot.png"), width = 8, height = 5, dpi = 300,bg = "white")

