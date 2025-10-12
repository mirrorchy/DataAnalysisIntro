# plots for slides 29 30 31: Logistic regression for the Titanic dataset
# Output: Titanic_sex.png Titanic_Pclass.png Titanic_Pclass_sex.png
# Titanic_Pclass_sex_age_Prob_on_age_3class.png  Titanic_Pclass_sex_age_Prob_vs_Real.png


library(titanic)
library(ggplot2)
library(dplyr)



rm(list = ls())


setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")

# 加载数据
data("titanic_train", package = "titanic")


#存活情况、船舱等级、性别、年龄、船票价格
titanic_clean <- titanic_train[, c("Survived", "Pclass", "Sex", "Age", "Fare")]
titanic_clean <- na.omit(titanic_clean)  # 删除有缺失值的行

titanic_clean$Pclass = as.character(titanic_clean$Pclass)
head(titanic_clean)

# 查看清理后的数据
print(paste("sample size:", nrow(titanic_clean)))
print(table(titanic_clean$Survived))  # 生存与死亡人数

# 3. 探索性分析
cat("=== 数据基本概况 ===\n")
cat("总样本数:", nrow(titanic_clean), "\n")
cat("生存率:", mean(titanic_clean$Survived), "\n")
cat("按性别的生存率:\n")
print(aggregate(Survived ~ Sex, data = titanic_clean, mean))
cat("按舱位的生存率:\n")
print(aggregate(Survived ~ Pclass, data = titanic_clean, mean))



# 模型1：仅用性别预测生存概率
model_sex <- glm(Survived ~ Sex, 
                 data = titanic_clean, 
                 family = binomial(link = "logit"))

# 查看模型结果
summary(model_sex)



plot_data <- data.frame(
  Sex = c("female", "male"),
  Probability = predict(model_sex, 
                        newdata = data.frame(Sex = c("female", "male")), 
                        type = "response")
)



# 绘制专业图表
ggplot(plot_data, aes(x = Sex, y = Probability, fill = Sex)) +
  geom_bar(stat = "identity", alpha = 0.7,width = 0.4) +
  # geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                # width = 0.2, color = "black") +
  geom_text(aes(label = round(Probability, 3)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue")) +
  labs(title = "逻辑回归预测：性别对生存概率的影响",
       subtitle = "泰坦尼克号数据集",
       x = "性别", 
       y = "预测生存概率") +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1))

ggsave("./Output/Lec3/Titanic_sex.png",width = 6,height = 4,dpi = 300,bg = "white")



#######################################################3
# 模型2：用船舱等级预测生存概率
model_class <- glm(Survived ~ Pclass, 
                   data = titanic_clean, 
                   family = binomial)

summary(model_class)


plot_data <- data.frame(
  Pclass = c("1","2","3"),
  Probability = predict(model_class, 
                        newdata = data.frame(Pclass = c("1","2","3")), 
                        type = "response")
)


plot_data$Pclass = as.character(plot_data$Pclass)

# 绘制专业图表
ggplot(plot_data, aes(x = Pclass, y = Probability, fill = Pclass)) +
  geom_bar(stat = "identity", alpha = 0.7,width = 0.4) +
  # geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                # width = 0.2, color = "black") +
  geom_text(aes(label = round(Probability, 3)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  # scale_fill_manual(values = c("female" = "pink", "male" = "lightblue")) +
  labs(title = "逻辑回归预测：船舱等级对生存概率的影响",
       subtitle = "泰坦尼克号数据集",
       x = "船舱等级", 
       y = "预测生存概率") +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1))

ggsave("./Output/Lec3/Titanic_Pclass.png",width = 6,height = 4,dpi = 300,bg = "white")






# 模型3：同时考虑性别和舱位等级
model_combined <- glm(Survived ~ Sex + Pclass, 
                      data = titanic_clean, 
                      family = binomial)

summary(model_combined)



# 1. 计算每个性别-舱位组合的实际存活率
actual_rates <- titanic_clean %>%
  group_by(Sex, Pclass) %>%
  summarize(Actual_Survival = mean(Survived),
            Count = n(),
            .groups = 'drop')

# 2. 创建预测数据
new_data <- expand.grid(
  Sex = c("female", "male"),
  Pclass = c("1", "2", "3")
)

# 3. 预测概率
new_data$Predicted_Probability <- predict(model_combined, newdata = new_data, type = "response")

# 4. 合并实际存活率
new_data <- new_data %>%
  left_join(actual_rates, by = c("Sex", "Pclass"))

# 5. 创建包含实际存活率的标签
new_data$Label <- sprintf("%.3f\n(实际: %.3f)", 
                          new_data$Predicted_Probability,
                          new_data$Actual_Survival)

# 6. 绘制分组条形图
ggplot(new_data, aes(x = factor(Pclass), y = Predicted_Probability, fill = Sex)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = Label), 
            position = position_dodge(0.8), vjust = -0.3, size = 3, lineheight = 0.8) +
  # 添加样本量信息
  # geom_text(aes(label = paste0("n=", Count), y = 0.05), 
            # position = position_dodge(0.8), size = 2.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"),
                    labels = c("female", "male")) +
  scale_x_discrete(labels = c("1", "2", "3")) +
  labs(title = "生存概率：性别与舱位的影响",
       subtitle = "上方数字：模型预测概率 | 括号内：实际观测存活率",
       x = "舱位等级",
       y = "预测生存概率",
       fill = "Sex") +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1.1))

ggsave("./Output/Lec3/Titanic_Pclass_sex.png",width = 6,height = 4,dpi = 300,bg = "white")










######################### 加入所有变量 #######################


# 构建完整模型
model_full <- glm(Survived ~ Sex + Pclass + Age , 
                  data = titanic_clean, 
                  family = binomial)

summary(model_full)


#########3 年龄的影响

# 创建包含三个舱位等级的预测数据
plot_data_age <- expand.grid(
  Age = seq(min(titanic_clean$Age), max(titanic_clean$Age), length.out = 50),
  Sex = "female",  # 固定性别为女性
  Pclass = c("1", "2", "3")  # 包含三个舱位等级
)

# 预测概率
plot_data_age$Probability <- predict(model_full, newdata = plot_data_age, type = "response")

# 将Pclass转换为因子并设置标签（可选）
plot_data_age$Pclass <- factor(plot_data_age$Pclass, 
                               levels = c("1", "2", "3"),
                               labels = c("1", "2", "3"))

# 绘制三条年龄效应曲线
ggplot(plot_data_age, aes(x = Age, y = Probability, color = Pclass, group = Pclass)) +
  geom_line(size = 1.2) +
  labs(title = "年龄对生存概率的影响",
       subtitle = "控制变量：女性，按舱位等级分组",
       x = "年龄",
       y = "预测生存概率",
       color = "舱位等级") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.line.y = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green"))

ggsave("./Output/Lec3/Titanic_Pclass_sex_age_Prob_on_age_3class.png",width = 6,height = 4,dpi = 300,bg = "white")

titanic_clean$Predicted_Prob <- predict(model_full, type = "response")

# 绘制预测概率的分布
ggplot(titanic_clean, aes(x = Predicted_Prob, fill = factor(Survived))) +
  geom_density(alpha = 0.5) +
  # geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"),
                    labels = c("死亡", "生存"),
                    name = "实际结果") +
  labs(title = "预测概率分布 vs 实际生存结果（性别+舱位+年龄）",
       x = "模型预测的生存概率",
       y = "密度") +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))


ggsave("./Output/Lec3/Titanic_Pclass_sex_age_Prob_vs_Real.png",width = 8,height = 5,dpi = 300,bg = "white")


