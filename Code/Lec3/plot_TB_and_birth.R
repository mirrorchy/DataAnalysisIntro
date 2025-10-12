library(ggplot2)
library(dplyr)
library(tidyr)

# plot for slide 11
# Output: TB_2015_2024.png  birth_2019_2023.png
# data collected from https://gis.fudan.edu.cn/Infectious/ 
# and https://www.nhc.gov.cn/guihuaxxs/c100133/new_list.shtml (2019-2023的卫生健康发展事业公报)


# 更改到当前工作路径
setwd("D:\\Software\\GithubDesktop\\DataAnalysisIntro")



rm(list = ls())
########## 肺结核发病率 /10万
data = c(42.71,54.87,39.76,45.37,47.76,55.55,59.27,60.53,61,63.42)
year = 2024:2015

# 创建数据框（ggplot2需要使用数据框格式）
tb_data <- data.frame(
  Year = year,
  Incidence = data
)

# 使用ggplot2绘图
ggplot(tb_data, aes(x = Year, y = Incidence)) +
  # 添加折线
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  # 添加数据点
  geom_point(color = "#A23B72", size = 3) +
  geom_text(aes(label = Incidence), vjust = -1.2, size = 3.5) + # 在点上方添加数值标签
  # 添加标签和标题
  labs(
    title = "2015-2024年 肺结核发病数（每10万人）",
    subtitle = "范围：中国大陆（不含港澳台）",
    x = "年份",
    y = "发病数 / 十万人",
  ) +
  # 设置x轴刻度为每个年份
  scale_x_continuous(breaks = year) +
  # 设置y轴范围，增加一些边距
  scale_y_continuous(limits = c(floor(min(data))-5, ceiling(max(data))+5)) +
  # 使用经典主题
  theme_classic() +
  # 自定义主题元素
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # 标题居中、加粗
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    axis.title = element_text(size = 12), # 坐标轴标题大小
    axis.text = element_text(size = 10), # 坐标轴文字大小
    panel.grid.major.y = element_line(color = "grey90") # 添加水平网格线
    
  )

ggsave("./Output/Lec3/TB_2015_2024.png",width = 6,height = 5,dpi = 300,bg = "white")




############### 卫生健康事业发展统计公报 ######## 
birth_rate = c(111.2, 111.1 ,110.9,110.46 ,110.14)
year = c(2023, 2022,2021,2020,2019 )



boy_data <- data.frame(
  Year = year,
  Boys = birth_rate/(birth_rate+100)
)

# 使用ggplot2绘图
ggplot(boy_data, aes(x = Year, y = Boys)) +
  # 添加折线
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  # 添加数据点
  geom_point(color = "#A23B72", size = 3) +
  geom_text(aes(label = round(Boys,3)), vjust = -1.2, size = 3.5) + # 在点上方添加数值标签
  # 添加标签和标题
  labs(
    title = "2019-2023年 中国出生人口中男性占比 ",
    x = "年份",
    y = "比例"
  ) +
  # 设置x轴刻度为每个年份
  scale_x_continuous(breaks = year) +
  # 设置y轴范围，增加一些边距
  scale_y_continuous(limits = c(0.52,0.53  ) ) +
  # 使用经典主题
  theme_classic() +
  # 自定义主题元素
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # 标题居中、加粗
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
    axis.title = element_text(size = 12), # 坐标轴标题大小
    axis.text = element_text(size = 10), # 坐标轴文字大小
    panel.grid.major.y = element_line(color = "grey90") # 添加水平网格线
    
  )


ggsave("./Output/Lec3/birth_2019_2023.png",width = 6,height = 5,dpi = 300,bg = "white")


