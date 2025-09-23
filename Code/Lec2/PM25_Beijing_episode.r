# 将工作路径设置为现文件夹所在目录
library(dplyr)

setwd("D:/GitHub/DataAnalysisIntro(Public)/")
df_all <- read.csv("Input/Lec2/Beijing_Aotizhongxin.csv", header = TRUE)

# 只保留PM2.5非缺失且>=0的数据
df_all <- df_all[!is.na(df_all$PM2.5) & df_all$PM2.5 >= 0, ]

# 筛选2015和2023年
df_sub <- df_all[df_all$year %in% c(2015, 2023), ]


# 按<=35, >35（包括>150）分组，另单独统计>150
df_sub$episode <- cut(df_sub$PM2.5, breaks=c(-Inf, 35, Inf), labels=c("<=35", ">35"), right=TRUE)
df_sub$date <- as.Date(sprintf("%04d-%02d-%02d", df_sub$year, df_sub$month, df_sub$day))
df_sub <- df_sub[order(df_sub$year, df_sub$date, df_sub$hour), ]
df_sub$block_id <- as.integer((as.numeric(df_sub$date - ave(df_sub$date, df_sub$year, FUN=min)))/7) + 1
df_sub$time_block <- paste0(df_sub$year, "_block", df_sub$block_id)

library(dplyr)
library(tidyr)
library(ggplot2)

episode_summary <- df_sub %>%
	group_by(year, time_block, episode) %>%
	summarise(hours=n(), .groups='drop') %>%
	tidyr::pivot_wider(names_from=episode, values_from=hours, values_fill=list(hours=0)) %>%
	arrange(year, time_block)
# 增加>150小时数列，保持<=35和>35两列不变
gt150 <- df_sub %>% group_by(year, time_block) %>% summarise(gt150_hours=sum(PM2.5>150, na.rm=TRUE))
episode_summary$`>150` <- gt150$gt150_hours

# 组合分组：第一列<35，第二列>35，第三列>150
episode_plot <- episode_summary %>%
	select(year, time_block, `<=35`, `>35`, `>150`) %>%
	pivot_longer(cols=c("<=35", ">35", ">150"), names_to="episode", values_to="hours")
episode_plot$episode <- factor(episode_plot$episode,
	levels=c("<=35", ">35", ">150"),
	labels=c("Low PM(<=35)", "Polluting Episode(>35)", "Very High PM(>150)"))

# box plot
p <- ggplot(episode_plot, aes(x=factor(year), y=hours, fill=factor(year))) +
		geom_boxplot() +
		labs(title="", x="Year", y="Hours", fill="Year") +
		theme_minimal(base_size = 20) +
		theme(
			axis.title = element_text(size=24),
			axis.text = element_text(size=20),
			strip.text = element_text(size=22),
			legend.title = element_text(size=22),
			legend.text = element_text(size=20)
		) +
		facet_wrap(~episode, labeller = as_labeller(c(
			"Low PM(<=35)" = "Low PM(<=35)",
			"Polluting Episode(>35)" = "Polluting Episode(>35)",
			"Very High PM(>150)" = "Very High PM(>150)"
		)))

print(p)
ggsave("Output/Lec2/pm25_2015_2023_episode_hours_boxplot_by_episode.png", p, width=14, height=6)

