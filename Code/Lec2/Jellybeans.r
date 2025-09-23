# For slides p71 p73: jellybean guesses - scatter, boxplot, histogram, with and without log transformation
# Output: jellybean_plots.png, jellybean_plots_log.png, jellybean_hist_square.png

# Example and data are adapted from David Spiegelhalter's "The Art of Statistics"

# 将工作路径设置为现文件夹所在目录
setwd("D:/GitHub/DataAnalysisIntro(Public)/")

Guesses<-read.csv("Input/Lec2/02-1-bean-data-full-x.csv") # read in data
NGuesses <- nrow(Guesses)
summary(Guesses)
sd(Guesses$V1) # standard deviation of first column of data-frame

library(magrittr)
library(ggplot2)
library(ggpubr)


BlankXTheme <- theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())

BlankYTheme <- theme(axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     plot.margin=unit(c(0.1,0.1,0.1,0.65),"cm")) # also give a little space for plot labelling

BiggerTicks <- theme(axis.text.x = element_text(size=16), axis.title.x = element_text(size=16))

AugmentedGuesses <- cbind(Guesses, runif(NGuesses), rep(1, NGuesses))
names(AugmentedGuesses) <- c("Guess", "Aug", "Group") # Aug is jittered plotting position

Strip <- ggplot(AugmentedGuesses, aes(x=Guess, y=Aug)) + geom_point() + labs(y="") + theme_bw() +
  BlankXTheme + BlankYTheme # scatter type plot

Box <- ggplot(AugmentedGuesses, aes(x=Group, y=Guess)) + geom_boxplot() + coord_flip() + theme_bw() + 
  labs(title="Boxplot of Jellybean Guesses", x="Guess", y="") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) # box and whisker type plot

Hist<- ggplot(AugmentedGuesses, aes(x=Guess)) + geom_histogram(bins=50) + theme_bw()  + BlankYTheme + BiggerTicks + labs(x="Guess at number of beans in jar", hjust=0.0) # histogram plot

p <- ggarrange(Strip, Box, Hist, ncol=1, nrow=3, 
               labels=c("(a)","(b)","(c)"), hjust=0.0) # arrange in grid with a,b,c labels for the plots, 

# 储存plot
ggsave("Output/Lec2/jellybean_plots.png", plot=p, width=10, height=10)


logbreaks <- c(200,500,2000,5000,20000)

Strip <- ggplot(AugmentedGuesses, aes(x=Guess, y=Aug)) + geom_point() + 
  scale_x_continuous(trans="log10", breaks=logbreaks) + 
  theme_bw() + BlankXTheme + BlankYTheme # scatter type plot

Hist<- ggplot(AugmentedGuesses, aes(x=Guess)) + geom_histogram(bins=50) + 
  scale_x_continuous(trans="log10", breaks=logbreaks) + 
  theme_bw() + BlankYTheme + BiggerTicks + labs(x="Guess at number of beans in jar") # histogram plot

p <- ggarrange(Strip, Box, Hist, ncol=1, nrow=3, 
               labels=c("(a)","(b)","(c)"), hjust=0.0) # arrange plots in grid with labels

ggsave("Output/Lec2/jellybean_plots_log.png", plot=p, width=10, height=10)


# 计算log10 transformation
TransGuesses <- log10(AugmentedGuesses$Guess)
mean(TransGuesses)
sd(TransGuesses)
median(TransGuesses)
IQR(TransGuesses)
range(TransGuesses)
median_q1q3(TransGuesses)

# 画出原数据分布曲线并标出mean median 和 IQR
Hist <- ggplot(AugmentedGuesses, aes(x=Guess)) + geom_histogram(bins=50, aes(y=..density..), fill="lightgrey", color="black") + 
  theme_bw() + BlankYTheme + BiggerTicks + labs(x="Guess at number of beans in jar") # histogram plot
Hist_plot <- Hist +
  geom_density(color="blue", size=1) +
  geom_vline(xintercept=mean(AugmentedGuesses$Guess), color="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median(AugmentedGuesses$Guess), color="green", linetype="dashed", size=1) +
  annotate("text", x=mean(AugmentedGuesses$Guess), y=0.00045, label="Mean", color="red", size=8, angle=90, vjust=-0.5) +
  annotate("text", x=median(AugmentedGuesses$Guess), y=0.00045, label="Median", color="green", size=8, angle=90, vjust=-0.5) +
  ggtitle("Distribution of Guesses at number of beans in jar") + theme(plot.title=element_text(size=24, hjust=0.5), axis.title.x=element_text(size=20), axis.text.x=element_text(size=18), axis.title.y=element_text(size=20), axis.text.y=element_text(size=18)) +
  coord_cartesian(ylim=c(0, 0.0005))

ggsave("Output/Lec2/jellybean_hist_square.png", plot=Hist_plot, width=10, height=10)

