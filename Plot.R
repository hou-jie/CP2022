library(ggplot2)
library(dplyr)
theme_set(theme_bw())




# A. 辨认曲线 ------------------------------------------------------------------

expA$Session <- paste ("S", expA$Session, sep = "", collapse = NULL)

ggplot(data = expA, mapping = aes(x = StimNo, y = ACC)) + 
  geom_point(stat = "summary", fun = "mean", shape = 19) + 
  #geom_line(stat = "summary", fun = "mean") +  # 实际曲线
  geom_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, se = FALSE, size = 1.2) +  # 拟合曲线 - 平均
  stat_smooth(mapping = aes(group = Subject), geom = "line", method = "glm", method.args = list(family = "binomial"), formula = y ~ x, se = FALSE, color = "blue", size = 0.4, alpha = 0.2) + # 拟合曲线 - 个体
  scale_x_continuous(breaks = 1:11) + 
  facet_wrap(~ Session, nrow = 2) + 
  ylab("Response") + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) # 网格线







# B. Position and Sharpness ------------------------------------------------

data$Session <- as.factor(data$Session)

ggplot(data = data, mapping = aes(x = Session, y = Position, group = Session, fill = Session)) + 
  #geom_bar(stat = "summary", fun = "mean", width = 0.7) +  # 条形
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # 箱型
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.2) +  # 误差线 
  geom_point(size = 0.8, stroke = 0.7, pch = 21, alpha = 0.7, position = position_jitter(w = 0.3, h = 0)) +  # 散点
  #geom_text(data = mean.data, label = mean.data$sign, vjust = -c(5, 5, 5, 5, 5), size = 4, color = 'black') +  # 显著性标志
  theme(legend.position="none")


# -- -- -- -- --

ggplot(data = data, mapping = aes(x = Session, y = Sharpness, group = Session, fill = Session)) + 
  #geom_bar(stat = "summary", fun = "mean", width = 0.7) +  # 条形
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # 箱型
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.2) +  # 误差线 
  geom_point(size = 0.8, stroke = 0.7, pch = 21, alpha = 0.7, position = position_jitter(w = 0.3, h = 0)) +  # 散点
  #geom_text(data = mean.data, label = mean.data$sign, vjust = -c(5, 5, 5, 5, 5), size = 4, color = 'black') +  # 显著性标志
  theme(legend.position="none") + 
  scale_y_continuous(trans='reverse')
  
  # Use grey scale
  scale_color_grey() + theme_classic()



# C. Individual Difference ------------------------------------------------

# Diverging Dot Plot & Diverging Lollipop Chart - SIZE: 5 * 4 Portrait

library(dplyr)
library(forcats)
library(ggplot2)
theme_set(theme_bw())


data$ID = as.factor(data$ID)

data %>%
  mutate(ID = fct_reorder(ID, ORD)) %>%
  ggplot(mapping = aes(x = ID, y = ACC, group = Type, label = Type, color = Type)) + 
  geom_point(stat = 'identity', size = 5, alpha = 0.5) + 
  geom_point(stat = 'identity', size = 5, alpha = 0.2, shape = 21) + # 圆点轮廓
  geom_segment(aes(y = ACC0, x = ID, yend = ACC, xend = ID), alpha = 0.6) + 
  geom_text(color = "white", size = 2.5) + 
  geom_hline(aes(yintercept = 0.96), color = "#377EB8", linetype = "dashed", linewidth = 0.5, alpha = 0.9) + # 辨认基线 
  geom_hline(aes(yintercept = 0.78), color = "#E41A1C", linetype = "dashed", linewidth = 0.5, alpha = 0.9) + # 区分基线
  annotate("curve", x = 20.5, xend = 21, y = 0.96, yend = 0.93, colour = "#377EB8", curvature = 0.3) +  # 注释
  annotate("curve", x = 20.5, xend = 21, y = 0.78, yend = 0.75, colour = "#E41A1C", curvature = 0.3) + 
  annotate("text", x = 21.5, y = 0.86, color = "#377EB8", label = "Standard I") + 
  annotate("text", x = 21.5, y = 0.63, color = "#E41A1C", label = "Standard D") + 
  coord_flip(xlim = c(1, 20), clip = "off") + 
  scale_color_brewer(palette = "Set1") + 
  theme(legend.position="none") + 
  theme(plot.margin = unit(c(0.8, 0.2, 0.2, 0.2), "cm")) + # 边距
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "white"), panel.background = element_rect(fill = "grey97", color = NA)) + 
  labs(x = "Participant ID", y = "Progress") # 标签内容






