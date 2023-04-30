library(ggplot2)
library(mgcv)
library(itsadug)
library(dplyr)  # 数据整理


# PART 1 基本模型 -------------------------------------------------------------

# Data Preparation

# save(expB, file = "expB.RData")

data <- expB

data1 <- data %>%
  group_by(Subject, Session, StimNo, TypeB) %>%
  summarise(ACC = mean(ACC, na.rm = TRUE))

mean.data <- data1 %>%
  group_by(Subject, Session, StimNo) %>%
  summarise(ACC = mean(ACC, na.rm = TRUE))


# model with separate smooths - 与直线比较
mean.data$Session = as.factor(mean.data$Session)
m.1 <- bam(ACC ~ Session + s(StimNo, by = Session, bs = "cr", k = 9), data = mean.data, method = "ML")

summary(m.1)

# model with smooth for A & difference smooth - 与基线比较
mean.data$Session.ord = as.ordered(mean.data$Session)
contrasts(mean.data$Session.ord) <- "contr.treatment"
m.2 <- bam(ACC ~ Session.ord + s(StimNo, bs = "cr", k = 9) + s(StimNo, by = Session.ord, bs = "cr", k = 9), data = mean.data, method = "ML")

summary(m.2)


# nested model without the difference smooth
m.0 <- bam(ACC ~ s(StimNo, bs = "cr", k = 9), data = mean.data, method = "ML")

# model comparison
compareML(m.0, m.2)

plot_smooth(m.2, view = "StimNo", plot_all = "Session.ord", rug = F)
plot_diff(m.2, view = "StimNo", comp = list(Session.ord = c("0","4")), col.diff = "red", col = "#000099", lwd = 2)
plot_diff(m.2, view = "StimNo", comp = list(Session.ord = c("3","4")), col.diff = "red", col = "#000099", lwd = 2)



# PART 2 混合效应 -------------------------------------------------------------

# random intercepts only
m.2.int <- bam(ACC ~ Session.ord + s(StimNo, bs = "cr", k = 9) + s(Subject, bs = "re", k = 100),
               data = mean.data, method = "fREML")

# random intercepts + slopes
m.2.slope <- bam(ACC ~ Session.ord + s(StimNo, bs = "cr", k = 9) + s(StimNo, by = Session.ord, bs = "cr", k = 9) + s(Subject, bs = "re") + 
                   s(Subject, StimNo, bs = "re", k = 9),  
                 data = mean.data, method = "fREML")

# random smooths
m.2.smooth <- bam(ACC ~ Session.ord + s(StimNo, bs = "cr", k = 9) + 
                    s(StimNo, by = Session.ord, bs = "cr", k = 9) +
                    s(StimNo, Subject, bs = "fs", m = 1, k = 9),
                  data = mean.data, method = "fREML")

AIC(m.2.int, m.2.slope, m.2.smooth)



# PART 3 区分曲线 -------------------------------------------------------------

library(tidymv)
theme_set(theme_bw())

m.2.data <- predict_gam(m.2) # 提取模型数据

m.2.data$Session <- m.2.data$Session.ord  # 标签

ggplot(m.2.data, aes(x = StimNo, y = fit)) + 
  geom_smooth_ci(group = Session.ord, ci_alpha = 0) +  # 曲线
  geom_point(mean.data, mapping = aes(x = StimNo, y = ACC, group = Session, color = Session), stat = "summary", fun = "mean", shape = 19) +  # 平均值
  stat_summary(mean.data, mapping = aes(x = StimNo, y = ACC, group = Session, color = Session), fun.data = mean_se, geom = 'errorbar', width = 0.0) + # 标准差
  scale_x_continuous(breaks = 1:9) + 
  theme(legend.position = "bottom") + 
  labs(x = "StimNo", y = "Response")  # 标签内容




m.diff <- get_smooths_difference(m.2, StimNo, list(Session.ord = c("0", "4")))

m.diff %>%
  ggplot(aes(x = StimNo, y = difference, group = group)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_ribbon(aes(ymin = -Inf, ymax = Inf, fill = sig_diff, group = group), alpha = 0.2) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
  geom_line(size = 0.5) + 
  scale_x_continuous(breaks = 1:9) + 
  theme(legend.position="none") + 
  labs(x = "StimNo", y = "Estimated difference")  # 标签内容


m.diff <- get_smooths_difference(m.2, StimNo, list(Session.ord = c("3", "4")))

m.diff %>%
  ggplot(aes(x = StimNo, y = difference, group = group)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_ribbon(aes(ymin = -Inf, ymax = Inf, fill = sig_diff, group = group), alpha = 0.2) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
  geom_line(size = 0.5) + 
  scale_x_continuous(breaks = 1:9) + 
  theme(legend.position="none") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(x = "StimNo", y = "")  # 标签内容




# PART 4 差异估计 -------------------------------------------------------------


m.diff1 <- get_smooths_difference(m.2, StimNo, list(Session.ord = c("0", "4")))
m.diff2 <- get_smooths_difference(m.2, StimNo, list(Session.ord = c("3", "4")))

m.diff1$Contrast <- "S0 - S4"
m.diff2$Contrast <- "S3 - S4"

m.diff0 <- rbind(m.diff1, m.diff2)


m.diff0 %>%
  ggplot(aes(x = StimNo, y = difference, group = group)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_ribbon(aes(ymin = -Inf, ymax = Inf, fill = sig_diff, group = group), alpha = 0.2) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3) +
  geom_line(linewidth = 0.5) + 
  scale_x_continuous(breaks = 1:9) + 
  facet_wrap(~ Contrast) + 
  theme(legend.position="none") + 
  labs(x = "StimNo", y = "Estimated difference")  # 标签内容




