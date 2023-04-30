library(lmerTest)
library(emmeans)



# GLMM --------------------------------------------------------------------

data <- expB[is.na(expB$StimType) == FALSE,]

data$Type[data$StimType == "WC"] <- -1
data$Type[data$StimType == "BC"] <- 1
data$Type = as.factor(data$Type)
data$Session = as.factor(data$Session)


# fit the model
m.0 <- glm(ACC ~ Session * Type, data = data, family = binomial())

m.1 <- glmer(ACC ~ Session * Type + (1|Subject), data = data, family = binomial(), control=glmerControl(optimizer="bobyqa"))


# Fixed Effect
summary(m.1)

# Main Effect
anova(m.1)
anova(m.1, test = "Chisq")
car::Anova(m.1, type = 3)

# Simple Effect
emmeans(m.1, pairwise ~ Session|Type)
emmeans(m.1, pairwise ~ Type|Session)



# GLMM PLOT ---------------------------------------------------------------

library(ggplot2)
theme_set(theme_bw())
library(dplyr)  # 数据整理


data <- expB[is.na(expB$StimType) == FALSE,]

mean.data <- data %>%
  group_by(Subject, Session, StimType) %>%
  summarise(ACC = mean(ACC, na.rm = TRUE))


ggplot(data = mean.data, mapping = aes(x = Session, y = ACC, group = StimType, color = StimType)) + 
  geom_point(stat = "summary", fun = "mean", shape = 19) +  # 平均值
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.0, alpha = 0.7) +  # 标准差
  geom_line(stat = "summary", fun = "mean") +  # 曲线
  annotate(geom = "text", x = c(0, 1, 2, 3, 4), y = 0.05, label = c("a", "b", "bc", "c", "bc")) + 
  annotate(geom = "text", x = c(0, 1, 2, 3, 4), y = 0.55, label = c("b", "d", "cd", "c", "a")) + 
  annotate(geom = "text", x = c(0, 1, 2, 3, 4), y = 0.60, label = c("***", "", "***", "***", "***")) + 
  annotate(geom = "rect", xmin = -0.3, xmax = 4.3, ymin = 0.03, ymax = 0.07, alpha = 0.1, fill = "blue") + 
  annotate(geom = "rect", xmin = -0.3, xmax = 4.3, ymin = 0.53, ymax = 0.57, alpha = 0.1, fill = "red") + 
  annotate(geom = "rect", xmin = -0.3, xmax = 4.3, ymin = 0.58, ymax = 0.62, alpha = 0.1, fill = "orange")






df1 <- mean.data[mean.data$StimType == "BC",]
df2 <- mean.data[mean.data$StimType == "WC",]

df <- df1
df$StimType <- "BC-WC"
df$ACC = df1$ACC - df2$ACC

ggplot(data = mean.data, mapping = aes(x = Session, y = ACC, group = StimType, color = StimType)) + 
  geom_point(stat = "summary", fun = "mean", shape = 19) +  # 平均值
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.0, alpha = 0.7) +  # 标准差
  geom_line(stat = "summary", fun = "mean") +  # 曲线
  geom_bar(data = df, aes(x = Session, y = ACC), stat = "summary", fun = "mean", color = "orange", fill = "orange", alpha = 0.1, width = 0.5) +  # 差值
  annotate(geom = "text", x = c(0, 1, 2, 3, 4), y = 0.55, label = c("a", "b", "bc", "c", "bc")) + 
  annotate(geom = "text", x = c(0, 1, 2, 3, 4), y = 0.60, label = c("b", "d", "cd", "c", "a")) + 
  annotate(geom = "text", x = c(0, 1, 2, 3, 4), y = 0.02, label = c("***", "", "***", "***", "***")) + 
  annotate(geom = "rect", xmin = -0.3, xmax = 4.3, ymin = 0.53, ymax = 0.57, alpha = 0.1, fill = "blue") + 
  annotate(geom = "rect", xmin = -0.3, xmax = 4.3, ymin = 0.58, ymax = 0.62, alpha = 0.1, fill = "red") +  
  #annotate(geom = "rect", xmin = -0.3, xmax = 4.3, ymin = 0.63, ymax = 0.67, alpha = 0.1, fill = "orange") + 
  theme(legend.position = "bottom") + 
  ylab("Response")  # 标签内容



# Save Object
save(p1, file = "p1.RData")
save(p2, file = "p2.RData")
save(p3, file = "p3.RData")

