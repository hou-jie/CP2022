library(lmerTest)




# Model for Position and Sharpness ----------------------------------------

m.1 <- lmer(Position ~ Session + (1|ID), data = data)
summary(m.1)

m.2 <- lmer(Sharpness ~ Session + (1|ID), data = data)
summary(m.2)

# 事后多重比较: https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/
library(emmeans)
emmeans(m.1, pairwise ~ Session)
emmeans(m.2, pairwise ~ Session)
