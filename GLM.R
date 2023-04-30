


data <- expA

# GLM ---------------------------------------------------------------------

# fit the model
m.1 <- glm(ACC ~ StimNo * Session, data = data, family = binomial())
m.2 <- glm(ACC ~ StimNo * Session, data = data, family = binomial(link = "probit"))

summary(m.1)


# calculate Slope, Position and Width
a = summary(model)$coef[2,"Estimate"]
b = summary(model)$coef[1,"Estimate"]
fitting = summary(model)$coef[2,"Pr(>|z|)"]
PA = -b/a
WA = -2*log(3)/a



# GLM For Each Trail ------------------------------------------------------

v <- 1:161
res <- data.frame(VALUE = c("a", "b", "PA", "WA", "fitting")) 

for ( i in v) {
  
  df <- data[data$Subject == i,]
  
  model <- glm(ACC ~ StimNo, data = df, family = binomial)
  #model <- glm(ACC ~ StimNo, data = df, family = binomial(link = "probit"))
  
  
  a = summary(model)$coef[2,"Estimate"]
  b = summary(model)$coef[1,"Estimate"]
  fitting = summary(model)$coef[2,"Pr(>|z|)"]
  PA = -b/a
  WA = -2*log(3)/a
  
  res$A <- c(a, b, PA, WA, fitting)
  names(res)[names(res) == "A"] <- i
  
}

write.table(x = res, file = "res.txt", quote = TRUE, sep = "\t", row.names = FALSE)
