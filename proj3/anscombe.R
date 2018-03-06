library(dplyr)
library(ggplot2)

anscombe
str(anscombe)
summary(anscombe)

cor.test(anscombe$x1, anscombe$y1)
cor.test(anscombe$x2, anscombe$y2)
cor.test(anscombe$x3, anscombe$y3)

pred_model = lm(y1 ~ x1, data=anscombe)
summary(pred_model)
anscombe$y1 - pred_model$fitted.values
res <- pred_model$residuals

ggplot(anscombe, aes(x=res)) +
  geom_dotplot(fill="orange")

ggplot(anscombe, aes(x=x4, y=y4)) +
  geom_point(col="blue") +
  geom_smooth(method = "lm", se=FALSE)
