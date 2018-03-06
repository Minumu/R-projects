library(dplyr)
library(ggplot2)

str(diamonds)

ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(col="lightblue")

cor.test(diamonds$carat, diamonds$price)

ggplot(data=diamonds, aes(x=carat, y=price, col=cut)) +
  geom_point()

ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(col="lightblue") +
  geom_smooth(method = "lm", se=FALSE) +
  facet_wrap(~cut)

lm_ideal = lm(price ~ carat, data=diamonds, cut=="Fair")
lm_ideal$coefficients
summary(lm_ideal)

ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(col="blue") +
  geom_smooth(method = "lm", se=FALSE)
