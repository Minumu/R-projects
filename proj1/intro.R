library(dplyr)
library(ggplot2)

setwd("~/Documents/R-projects")

flats <- read.csv("flats.csv", stringsAsFactors = FALSE, encoding = "UTF-8", dec=",")
class(flats)

################
#dim(table) - number of records (cols and rows)
#head(table, n) - head of table (n-records)
#tail(table, n)
#names(table) - names in table
################

str(flats) #structure of table
summary(flats) #length, mean, median, Qu...
glimpse(flats) #general info
flats %>%
  filter(Місто != "Києво-Святошинський") %>%
  #filter(Кімнат == 1) %>%
  group_by(Місто) %>%
  #count(Місто) %>%
  #arrange(desc(n))
  summarise(mean=median(Загальна_площа), sd=sd(Загальна_площа))
ggplot(flats, aes(x=Загальна_площа)) +
  geom_histogram(breaks=seq(0, 250, by = 20),
                   fill="lightblue", col="blue") +
  ylab('Кількість')
ggplot() + geom_boxplot(data=flats, aes(x=factor(Кімнат), y=Ціна)) +
  xlab('') + ylab('Ціна грн') +
  coord_flip()

#################
# geom_bar
# geom_line
# geom_point
# geom_histogram
# geom_boxplot
#################