library(lubridate)
library(dplyr)
library(ggplot2)

setwd("~/Documents/R-projects/proj4")

crimes <- read.csv("crimes.csv", header = TRUE)
moon <- read.csv("moon.csv", header = TRUE)
str(moon)
str(crimes)

crimes$POSIX <- ymd_hms(as.character(crimes$Dates))
crimes$Dates <- as.Date(ymd_hms(as.character(crimes$Dates)))

moon$date <- as.Date(moon$date, "%m/%d/%Y")

full_data <- merge(crimes, moon, by.x = "Dates", by.y = "date")
#or
# full_data <- inner_join(crimes, moon, by=c("Dates" = "date"))

date_phase <- full_data %>%
  group_by(Dates, phase) %>%
  count() %>%
  arrange(desc(n))

glimpse(date_phase)

ggplot(date_phase, aes(Dates, n)) +
  geom_line(alpha=0.5) +
  labs(title= "crimes in San-Francisco (2003-2015)",
       x = "Date",
       y = "Crimes") +
  geom_point(data=date_phase[date_phase$phase == "Full Moon",], color="red") +
  geom_smooth()

x <- mean(date_phase$n[date_phase$phase == "Full Moon"])
x
x1 <- mean(date_phase$n[date_phase$phase != "Full Moon"])
x1

n <- length(date_phase$n[date_phase$phase == "Full Moon"])
n

s <- sd(date_phase$n[date_phase$phase == "Full Moon"])
s

t <- (x-x1)/(s/sqrt(n))
t

p_value <- 2 * pt(t, df=n-1, lower.tail = FALSE)
p_value

x_vector <- date_phase$n[date_phase$phase == "Full Moon"]

t.test(x_vector, mu = x1, alternative = "two.sided", conf.level = 0.95)

day_crimes <- full_data %>%
  group_by(DayOfWeek) %>%
  count()  

glimpse(day_crimes)

day_crimes$DayOfWeek <- factor(day_crimes$DayOfWeek, levels = c("Monday",
      "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(data=day_crimes, aes(x=DayOfWeek, y=n)) +
  geom_bar(stat="identity", fill="lightblue")

crimes_by_day <- full_data %>%
  group_by(Dates, DayOfWeek) %>%
  count()

sample <- crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"]
sample
n <- length(sample)

x <- mean(crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"])
x
s <- sd(crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"])
s

t <- (x-x1)/(sd/sqrt(n))
t

p_value <- 2*pt(t, df = n-1, lower.tail = FALSE)
p_value

vector <- crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"]
t.test(vector, mu=x1, alternative = "two.sided", conf.level = 0.99)

