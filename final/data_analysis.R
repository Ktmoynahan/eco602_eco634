# Read in data ----
require(here)
del = read.csv(here("data", "delomys.csv"))

# Numeric Exploration ----
summary(del$body_mass, del$body_length)
shapiro.test(del$body_mass)
shapiro.test(del$body_length)
# Graphical Exploration ----
plot(del$body_length, del$body_mass, xlab = "Body Length", ylab = "Body Mass", main = "Scatterplot of Body Mass & Body Length")
hist(del$body_mass, xlab = "Body Mass", main = "Histogram of Body Mass")
hist(del$body_length, xlab = "Body Length", main = "Histogram of Body Length", breaks = 8)
boxplot(del$body_mass ~ del$binomial, main = "Conditional BoxPlot of Body Mass on Species")
boxplot(del$body_mass ~ del$binomial, main = "Conditional BoxPlot of Body Mass on Species")
boxplot(del$body_mass ~ del$sex, main = "Conditional BoxPlot of Body Mass on Sex")
boxplot(
  body_mass ~ binomial + sex + binomial:sex, data = del, 
  main = "Conditional BoxPlot of Body Mass by Sex & Species")
