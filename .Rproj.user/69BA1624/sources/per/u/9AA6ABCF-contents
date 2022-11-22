data(iris)
fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)
summary(fit_species)
boxplot(Sepal.Length ~ Species, data = iris, main = "Conditional BoxPlot of fit_species")
shapiro.test(fit_species)

fit_petal = 
  lm(
   Petal.Width ~ Petal.Length,
    data = iris)
summary(fit_petal)