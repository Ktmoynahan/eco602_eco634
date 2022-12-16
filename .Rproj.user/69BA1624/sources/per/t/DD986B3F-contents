##1-3
require(palmerpenguins)
boxplot(
  body_mass_g ~ species + sex + species:sex, data = penguins, 
  main = "Conditional BoxPlot of Body Mass by Sex & Species",
  xlab = "", ylab = "Body Mass (g)", names = c("female\nAdelie", "male\nAdelie", "female\nChinstrap", "male\nChinstrap", "female\nGentoo", "male\nGentoo"),
  las = 2, cex.axis = 0.6)
##4-7
fit_both = lm(body_mass_g ~ sex*species, data = penguins)
summary(fit_both)
##8
dat_chin = subset(penguins, species == "Chinstrap")
t.test(subset(dat_chin, sex == "female")$body_mass_g)
