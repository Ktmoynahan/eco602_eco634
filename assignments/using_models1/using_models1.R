require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
summary(catrate)
hist(catrate$cat.rate, xlab = 'Catastrophe Rate', main = "Histogram of Catastrophe Rates")

shapiro.test(catrate$cat.rate)

t.test(catrate$cat.rate, mu = 2/7)

wilcox.test(catrate$cat.rate, mu = 2 / 7)


require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
dat_adelie = subset(penguin_dat, species == "Adelie")
shapiro.test(dat_adelie$flipper_length_mm)
dat_chin = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_chin$flipper_length_mm)


par(mfrow = c(1, 2))
hist(dat_chin$flipper_length_mm, xlab = 'Flipper Length (mm)', main = "Chinstrap Penguins")
hist(dat_adelie$flipper_length_mm, xlab = 'Flipper Length (mm)', main = "Adelie Penguins")

t.test(flipper_length_mm ~ species, data = dat_pen)

