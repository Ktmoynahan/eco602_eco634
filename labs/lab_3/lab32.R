install.packages("psych")
install.packages("tmvnsim")
require(psych)
pairs.panels(iris)
names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])
require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)
require(here)
dat_hab = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_hab)

dat_all = merge(dat_bird, dat_hab, by = 'basin', all = TRUE)

pairs.panels(dat_hab)
names(dat_hab)
pairs.panels(dat_hab[, c("elev", "slope", "aspect", "ba.tot")])