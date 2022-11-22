require(here)
dat_catrate = read.csv(
  here("data", "catrate.csv")
)
head(dat_catrate)
is.numeric()

require(here)
dat_delomys = read.csv(
  here("data", "delomys.csv")
)
head(dat_delomys)

require(here)
dat_rope = read.csv(
  here("data", "rope.csv")
)
head(dat_rope)

hist(dat_delomys$body_mass, xlab = "Body Mass", ylab = "Frequency", main = "Body Mass of Delomys by Keegan Moynahan")
