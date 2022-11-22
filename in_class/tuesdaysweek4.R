require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)


require(here)
dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)
names(dat_bird)
names(dat_habitat)
pairs(dat_habitat[, c("ba.hard", "ba.con", "ba.snag")])



require(palmerpenguins)
pairs(penguins)
pairs(penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])


names(dat_bird)
names(dat_habitat)
pairs(dat_habitat[, c("ba.hard", "ba.con", "ba.snag")])
hist(dat_bird$HAFL, xlab = "HAFL", breaks = 0:7 - 0.5, ylab = "Frequency", main = "HAFL sightings")

