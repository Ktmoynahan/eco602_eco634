require(here)
ginkgo = data.frame(read.csv(here("data", "ginkgo_data_2022.csv")))
head(ginkgo)

seed_site = subset(ginkgo, select= c('seeds_present','site_id'))
trees = length(unique(seed_site$site_id))
seed = sum(seed_site$seeds_present/10)

boxplot(max_width ~ seeds_present, data = ginkgo, main = "Conditional BoxPlot of Seeds Present")
boxplot(ginkgo$petiole_length, main = "Boxplot of Petiole Length", ylim = c(0,150), ylab = "Petiole Length")

plot(max_width ~ max_depth, data = ginkgo, xlab = 'Leaf Depth (cm)', ylab = 'Leaf Width (cm)', main = "Scatter Plot of Leaf Depth and Width")

require(here)
library(here)
library(palmerpenguins)
require(palmerpenguins)
hist(penguins$flipper_length_mm, xlab = "Flipper Length (mm)", breaks = 8)

par(mfrow = c(2, 2))
hist(penguins$flipper_length_mm, xlab = 'Flipper Length (mm)', main = "Penguins Flipper Length")
hist(penguins$body_mass_g, xlab = 'Body Mass (g)', main = "Penguins Body Mass")
hist(penguins$bill_length_mm, xlab = 'Bill Length (mm)', main = "Penguins Bill Length")
hist(penguins$bill_depth_mm, xlab = 'Bill Depth (mm)', main = "Penguins Bill Depth")
