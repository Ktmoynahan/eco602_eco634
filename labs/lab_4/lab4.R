# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)
require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")
mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)
n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)
dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)
set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)


norm_17 = rnorm(n = 17, mean = 10.4, sd = 2.4)
norm_30 = rnorm(n = 30, mean = 10.4, sd = 2.4)
norm_300 = rnorm(n = 300, mean = 10.4, sd = 2.4)
norm_3000 = rnorm(n = 3000, mean = 10.4, sd = 2.4)

require(here)
png(
  filename = here("lab_04_hist_01.png"),
  width = 1500, height = 1600, 
  res = 180)
par(mfrow = c(2, 2))

hist(norm_17, main = "17 Random Points")
hist(norm_30, main = "30 Random Points")
hist(norm_300, main = "300 Random Points")
hist(norm_3000, main = "3000 Random Points")
dev.off()

svg("norm_1.svg")
x = seq(5, 15, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, main = "Normal SVG: mean = 10.4 SD = 2.4", type = "l", xlim = c(5, 15))
abline(h = 0)
dev.off()

require(here)
png(
  filename = here("Random_Data_Plots.png"),
  width = 1500, height = 1600, 
  res = 180)
par(mfrow = c(2, 2))

x_random = runif(n = 5, min = 1, max = 5)
y_random = rnorm(n = 5)
dat_random1 = data.frame(x = x_random, y = y_random)
plot(y ~ x, data = dat_random1, pch = 8, main = "5 Random numbers: Min = 1 Max = 5")

x_random = runif(n = 10, min = 1, max = 10)
y_random = rnorm(n = 10)
dat_random2 = data.frame(x = x_random, y = y_random)
plot(y ~ x, data = dat_random2, pch = 7, main = "10 Random numbers: Min = 1 Max = 10")

x_random = runif(n = 15, min = 1, max = 15)
y_random = rnorm(n = 15)
dat_random3 = data.frame(x = x_random, y = y_random)
plot(y ~ x, data = dat_random3, pch = 9, main = "15 Random numbers: Min = 1 Max = 15")

x_random = runif(n = 20, min = 1, max = 20)
y_random = rnorm(n = 20)
dat_random4 = data.frame(x = x_random, y = y_random)
plot(y ~ x, data = dat_random4, pch = 6, main = "20 Random numbers: Min = 1 Max = 20")
dev.off()

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

require(here)
png(
  filename = here("Fitted_Linear_Model.png"),
  width = 1500, height = 1600, 
  res = 180)

plot(y ~ x, data = dat_random1, pch = 8, main = "Fitted Linear Model on Data Frame 1")
curve(line_point_slope(x, 6, 0, 0.6), add = T)
dev.off()

line_point_slope(dat_random1$x, 6, 0, 0.6)
dat_random1$predicted = line_point_slope(dat_random1$x, 6, 0, 0.6)
dat_random1$residuals = dat_random1$y - dat_random1$predicted 
x_random = runif(n = 5, min = 1, max = 5)
y_random = rnorm(n = 5)
dat_random1 = data.frame(x = x_random, y = y_random)

hist(dat_random1$residuals, xlab = "Residuals", ylab = "Frequency", main = "Residuals of Dat_random1")
plot(dat_random1$predicted, dat_random1$residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Predicted Values and Residuals of dat_random1")