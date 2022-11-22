ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
exp_fun = function(x, a, b)
{
  return(a* exp(-b * x))
}
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")
fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 3))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)


exp_fun = function(x, a, b)
{
  return(a* exp(-b * x))
}
curve(
  exp_fun(x, 1.9, 0.07), col = "black", lty = "solid", add = TRUE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)"); box()
curve(
  exp_fun(x, 1.9, 0.3), col "black", lty = "dotted", add = TRUE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)"); box()
curve(
  exp_fun(x, 1.2, 0.2), col = "red", lty = "solid", add = TRUE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)"); box()
curve(
  exp_fun(x, 1.2, 0.4), col = "red", lty = "dotted", add = TRUE, from = 0, to = 50,
  ann = FALSE, ylab = "f(x)"); box()


ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 80, add = TRUE, col = "black", lty = "solid", ylim = c(0,90),
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 80, add = TRUE, col = "black", lty = "dotted", ylim = c(0,90),
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 80, add = TRUE, col = "black", lty = "dotted", ylim = c(0,90),
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 80, add = TRUE, col = "red", lty = "solid", ylim = c(0,90),
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 80, add = TRUE, col = "red", lty = "dotted", ylim = c(0,90),
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 80, add = TRUE, col = "red", lty = "dotted", ylim = c(0,90),
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

require(here)
dat_sal = read.csv(
  here("data", "dispersal.csv")
)
head(dat_sal)

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

plot(
  dat_sal$dist.class,
  dat_sal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n Fitted linear model")
curve(line_point_slope(x, 1400, 0.07, -0.0005), add = TRUE)


exp_fun = function(x, a, b)
{
  return(a* exp(-b * x))
}
plot(
  dat_sal$dist.class,
  dat_sal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n Exponential model")
curve(
  exp_fun(x, 2, 0.0034), add = TRUE, from = 0, to = 1500,
  ann = FALSE, ylab = "f(x)"); box()

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
plot(
  dat_sal$dist.class,
  dat_sal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate",
  ylim = c(0,1),
  main = "Marbled Salamander - first time breeders\n Ricker model")
curve(
  ricker_fun(x, 0.02, 0.0075), 
  from = 0, to = 1500, add = TRUE,
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

dat_sal$predicted_linear = line_point_slope(dat_sal$dist.class, 1400, 0.07, -0.0005)
dat_sal$residuals_linear = dat_sal$disp.rate.ftb - dat_sal$predicted 
dat_sal$predicted_exponential = exp_fun(dat_sal$dist.class, 2, 0.0034)
dat_sal$residuals_exponential = dat_sal$disp.rate.ftb - dat_sal$predicted 
dat_sal$predicted_ricker = ricker_fun(dat_sal$dist.class, 0.02, 0.0075)
dat_sal$residuals_ricker = dat_sal$disp.rate.ftb - dat_sal$predicted 

par(mfrow = c(1, 3))
hist(dat_sal$residuals_linear, xlab = "Residuals", ylab = "Frequency", main = "Residuals of Linear Slope Model")
hist(dat_sal$residuals_exponential, xlab = "Residuals", ylab = "Frequency", main = "Residuals of Exponential Model")
hist(dat_sal$residuals_ricker, xlab = "Residuals", ylab = "Frequency", main = "Residuals of Ricker Model")

