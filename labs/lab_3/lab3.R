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

dat_all = merge(dat_bird, dat_hab)
plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)

my_vec <- dat_all$CEWA
my_vec > 1
cewa_present_absent = as.numeric(my_vec > 1)
plot(x = dat_all$elev, y = cewa_present_absent)


# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

pairs.panels(dat_hab)
names(dat_hab)
pairs.panels(dat_hab[, c("elev", "slope", "aspect", "ba.tot")])

my_vec1 <- dat_all$RECR
my_vec1 > 1
RECR_present_absent = as.numeric(my_vec1 > 1)
plot(x = dat_all$ba.tot, y = RECR_present_absent)

my_vec <- dat_all$AMCR
my_vec > 1
AMCR_present_absent = as.numeric(my_vec > 1)
plot(x = dat_all$ba.tot, y = AMCR_present_absent, )

plot(x = dat_all$ba.tot, y = RECR_present_absent, xlab = "Basal Area", ylab = "Red Crossbill")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.3), add = TRUE)

gray_vec <- dat_all$GRJA
sum(gray_vec)


my_vec <- dat_all$GRJA
sum(my_vec >= 1)
