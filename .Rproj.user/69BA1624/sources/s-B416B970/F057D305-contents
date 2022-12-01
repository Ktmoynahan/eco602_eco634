require(palmerpenguins)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "less")

install.packages("simpleboot")
library(simpleboot)
x = droplevels(subset(dat_pen, species != "Chinstrap"))
y = droplevels(subset(dat_pen, species != "Adelie"))
pen_boot <- two.boot(
  na.omit(x$flipper_length_mm), na.omit(y$flipper_length_mm), mean, 
  10000)
sd(pen_boot$t)
hist(pen_boot$t, xlab = 'T', main = "Mean of Flipper Length")
quantile(pen_boot$t, probs = c(0.025, 0.975))
mean(pen_boot$t)
median(pen_boot$t)
pen_ecdf = ecdf(pen_boot$t)
1 - pen_ecdf(-4.5)
pen_ecdf(-8)

require(here)
veg = read.csv(here("data", "vegdata.csv"))
head(veg)
veg_treat = droplevels(subset(veg, treatment %in% c("control", "clipped")))
wilcox.test(pine ~ treatment, data = veg_treat, alternative = "two.sided")

x = droplevels(subset(veg_treat, treatment == "clipped"))
y = droplevels(subset(veg_treat, treatment == "control"))
tree_boot <- two.boot(x$pine, y$pine, mean, 1000)
quantile(tree_boot$t, probs = c(0.025, 0.975))

require(here)
dat_bird = read.csv(here("data", "bird.sub.csv"))
head(dat_bird)
require(here)
dat_habitat = read.csv(here("data", "hab.sub.csv"))
head(dat_habitat)
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (
  dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)


s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

dat_all$s.sidi.standardized = (
  dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

m = 10000 
result_mc = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  result_mc[i] = coef(fit_resampled_i)[2]
} 
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
crit = quantile(result_mc, c(.05))
hist(
  result_boot,
  main = "Mike's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = crit, lty = 2, col = 1, lwd = 2)

hist(
  result_mc,
  main = "Keegan's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

quantile(result_mc, c(.05))

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)
fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)

m = 10000 
result_boot = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  head(dat_boot)
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  
  coef(fit_bs1)
  
  result_boot[i] = coef(fit_bs1)[2]
} 
plot(
  density(result_mc),
  main = "Mike's Null Distribution Density Plot",
  xlab = "Slope Coefficient")
plot(
  density(result_boot),
  main = "Mike's Alternative Distribution Density Plot",
  xlab = "Slope Coefficient")


#plot first line
plot(
  density(result_mc),
  type='l', col='red', main = "Keegan's Null and Alternative Distributions",
  xlab='Slope Coefficient', ylab='Density', ylim = c(0,80))

#add second line to plot
lines(
  density(result_boot))
#add legend
legend(0.02, 60, legend=c("Null", "Alt."), 
       fill = c("red","black")
)