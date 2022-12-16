require(here)
dat_dispersal = read.csv(
  here("data", "dispersal.csv"))

exp_fun = function(x, a, b)
{
  return(a* exp(-b * x))
}
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
fit_exp_nls = nls(
  disp.rate.ftb ~ exp_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_exp_nls)

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate",
  ylim = c(0,1),
  main = "Marbled Salamander - first time breeders\n Exponential model", col = "blue")
dist_newdata = data.frame(dist.class = seq(0, 1600, length.out = 1600))  
lines(predict(fit_exp_nls, newdata = dist_newdata))
curve(
  exp_fun(x, 2, 0.0034), add = TRUE, from = 0, to = 1500,
  ann = FALSE, ylab = "f(x)", col = "red"); box()
legend("topright", legend = c("NLS Fit", "Simulated Points", "Guess Curve"), pch = c(NA, 1, NA), lty = c(1, 0, 1), col = c("black", "blue", "red"))


require(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

dat_all$GCKI_pres = dat_all$GCKI > 0

# Create model fits
fit_gcki_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_gcki_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_gcki_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_gcki_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)
AIC(fit_gcki_slope)
AIC(fit_gcki_ba_tot)
AIC(fit_gcki_both_additive)
AIC(fit_gcki_both_interactive)

coef(summary(fit_gcki_both_interactive))

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

n = 500

ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n
  )
)

slope_newdata$gcki_predicted = 
  predict(
    fit_gcki_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$gcki_predicted = 
  predict(
    fit_gcki_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )

par(mfrow = c(2, 1))

# Presence/absence data, translucent points:
plot(
  GCKI_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(gcki_predicted ~ slope, data = slope_newdata)

plot(
  GCKI_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(gcki_predicted ~ ba.tot, data = ba_newdata)

n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)
head(new_dat_all)

tail(new_dat_all)

new_dat_all$pred_add = predict(
  fit_gcki_both_additive,
  newdata = new_dat_all,
  type = "response")
new_dat_all$pred_int = predict(
  fit_gcki_both_interactive,
  newdata = new_dat_all,
  type = "response")
z_gcki_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)
z_gcki_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)



# Contour ----
par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_gcki_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_gcki_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")