install.packages("psych")
install.packages("tmvnsim")
require(psych)

require(here)
dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)

hist(dat_habitat$elev, xlab = "Elevation", ylab = "Frequency", main = "Elevation of Sampling site")

hist(dat_habitat$aspect, xlab = "Aspect", ylab = "Frequency", main = "Aspect of Sampling site")

hist(dat_habitat$slope, xlab = "Slope", ylab = "Frequency", main = "Slope of Sampling site")

par(mfrow = c(1,3))


plot(dat_habitat$elev, dat_habitat$ba.tot, xlab = "Elevation", ylab = "Basal Area", main = "Basal Area Terrain(Elevation)")

plot(dat_habitat$aspect, dat_habitat$ba.tot, xlab = "Aspect", ylab = "Basal Area", main = "Basal Area Terrain(Aspect)")

plot(dat_habitat$slope, dat_habitat$ba.tot, xlab = "Slope", ylab = "Basal Area", main = "Basal Area Terrain(Slope)")

par(mfrow = c(1,3))

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
par(mfrow = c(1,3))
plot(
  x = dat_habitat$elev, 
  y = dat_habitat$ba.tot,
  xlab = "Elevation",
  ylab = "Basal Area",
  main = "Elevation/Terrain Linear Model",
  col = "#009dff"
)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.01), add = TRUE)

plot(
  x = dat_habitat$aspect, 
  y = dat_habitat$ba.tot,
  xlab = "Aspect",
  ylab = "Basal Area",
  main = "Aspect/Terrain Linear Model",
  col = "red"
)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.01), add = TRUE)

plot(
  x = dat_habitat$slope, 
  y = dat_habitat$ba.tot,
  xlab = "Slope",
  ylab = "Basal Area",
  main = "Slope/Terrain Linear Model",
  col = "green"
)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.05), add = TRUE)
par(mfrow = c(1,3))

hist(dat_habitat$residuals, xlab = "Residuals", ylab = "Frequency", main = "Residuals of Dat_random1")
