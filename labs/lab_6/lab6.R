require(palmerpenguins)
sse_mean <- function(x){
values <- !is.na(x)
y <- x[values]
sd(y)/sqrt(length(y))
}
sse_mean(penguins$bill_depth_mm)
sse_mean(mtcars$mpg)


two_group_resample_diff = function(x, n_1, n_2) 
{
  values <- !is.na(x)
  y <- x[values]
  z <-sample(y, n_1, replace = TRUE)
  v <- sample(y, n_1, replace = TRUE)
  diff_in_means = 
    mean(z) - mean(v)
  
    return(diff_in_means)
}
two_group_resample_diff(penguins$bill_depth_mm, 100, 300)
dat_pen = subset(penguins, species != "Gentoo")
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
flipleng_mean<-sum(abs(mean_differences)>=5.8)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(
    body_mass_g ~ species, data = penguins,
    ylab = "Body Mass (g)")
}

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means
diff_crit

t.test(dat_pen$body_mass_g ~ dat_pen$species)

sse_mean <- function(x){
  values <- !is.na(x)
  y <- x[values]
  sd(y)/sqrt(length(y))
}
sse_mean(penguins$body_mass_g)

two_group_resample_diff = function(x, n_1, n_2) 
{
  values <- !is.na(x)
  y <- x[values]
  z <-sample(y, n_1, replace = TRUE)
  v <- sample(y, n_1, replace = TRUE)
  diff_in_means = 
    mean(z) - mean(v)
  
  return(diff_in_means)
}
two_group_resample_diff(penguins$body_mass_g, 68, 152)


n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$body_mass_g, 68, 152)
  )
}
hist(mean_differences)
bodymass_mean<-sum(abs(mean_differences)>=diff_crit)