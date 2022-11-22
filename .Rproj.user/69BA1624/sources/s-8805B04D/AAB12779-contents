birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)
br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

require(palmerpenguins)
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)

fit_both = 
  lm(
    formula = body_mass_g ~ species + sex + species:sex,
    data = penguins)

boxplot(body_mass_g ~ species, data = penguins, main = "Conditional BoxPlot of fit_species")
boxplot(body_mass_g ~ sex, data = penguins, main = "Conditional BoxPlot of fit_sex")
boxplot(
  body_mass_g ~ species + sex + species:sex, data = penguins, 
  main = "Conditional BoxPlot of fit_box",
  names = c("female Adeilie", "female Chinstrap", "female Gentoo", "male Adeilie", "male Chinstrap", "male Gentoo"),
  las = 2)
boxplot(
  body_mass_g ~ species + sex + species:sex, data = penguins, 
  main = "Conditional BoxPlot of fit_box",
  xlab = "",
  las = 2, cex.axis = 0.6)

bartlett.test(body_mass_g ~ species, data = penguins)
bartlett.test(body_mass_g ~ sex, data = penguins)

dat_groups = aggregate(
  body_mass_g ~ species*sex,
  data = penguins,
  FUN = c)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g)

require(here)
dat_fl = data.frame(read.csv(here("data", "trees_FL.csv")))
head(dat_fl)
dat_probfail = table(dat_fl$ProbabilityofFailure)
dat_failstand = table(dat_fl$Failure_Standardized)
dat_dbh = table(dat_fl$DBH_in)
dat_hei = table(dat_fl$HeighttoTop_ft)
barplot(dat_probfail, xlab = 'Classes', ylab = 'Frequency', main = "Boxplot of Probability of Failure")
barplot(dat_failstand, xlab = 'Classes', ylab = 'Frequency', main = "Boxplot of Failure_Standardized")
hist(dat_dbh, xlab = 'Classes', ylab = 'DBH (in)', main = "Histogram of DBH", breaks = 5)
plot(HeighttoTop_ft ~ DBH_in, data = dat_fl, xlab = 'DBH (in)', ylab = 'Height (ft)', main = "Histogram of DBH & Height")

dat_whole = droplevels(subset(dat_fl, Failure_Standardized == "whole"))
dat_none = droplevels(subset(dat_fl, Failure_Standardized == "none"))
ks.test(dat_whole$DBH_in,dat_none$DBH_in)

cor.test(
  dat_fl$HeighttoTop_ft,
  dat_fl$DBH_in,
  use='complete.obs',
  method='spearman')

dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")

fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2

chisq_fl.fail = chisq.test(fl_table_2)
str(chisq_fl.fail)
chisq_fl.fail$residuals
