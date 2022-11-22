require(here)
dat_rope = read.csv(here("data", "rope.csv"))
head(dat_rope)

##Template##
require(here)
rm(list = ls())

rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)
## check##
length(levels(rope$rope.type))
##
                        
n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))
                        
grand_mean = mean(rope$p.cut)
resid = (rope$p.cut - grand_mean)
ss_tot = sum(resid^2)
df_tot = n_obs - 1

test_agg = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))
agg_resid = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x - mean(x))
str(agg_resid)
agg_sum_sq_resids =  aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x - mean(x))^2))
str(agg_sum_sq_resids)
ss_within = sum(agg_sum_sq_resids$x)
df_within = n_obs - 6
                        
ss_among = ss_tot - ss_within
df_among = n_groups - 1
                        
ms_within = ss_within / (n_obs - n_groups)
ms_among  = ss_among / (n_groups - 1)
                        
f_ratio = ms_among / ms_within
f_pval = 1 - pf(f_ratio, df_among, df_within)

boxplot(p.cut ~ rope.type, rope)
bartlett.test(p.cut ~ rope.type, rope)

residuals(agg_resid)
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)
fit_resids=residuals(fit_rope_1)
shapiro.test(fit_resids)

resids_group = sapply(agg_resid$x, shapiro.test)
resids_group

require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")

boxplot(body_mass_g ~ species, data = pen_fem, main = "Conditional BoxPlot of Female Species")
bartlett.test(body_mass_g ~ species, data = pen_fem)

fit_fempen = lm(body_mass_g ~ species, data = pen_fem)
summary(fit_fempen)

fit_fempen_2 = lm(body_mass_g ~ species, data = pen_fem)
fempen2_hsd = TukeyHSD(aov(fit_fempen_2))
class(fempen2_hsd)
round(fempen2_hsd$species, digits = 4)
