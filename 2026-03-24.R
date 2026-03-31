rm(list = ls())
library(tidyverse)
library(broom)
library(cowplot)
library(mosaic)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/BovetAndRaymondData.csv"
d <- read_csv(f, col_names = TRUE)

ggplot(data = d, aes(x = Century, y = `Corrected WHR`)) +
  geom_point() +
  geom_smooth(method = "lm")

d <- d |> filter(Century > 5)

ggplot(data = d, aes(x = Century, y = `Corrected WHR`)) +
  geom_point() +
  geom_smooth(method = "lm")

# Fit the original model...
m <- lm(`Corrected WHR` ~ Century, data = d)
summary(m) # WHR ratio of female subjects in classical artwork has decreased

# ... and extract the observed coefficient
observed_coef <- tidy(m) |>
  filter(term == "Century") |>
  pull(estimate)

# --- Permutation Test ---
# If WHR has not changed over time, then randomly shuffling Century values should not change the estimate of the regression coefficients much
# We build a null distribution by doing this many times

nperm <- 1000
perm_coefs <- vector(length = nperm)

for (i in 1:nperm){
  d_perm <- d  # make a fresh copy each iteration
  d_perm$Century <- sample(d$Century) # shuffle predictor
  perm_coefs[[i]] <- lm(`Corrected WHR` ~ Century, data = d_perm) |>
    tidy() |>
    filter(term == "Century") |>
    pull(estimate)
}
hist(perm_coefs)

# --- Calculate two-tailed p-value ---
# How often does a permuted coefficient exceed our observed coefficient in magnitude?
p_value <- mean(abs(perm_coefs) >= abs(observed_coef))
cat("Permutation p-value:", p_value, "\n")

# Compare to the classic parametric p value from the model
observed_coef_p <- tidy(m) |>
  filter(term == "Century") |>
  pull(p.value)
cat("Parametric p-value:", observed_coef_p, "\n")

# How is this calculated? From t statistic of the model and # of degrees of freedom...
# Extract the t-statistic and df from the model
t_stat <- tidy(m) |>
  filter(term == "Century") |>
  pull(statistic)

df <- df.residual(m)  # n - number of parameters estimated

# The p-value is the probability of observing a t-statistic this extreme or MORE, under the null hypothesis that the true slope coefficient = 0
p_value_manual <- 2 * pt(-abs(t_stat), df = df)
cat("Manual p-value:", p_value_manual, "\n")

summary(m)

# --- Bootstrap Confidence Interval ---
# We don't know the true sampling distribution of our coefficient, so we approximate it by resampling *with replacement* from our own data
# Each resample is treated as if it were a new dataset from the same population

nboot <- 1000
boot_coefs <- vector(length = nboot)

for (i in 1:nboot) {
  d_boot <- d[sample(nrow(d), replace = TRUE), ]  # resample rows with replacement
  boot_coefs[[i]] <- lm(`Corrected WHR` ~ Century, data = d_boot) |>
    tidy() |>
    filter(term == "Century") |>
    pull(estimate)
}
hist(boot_coefs)


# --- Calculate the 95% CI from the middle 95% of the bootstrap distribution ---
ci <- quantile(boot_coefs, c(0.025, 0.975))
cat("Bootstrap 95% CI:", ci[[1]], "to", ci[[2]], "\n")

# Compare to the classic parametric CI from the model, which is based on a t statistic
cat("Parametric 95% CI:",
    tidy(m, conf.int = TRUE) |>
      filter(term == "Century") |>
      pull(conf.low),
    "to",
    tidy(m, conf.int = TRUE) |>
      filter(term == "Century") |>
      pull(conf.high), "\n")

# How is this calculated? From t statistic of the model, degrees of freedom, and standard error...
# Extract the SE
se <- tidy(m) |>
  filter(term == "Century") |>
  pull(std.error)

# Critical t-value for 95% CI
t_crit <- qt(0.975, df = df)

ci_manual <- c(
  lower = observed_coef - t_crit * se,
  upper = observed_coef + t_crit * se
)
cat("Hand-calculated 95% CI:", ci_manual[1], "to", ci_manual[2])


# --- Visualize both distributions ---
par(mfrow = c(1, 2))

# Permutation null distribution
hist(perm_coefs, breaks = 30, main = "Permutation\nNull Distribution",
     xlab = "Coefficient (shuffled)", xlim = c(-0.01, 0.01))
abline(v = observed_coef, col = "firebrick", lwd = 2)
abline(v = -observed_coef, col = "firebrick", lwd = 2, lty = 2)

# Bootstrap sampling distribution
hist(boot_coefs, breaks = 30, main = "Bootstrap Sampling Distribution",
     xlab = "Coefficient (resampled)")
abline(v = observed_coef, col = "firebrick", lwd = 2)
abline(v = ci, col = "steelblue", lwd = 2, lty = 2)

rm(list = ls())
library(tidyverse)
library(broom)
library(cowplot)
library(mosaic)
library(ggExtra)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE)



p1 <- ggplot(data = d,
             aes(x = Body_mass_female_mean,
                 y = MaxLongevity_m)) +
      geom_point(na.rm = TRUE)
p1

p2 <- ggplot(data = d,
             aes(x = log(Body_mass_female_mean),
                 y = MaxLongevity_m)) +
      geom_point(na.rm = TRUE)
p2

p3 <- ggplot(data = d,
             aes(x = log(Body_mass_female_mean),
                 y = log(MaxLongevity_m))) +
      geom_point(na.rm = TRUE)
p3
p3
plot_grid(p1, p2, p3, nrow = 1)


# Add marginal density plots to each
p1m <- ggMarginal(p1, type = "densigram")
p2m <- ggMarginal(p2, type = "densigram")
p3m <- ggMarginal(p3, type = "densigram")

plot_grid(p1m, p2m, p3m, nrow = 1)

m1 <- lm(MaxLongevity_m ~ Body_mass_female_mean, data = d)
m2 <- lm(MaxLongevity_m ~ log(Body_mass_female_mean), data = d)
m3 <- lm(log(MaxLongevity_m) ~ log(Body_mass_female_mean), data = d)

p1 <- ggplot(data = NULL,
             aes(x = m1$model$Body_mass_female_mean,
                 y = m1$residuals)) +
      geom_point(na.rm = TRUE)
p2 <- ggplot(data = NULL,
             aes(x = m2$model$`log(Body_mass_female_mean)`,
                 y = m2$residuals)) +
      geom_point(na.rm = TRUE)
p3 <- ggplot(data = NULL,
             aes(x = m3$model$`log(Body_mass_female_mean)`,
                 y = m3$residuals)) +
      geom_point(na.rm = TRUE)
p4 <- histogram(m1$residuals, nint = 20)
p5 <- histogram(m2$residuals, nint = 20)
p6 <- histogram(m3$residuals, nint = 20)
plot_grid(p1, p4, p2, p5, p3, p6, nrow = 3)

car::qqPlot(m1$residuals)
car::qqPlot(m2$residuals)
car::qqPlot(m3$residuals)

p1 <- ggpubr::ggqqplot(m1$residuals)
p2 <- ggpubr::ggqqplot(m2$residuals)
p3 <- ggpubr::ggqqplot(m3$residuals)
plot_grid(p1, p2, p3, nrow = 1)

shapiro.test(m1$residuals)
shapiro.test(m2$residuals)
shapiro.test(m3$residuals)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)
keep <- c("Species1", "Family1", "Order1", "Beak.Length_Culmen", "Beak.Width", "Beak.Depth", "Tarsus.Length", "Wing.Length", "Tail.Length", "Mass", "Habitat", "Migration", "Trophic.Level", "Trophic.Niche", "Primary.Lifestyle", "Min.Latitude", "Max.Latitude", "Centroid.Latitude", "Range.Size")
d <- d |> select(all_of(keep))
glimpse(d)

p1 <- ggplot(data = d |> drop_na(Trophic.Level),
             aes(x = Trophic.Level, y = log(Mass))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1)

# nice visualization
density_p1 <- ggplot(data = d |> drop_na(Trophic.Level),
                     aes(x = log(Mass), fill = Trophic.Level)) +
  geom_density(alpha = 0.5) +
  coord_flip() +  # align with boxplot y-axis
  theme_void()    # clean up axes so it looks like a margin plot

plot_grid(p1, density_p1, nrow = 1, rel_widths = c(1, 1))

p2 <- ggplot(data = d |> drop_na(Migration),
             aes(x = as.factor(Migration), y = log(Mass))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1)
p2

m1 <- lm(log(Mass) ~ Trophic.Level, data = d)
m2 <- lm(log(Mass) ~ as.factor(Migration), data = d)
summary(m1)


(pairwise.t.test(log(d$Mass), d$Trophic.Level,
                 p.adj = "bonferroni"))

plotDist("f", df1 = 3, df2 = 11000)

m1aov <- aov(log(Mass) ~ Trophic.Level, data = d)
(posthoc <- TukeyHSD(m1aov, which = "Trophic.Level",
                       conf.level = 0.95))


observed.F <- aov(log(Mass) ~ Trophic.Level, data = d) |>
  broom::tidy() |>
  filter(term == "Trophic.Level")

observed.F

# ANOVA by permutation
nperm <- 1000
permuted.F <- vector(length = nperm)

for (i in 1:nperm){
  d_perm <- d
  d_perm$Trophic.Level <- sample(d$Trophic.Level) # shuffle predictor
  permuted.F[[i]] <- aov(log(Mass) ~ Trophic.Level, data = d_perm) |>
    tidy() |>
    filter(term == "Trophic.Level") |>
    pull(statistic)
}

histogram(permuted.F)

p.value <- mean(permuted.F > observed.F$statistic)
p.value

# ANOVA by permutation using {infer}
library(infer)
d <- d |> mutate(logMass = log(Mass)) # data wrangling to be able to use `specify()`

permuted.F <- d |>
  specify(logMass ~ Trophic.Level) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "F")

visualize(permuted.F) +
  shade_p_value(obs_stat = 	observed.F$statistic,
                direction = "greater")

p.value <- permuted.F |>
  get_p_value(obs_stat = observed.F$statistic,
              direction = "greater")

p.value

original.F$p.value

# how does R values parametric p value??
pf(observed.F$statistic, df1 = 3, df2 = 11000, lower.tail = FALSE) # not a two-tailed test...



f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)
keep <- c("Species1", "Family1", "Order1", "Beak.Length_Culmen", "Beak.Width", "Beak.Depth", "Tarsus.Length", "Wing.Length", "Hand-Wing.Index", "Tail.Length", "Mass", "Habitat", "Migration", "Trophic.Level", "Trophic.Niche", "Primary.Lifestyle", "Min.Latitude", "Max.Latitude", "Centroid.Latitude", "Range.Size")
d <- d |> select(all_of(keep))
glimpse(d)

d <- d |> mutate(
  logMass = log(Mass),
  logRS = log(Range.Size),
  logBeak = log(Beak.Length_Culmen),
  logTarsus = log(Tarsus.Length),
  Migration = as.factor(Migration))

relBeak <- lm(logBeak ~ logMass, data = d)
relTarsus <- lm(logTarsus ~ logMass, data = d)
d <- d |> mutate(
  relBeak = relBeak$residuals,
  relTarsus = relTarsus$residuals)

d <- d |> mutate(
  Primary.Lifestyle = factor(Primary.Lifestyle,
                             levels = c("Aerial", "Aquatic", "Insessorial", "Terrestrial", "Generalist"))
)

p1 <- ggplot(data = d |> drop_na(Primary.Lifestyle), aes(x=Primary.Lifestyle, y=`Hand-Wing.Index`)) +
  geom_boxplot() +
  #geom_jitter(alpha = 0.05) +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1))

p2 <- ggplot(data = d |> drop_na(Primary.Lifestyle), aes(x=Primary.Lifestyle, y=relTarsus)) +
  geom_boxplot() +
  #geom_jitter(alpha = 0.05) +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1))

d <- d |> mutate(
  Trophic.Niche = factor(Trophic.Niche,
                         levels = c("Nectarivore", "Herbivore aquatic", "Frugivore", "Granivore", "Herbivore terrestrial", "Aquatic predator", "Invertivore", "Vertivore", "Scavenger", "Omnivore"))
)

p3 <- ggplot(data = d |> drop_na(Trophic.Niche), aes(x=Trophic.Niche, y=relBeak)) +
  geom_boxplot() +
  # geom_jitter(alpha = 0.05) +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1))

plot_grid(p1, p2, p3, nrow = 1)

# Note using tropic level not trophic niche now...

pass <- d |> filter(Order1 == "Passeriformes") |>
  drop_na(Primary.Lifestyle, Trophic.Level) |>
  mutate(Primary.Lifestyle = as.factor(Primary.Lifestyle),
         Trophic.Level = as.factor(Trophic.Level))
glimpse(pass)
m <- aov(relBeak ~ Primary.Lifestyle, data = pass)
summary(m)
m <- aov(relBeak ~ Trophic.Level, data = pass)
summary(m)
m <- aov(relBeak ~ Primary.Lifestyle + Trophic.Level, data = pass)
summary(m)
m <- aov(relBeak ~ Primary.Lifestyle + Trophic.Level + Primary.Lifestyle:Trophic.Level, data = pass)
summary(m)

interaction.plot(
  x.factor = pass$Primary.Lifestyle,
  xlab = "Primary Lifestyle",
  trace.factor = pass$Trophic.Level,
  trace.label = "Trophic Level",
  response = pass$relBeak,
  fun = base::mean, # make sure we use {base} version
  ylab = "Mean Relative Beak Length"
)

interaction.plot(
  x.factor = pass$Trophic.Level,
  xlab = "Trophic Level",
  trace.factor = pass$Primary.Lifestyle,
  trace.label = "Primary Lifestyle",
  response = pass$relBeak,
  fun = base::mean, # make sure we use {base} version
  ylab = "Mean Relative Beak Length"
)

library(sjPlot)
# Plot marginal means
plot_model(m, type = "emm", terms = c("Primary.Lifestyle", "Trophic.Level"))

# or, alternatively
library(emmeans)
# Plot marginal means
emmip(m, Trophic.Level ~ Primary.Lifestyle, CIs = TRUE)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
z <- read_csv(f, col_names = TRUE)
m <- lm(height ~ weight + age, data = z)
summary(m)
plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
plot(fitted(m), residuals(m))

F <- (summary(m)$r.squared*(nrow(z)-2-1))/((1-summary(m)$r.squared) * 2)

p <- pf(F, df1 = 2, df2 = 997, lower.tail = FALSE)
m <- lm(height ~ weight + age + gender , data = z)
summary(m)
library(car)
vif(m)

library(jtools)
effect_plot(m, pred = weight,
            interval = TRUE, int.type = "confidence", int.width = 0.95,
            plot.points = TRUE)

plot_summs(m)
