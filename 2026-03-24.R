rm(list = ls())
library(tidyverse)
library(broom)

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


f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE)

p1 <- ggplot(data = d, aes(x=Body_mass_female_mean, y=MaxLongevity_m)) + geom_point(na.rm = TRUE)
p2 <- ggplot(data = d, aes(x=log(Body_mass_female_mean), y=MaxLongevity_m)) + geom_point(na.rm = TRUE)
p3 <- ggplot(data = d, aes(x=log(Body_mass_female_mean), y=log(MaxLongevity_m))) + geom_point(na.rm = TRUE)
plot_grid(p1, p2, p3, nrow = 1)

m1 <- lm(MaxLongevity_m ~ Body_mass_female_mean, data = d)
m2 <- lm(MaxLongevity_m ~ log(Body_mass_female_mean), data = d)
m3 <- lm(log(MaxLongevity_m) ~ log(Body_mass_female_mean), data = d)

p1 <- ggplot(data = NULL, aes(x=m1$model$Body_mass_female_mean, y=m1$residuals)) + geom_point(na.rm = TRUE)
p2 <- ggplot(data = NULL, aes(x=m2$model$`log(Body_mass_female_mean)`, y=m2$residuals)) + geom_point(na.rm = TRUE)
p3 <- ggplot(data = NULL, aes(x=m3$model$`log(Body_mass_female_mean)`, y=m3$residuals)) + geom_point(na.rm = TRUE)
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
