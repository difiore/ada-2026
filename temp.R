library(tidyverse)
library(mosaic)
library(infer)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/woolly-weights.csv"
d <- read_csv(f, col_names = TRUE)
m <- mean(d$weight)
sd <- sd(d$weight)
se <- sd/sqrt(length(d$weight))
n <- length(d$weight)
se <- sd/sqrt(n)
mu <- 7.2 # expectation for mean
t_stat <- (m - mu)/se
t_stat
ci <- m + qt(
  p = c(0.025, 0.975),
  ncp = 0,
  df = n -1
) * se

p_lower <- pt(-1 * abs(t_stat), df = n - 1)
p_upper <- 1 - pt(1 * abs(t_stat), df = n - 1)
p <- p_lower + p_upper
p

t.test(d$weight, mu = 7.2)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/tbs-2006-2008-ranges.csv"
d <- read_csv(f, col_names = TRUE)

s <- d |>
  select(id, sex, kernel95) |>
  group_by(sex) |>
  summarize(avg = mean(kernel95),
            sd = sd(kernel95),
            se = sd/sqrt(nrow(d))
            )
s <- d |>
  group_by(sex) |>
  summarize(mean = mean(kernel95),
         sd = sd(kernel95),
         se = sd/sqrt(nrow(d))
  )

p <- ggplot(data = d, aes(x = sex, y = kernel95)) +
  geom_boxplot() +
  geom_jitter()

p

males <- d |> filter(sex == "M")
females <- d |> filter(sex == "F")

n_boot <- 10000
boot_males <- {do(n_boot) *
                 mean(sample(
                   males$kernel95,
                   nrow(males),
                   replace = TRUE))} |>
  pull(mean)

histogram(boot_males)
plotDist("norm", mean(boot_males), sd(boot_males), add=TRUE)

# or

boot_males <- {do(n_boot) *
                 mean(resample(males$kernel95))}|>
  pull(mean)
m_males <- mean(boot_males)
sd_males <- sd(boot_males)

histogram(boot_males)
plotDist("norm", mean = m_males, sd = sd_males, add = TRUE)

ci_boot_m <- quantile(boot_males, probs = c(0.025, 0.975))

ci_norm_m <- m_males + qnorm(c(0.025, 0.975)) * sd_males

boot_females <- {do(n_boot) *
                   mean(resample(females$kernel95))}|>
  pull(mean)

m_females <- mean(boot_females)
sd_females <- sd(boot_females)

histogram(boot_females)
plotDist("norm", mean = m_females, sd = sd_females, add = TRUE)

ci_boot_f <- quantile(boot_females, probs = c(0.025, 0.975))
ci_norm_f <- m_females + qnorm(c(0.025, 0.975)) * sd_females

t_num <- mean(males$kernel95) - mean(females$kernel95) - 0

t_denom <- sqrt(var(males$kernel95)/nrow(males) + var(females$kernel95)/nrow(females))

(t <- t_num/t_denom)

df_num <- (var(males$kernel95)/nrow(males) + var(females$kernel95)/nrow(females))^2

df_denom <- (var(males$kernel95)/nrow(males))^2/(nrow(males) - 1) + (var(females$kernel95)/nrow(females))^2/(nrow(females) - 1)

(df <- df_num/df_denom)

# implemented in R
t.test(males$kernel95, females$kernel95, var.equal = FALSE)

d <- d |>
  select(sex, kernel95)

# actual difference between means
obs_diff <-
  d |>
    summarize(diff = mean(kernel95[sex == "M"]) -
                     mean(kernel95[sex =="F"])) |>
  pull(diff)
obs_diff

n_perm <- 10000
perm_diff <- vector()
for (i in 1:n_perm) {
  d |>
    mutate(sex = sample(sex)) |>
    summarize(diff = mean(kernel95[sex == "M"]) -
                mean(kernel95[sex =="F"])) |>
    pull(diff) -> perm_diff[[i]]
}
histogram(perm_diff)

# or...
perm_diff <- {do(n_perm) *
    d |>
    mutate(sex=sample(sex)) |>
    summarize(diff =	mean(kernel95[sex == "M"]) -
                mean(kernel95[sex == "F"]))
  } |>
  pull(diff) # extract column of value as vector
histogram(perm_diff)

# or...
perm_diff <- replicate(n_perm, {
  d |>
    mutate(sex=sample(sex)) |>
    summarize(diff =	mean(kernel95[sex == "M"]) -
                mean(kernel95[sex == "F"])) |>
    pull(diff) # extract column of value as vector
  })
histogram(perm_diff)

(p_val <- sum(abs(perm_diff) >= abs(obs_diff))/length(perm_diff))

(p_val <- mean(abs(perm_diff) >= abs(obs_diff)))

# infer package
library(infer)
d <- d |>
  specify(formula = kernel95 ~ sex)
d <- d |>
  hypothesize(null = "independence")
perm <- d |>
  generate(reps = 1000, type = "permute")
perm_diff <- perm |>
  calculate(stat = "diff in means", order = c("M", "F"))
visualize(perm_diff, bins = 20)

obs_diff <- d |>
  specify(kernel95 ~ sex) |>
  calculate(stat = "diff in means", order = c("M", "F"))

visualize(perm_diff, bins = 20) +
  shade_p_value(obs_stat = obs_diff, direction = "both")
get_p_value(perm_diff, obs_diff, direction = "both")
