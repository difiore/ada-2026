library(tidyverse)
library(broom)
library(effects)
library(jtools)
library(sjPlot)
library(lmtest)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/titanic_train.csv"
d <- read_csv(f, col_names = TRUE)
d <- d |> select(-c("Name", "PassengerId", "Ticket", "Cabin"))
d <- d |> mutate(
  Sex = as.factor(Sex),
  Embarked = as.factor(Embarked),
  Pclass = as.factor(Pclass)
)
glimpse(d)

# plot survival in relation to sex
ggplot(d, aes(x = Sex, y = Survived)) +
  geom_jitter(na.rm = TRUE)

# plot survival in relation to plass
ggplot(d, aes(x = Pclass, y = Survived)) +
  geom_jitter(na.rm = TRUE)

# plot survival in relation to age
ggplot(d, aes(x = Sex, y = Survived)) +
  geom_point(na.rm = TRUE)



  +
  geom_smooth(method = "glm", method.args = list(family = binomial), na.rm = TRUE)

m <- glm(Survived ~ Sex, data = d, family = "binomial")
summary(m)


coefs <- tidy(m) |> select(estimate)
log_odds_female_survival <- coefs$estimate[1] + coefs$estimate[2] * 0
log_odds_male_survival <- coefs$estimate[1] + coefs$estimate[2] * 1

odds_female_survival <- exp(log_odds_female_survival) # odds of females surviving versus not
odds_male_survival <- exp(log_odds_male_survival) # odds of males surviving versus not

pr_female_survival <- odds_female_survival/(1 + odds_female_survival)
pr_male_survival <- odds_male_survival/(1 + odds_male_survival)
summary(m)

surv <- d |>
  group_by(Sex) |>
  summarize(surv = mean(Survived))


# {effects}
plot(allEffects(m)) # plots probabilities
# {jtools}
effect_plot(m, pred = Sex) # plots probabilities
# {sjPlot}
plot_model(m, type = "eff", terms = "Sex") # plots probabilities

# {jtools}
plot_summs(m) # plots log odds
plot_summs(m, exp = TRUE) # plots odds

# {sjPlot}
plot_model(m, type = "est", transform = NULL) # plots log odds
plot_model(m, type = "est") # plots odds

x <- data.frame(Sex = c("male","female"))
log_odds <- predict(m, newdata = x)
odds <- exp(log_odds) # odds of male and female surviving
y <- predict(m, newdata = x, type = "response", se.fit = TRUE) # prob of male and female surviving
y$fit # prob of male and female surviving

m <- glm(Survived ~ Age + Sex, data = d, family = "binomial")
summary(m)

coefs <- tidy(m) |> select(term, estimate)
coefs <- coefs |> mutate(odds = exp(estimate))
coefs

# Poisson regression
f <-"https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/woollydata.csv"

d <- read_csv(f, col_names = TRUE)
(p <- ggplot(data = d, aes(x = age, y = success)) +
    geom_point() +
    xlab("Age") +
    ylab("Mating Success"))

# run a glm of success ~ age
glm <- glm(data = d, success ~ age, family = "poisson")
summary(glm)
(results <- tidy(glm, conf.int = TRUE, conf.level = 0.95))
# effects with {sjPlot}
plot_model(glm, type = "eff", terms = "age") +
  geom_point(data = d, aes(x = age, y = success))
plot_model(glm, type = "est", terms = "age", transform = NULL) +
  scale_y_continuous(limits = c(0, 0.2))
plot_model(glm, type = "est", terms = "age") +
  scale_y_continuous(limits = c(0.9, 1.5))

fuller <- glm(data = d, success ~ age, family = "poisson")
reduced <- glm(data = d, success ~ 1, family = "poisson")
summary(reduced)

lrtest(reduced, fuller)
Gsq <- reduced$deviance - fuller$deviance
(p <- 1 - pchisq(Gsq, df = 1))

anova(reduced, fuller, test = "Chisq")

# bootstrapping to get p value for lrtest

set.seed(1)
boots <- 10000

# observed LRT statistic
Gsq <- 2 * (logLik(fuller) - logLik(reduced))
Gsq <- reduced$deviance - fuller$deviance

# extract the null model's fitted rate
lambda <- exp(coef(reduced)[1])  # exp(intercept)

Gsq_boot <- numeric()

for (i in 1:boots) {
  # simulate new outcome under the null (intercept-only Poisson)
  y_star <- rpois(nrow(d), lambda = lambda)
  # refit both models to simulated data
  fuller <- glm(y_star ~ d$age, family = "poisson")
  reduced <- glm(y_star ~ 1, family = "poisson")
  # compute and store LRT statistic
  Gsq_boot[i] <- reduced$deviance - fuller$deviance
}

# bootstrap p-value
p <- mean(Gsq_boot >= Gsq)
p

hist(Gsq_boot, breaks = 40)
abline(v = Gsq, col = "red")

# Mixed Effects Modeling

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/chimpgrooming.csv"

d <- read_csv(f, col_names = TRUE)
