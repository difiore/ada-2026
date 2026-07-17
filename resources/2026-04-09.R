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
