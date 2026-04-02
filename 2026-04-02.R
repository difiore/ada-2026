library(tidyverse)
library(broom)
library(car)
library(jtools)
library(sjPlot)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
z <- read_csv(f, col_names = TRUE)
m <- lm(height ~ weight + age, data = z)
summary(m)
tidy(m)

plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
plot(fitted(m), residuals(m))

F <- (summary(m)$r.squared*(nrow(z)-2-1))/((1-summary(m)$r.squared) * 2)
p <- pf(F, df1 = 2, df2 = 997, lower.tail = FALSE)

m <- lm(height ~ weight + age + gender , data = z)
summary(m)
tidy(m)

plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
boxplot(m$residual ~m$model$gender)
plot(fitted(m), residuals(m))

F <- (summary(m)$r.squared*(nrow(z)-3-1))/((1-summary(m)$r.squared) * 3)
p <- pf(F, df1 = 3, df2 = 996, lower.tail = FALSE)

vif(m)



f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)
acc <- d |>
  filter(Order1 == "Accipitriformes")
m <- lm(log(Range.Size) ~ log(Mass) + Primary.Lifestyle, data = acc)
summary(m)

unique(acc$Primary.Lifestyle)


m <- lm(height ~ weight + age + gender, data = z)
ci <- predict(m, newdata = data.frame(age = 29, gender = "Male", weight = 160), interval = "confidence", level = 0.95)
pi <- predict(m, newdata = data.frame(age = 29, gender = "Male", weight = 160), interval = "prediction", level = 0.95)

effect_plot(m, pred = weight,
            plot.points = TRUE)

effect_plot(m, pred = age,
            plot.points = TRUE)

effect_plot(m, pred = gender,
            plot.points = TRUE,
            jitter = 0.1)

plot_summs(m)


plot_summs(m, plot.distributions = TRUE, rescale.distributions = TRUE)




plot_model(m, type = "pred", show.data = TRUE, terms = "weight")
plot_model(m, type = "pred", show.data = TRUE, terms = "age")
plot_model(m, type = "pred", show.data = TRUE, terms = "gender", jitter = 0.1)
plot_model(m, type = "est")


f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)
keep <- c("Species1", "Family1", "Order1", "Beak.Length_Culmen", "Beak.Width", "Beak.Depth", "Tarsus.Length", "Wing.Length", "Hand-Wing.Index", "Tail.Length", "Mass", "Habitat", "Migration", "Trophic.Level", "Trophic.Niche", "Primary.Lifestyle", "Min.Latitude", "Max.Latitude", "Centroid.Latitude", "Range.Size")
d <- d |> select(all_of(keep))
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

m1 <- lm(data = d, logBeak ~ logRS * Migration)
m2 <- lm(data = d, logBeak ~ logRS + Migration)
m3 <- lm(data = d, logBeak ~ logRS)
m4 <- lm(data = d, logBeak ~ Migration)
m5 <- lm(data = d, logBeak ~ 1)
anova(m2, m1, test = "F")
anova(m4, m2, test = "F")

vars <- c("logBeak", "logRS", "Migration")
d_clean <- d |>
  dplyr::select(all_of(vars)) |>
  drop_na()




m2 <- lm(data = d_clean, logBeak ~ logRS + Migration) # model without interaction term
m4 <- lm(data = d_clean, logBeak ~ Migration)

anova(m4, m2, test = "F")

vars <- c("relBeak", "logRS", "Migration", "Trophic.Level", "relTarsus", "Primary.Lifestyle")
d_new <- d |>
  dplyr::select(all_of(vars)) |>
  drop_na()

m <- lm(relBeak ~ logRS + Migration + Trophic.Level + relTarsus + Primary.Lifestyle, data = d_new)

m_null <- lm(data = d_new, relBeak ~ 1)
add1(m_null, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

m1 <- update(m_null, formula = .~. + Primary.Lifestyle)
add1(m1, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

m2 <- update(m1, formula = .~. + Trophic.Level)
add1(m2, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

m3 <- update(m2, formula = .~. + Migration)
add1(m3, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

m_full <- lm(data = d_new,
             relBeak ~ logRS + relTarsus +
             Migration + Trophic.Level +
             Primary.Lifestyle)

drop1(m_full, test = "F")

m1 <- update(m_full, formula = .~. - logRS)
drop1(m1, test = "F")

m2 <- update(m1, formula = .~. - relTarsus)
drop1(m2, test = "F")

library(MASS)
s <- stepAIC(m_full, scope = .~., direction = "both")
s <- stepAIC(m_null,
             scope = .~. + logRS + relTarsus +
              Migration +
              Trophic.Level +
              Primary.Lifestyle,
             direction = "both")

library(MuMIn)
m_full <- lm(data = d_new,
             relBeak ~ logRS + relTarsus +
               Migration + Trophic.Level +
               Primary.Lifestyle,
             na.action = na.fail)

mods <- dredge(m_full)
mods
mods.avg <- summary(model.avg(mods, subset = delta <= 4, fit = TRUE))
mods.avg$msTable
confint(mods.avg)
plot(mods.avg, full = TRUE)
