# Model Selection
library(tidyverse)
library(broom)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)

# or
library(avonet)
d <- traitdata

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

vars <- c("logBeak", "logRS", "Migration")
d_new <- d |> drop_na(vars)

m1 <- lm(data = d_new, logBeak ~ logRS * Migration) # full model
m2 <- lm(data = d_new, logBeak ~ logRS + Migration) # model without interaction
m3 <- lm(data = d_new, logBeak ~ logRS) # model with one predictor
m4 <- lm(data = d_new, logBeak ~ Migration) # model with one predictor
m5 <- lm(data = d_new, logBeak ~ 1) # intercept only model

f <- anova(m2, m1, test = "F")
f

p <- pf(5.5055, df1 = 4-2, df2 = 10937-4-1, lower.tail = FALSE)

f <- anova(m3, m2, test = "F")
tidy(f)
p <- pf(47.032, df1 = 3-1, df2 = 10937-2-1, lower.tail = FALSE)

vars <- c("relBeak", "logRS", "Migration", "Trophic.Level", "relTarsus", "Primary.Lifestyle")
d_new <- d |>
  drop_na(vars)

m_full <- lm(relBeak ~ logRS + Migration + Trophic.Level + relTarsus + Primary.Lifestyle, data = d_new)

m_null <- lm(data = d_new, relBeak ~ 1)
add1(m_null, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

m1 <- update(m_null, formula = .~. + Primary.Lifestyle)
add1(m1, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

m2 <- update(m1, formula = .~. + Trophic.Level)
add1(m2, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

m3 <- update(m2, formula = .~. + Migration)
add1(m3, scope = .~. + logRS + relTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")

drop1(m_full, test = "F")

m1 <- update(m_full, formula = .~. - logRS)
drop1(m1, test = "F")

m2 <- update(m1, formula = .~. - relTarsus)
drop1(m2, test = "F")

library(MASS)
m_full <- lm(data = d,
             relBeak ~ logRS + relTarsus +
               Migration + Trophic.Level +
               Primary.Lifestyle)

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
