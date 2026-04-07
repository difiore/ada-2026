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
