library(tidyverse)
library(manipulate)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"

d <- read_csv(f, col_names = TRUE)
names(d)

cov <- sum(((d$height-mean(d$height)) * (d$weight-mean(d$weight)))/(nrow(d)-1))
cov(d$height,d$weight)
cor <- cov/(sd(d$height)*sd(d$weight))

plot(d$weight, d$height)


temp <- d |> mutate(
  centered_height = height - mean(height),
  centered_weight = weight - mean(weight))

slope.test <- function(beta1, data){
  g <- ggplot(data=data, aes(x = centered_weight, y = centered_height))
  g <- g + geom_point()
  g <- g + geom_abline(intercept = 0, slope = beta1, linewidth = 1, colour="blue", alpha=1/2)
  ols <- sum((data$centered_height - beta1 * data$centered_weight) ^2)
  g <- g + ggtitle(paste("Slope = ", beta1, "\nSum of Squared Deviations = ", round(ols, 3)))
  g
}

manipulate(slope.test(beta1, data=temp),
           beta1 = slider(-1, 1, initial = 0, step = 0.005))

m <- lm(height ~ weight, data = d)
summary(m)

beta1 <- cov(d$height, d$weight) / var(d$weight)

beta0 <- mean(d$height) - beta1 * mean(d$weight)


f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/comparative_primate_sexuality_data.csv"

d <- read_csv(f, col_names = TRUE)
names(d)
ggplot(data = d,
        aes(x=log(Body_mass_male_mean), y = log(Combined_testis_mass))
       ) +
  geom_point(na.rm = TRUE)

m <- lm(log(Combined_testis_mass) ~ log(Body_mass_male_mean), data = d)
summary(m)
names(m)

library(lmodel2)
m2 <- lmodel2(log(Combined_testis_mass) ~ log(Body_mass_male_mean), data = d, range.y =" relative", range.x = "relative", nperm = 1000)
m2
