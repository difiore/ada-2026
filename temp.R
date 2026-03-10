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

# Maximum Likelihood

# What is the relative likelihood of drawing the set of three numbers 41, 70, and 10 from a normal distribution with $\mu$ = 50 and $\sigma$ = 10?

val <- c(41, 70, 10)
mean <- 65
sd <- 10
(l <- dnorm(val, mean, sd)) # vector of likelihoods of each value
(l[1] * l[2] * l[3]) # product of likelihoods
(ll <- log(l)) # log likelihoods of each value
(ll <- sum(ll)) # summed log likelihood
(l <- exp(ll)) # convert back to likelihood

# sample 100 numbers from a normal dist with mean 50 and sd 10

set.seed(0)
d <- rnorm(100, 50, 10)

# What are the log likelihood and likelihood of drawing the sample this particular sample from this normal distribution?
mean <- 50
sd <- 10
l <- dnorm(d, mean, sd)
(l <- sum(l))
(ll <- log(sum(l)))
(l <- exp(ll))

# How do these compare to the log likelihood and likelihood of that same sample being drawn from a normal distribution with mean = 65 and sd = 20?

mean <- 65
sd <- 20
l <- dnorm(d, mean, sd)
(l <- sum(l)) # lower likelihood!
(ll <- log(sum(l)))
(l <- exp(ll))

# What is the MLE for mean and sigma given this d?

library(bbmle)
minuslogl <- function(mu, sigma, verbose = TRUE) {
  ll = sum(dnorm(d, mean = mu, sd = sigma, log = TRUE))
  nll <- -1 * ll
  return(nll)
}

m <- mle2(
  minuslogl = minuslogl,
  start  = list(mu = 0, sigma = 1),
  method = "SANN" # simulated annealing method of optimization, one of several options
)

summary(m)

m <- mle2(
  minuslogl = minuslogl,
  start  = list(mu = 0, sigma = 1),
  method = "SANN" # simulated annealing method of optimization, one of several options
)

summary(m)

# or

logl <- function(parameters) {
  # `parameters` is a vector of parameter values
  mu <- parameters[1]
  sigma <- parameters[2]
  ll = sum(dnorm(d, mean = mu, sd = sigma, log = TRUE))
  return(ll)
}

library(maxLik)

m <- maxLik(
  logLik = logl,
  start = c(mu=0, sigma=1),
  method = "NM"
)

summary(m)

# larger sample size?
set.seed(0)
d <- rnorm(1000, 50, 10)
