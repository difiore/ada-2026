v <- 1:10
v <- c(1:10)

m <- matrix(1:30, nrow=5, ncol=6)
# byrow = FALSE by default
m <- matrix(1:30, nrow=5, ncol=6, byrow = TRUE)
a <- array(data=1:90, dim=c(5, 6, 3))
dim(a)
class(a)
attributes(a)
v[3]
v[3:6]
m[3, 5]
m[2:4, 3:5]
a[1,,1]
a[1,1,]
a[,,1]
a[,,3]
s <- c("this", "is", "a", "string", "vector")
# vector of character strings
m <- matrix(data=1:40, nrow=5, ncol=8) # matrix
b <- FALSE # boolean variable
l <- list(s, m, b)
l <- list(string = s, matrix = m, bool = b)
attributes(l)
l[[2]]
l[[2]][2, 6]
l[[2]]
l[2]
df <- data.frame(
  firstName=c("Rick", "Negan", "Maggie", "Michonne"),
  community=c("Alexandria","Saviors","Hilltop","Alexandria"),
  sex=c("M", "M", "F", "F"),
  age=c(42, 40, 33, 28)
)
df
f <- "~/Downloads/CPDS-1960-2014-reduced.csv"
cpds <- read.csv(
  file = f,
  header = TRUE,
  stringsAsFactors = FALSE
)
class(cpds)

library(tidyverse)
f <-
  "https://raw.githubusercontent.com/difiore/ada-datasets/main/Austin_311_Public_Data_20251230_small.csv"
austin311 <- read_csv(file = f, col_names = TRUE)
names(austin311)
austin311[1, 2]
austin311[1, ]
austin311[, 2]
austin311$County


f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/CPDS-1960-2014-reduced.csv"
cpds <- read_csv(f, col_names = TRUE)

# https://data.austintexas.gov/Utilities-and-City-Services/Austin-311-Public-Data/xwdj-i9he/about_data

f <-"https://raw.githubusercontent.com/difiore/ada-datasets/main/Austin_311_Public_Data_20251230_small.csv"
austin311 <- read_csv(f, col_names = TRUE)

f <- "https://utexas.instructure.com/courses/1438384/files/folder/Course%20Media?preview=89344161"

path <- "/Users/ad26693/Desktop/img.png"

# Download the file
download.file(url = f, destfile = path)


library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE) # creates a "tibble"

dim(d)

names(d)

attach(d)
mean(Brain_Size_Female_Mean, na.rm = TRUE)
detach(d)
mean(Brain_Size_Female_Mean, na.rm = TRUE) # throws an error
with(d, mean(Brain_Size_Female_Mean, na.rm = TRUE))
summary(d)
# five-number summary plus mean and number of NAs

library(skimr)
head(skim(d))
tail(skim(d))
boxplot(log(d$Body_mass_female_mean))
boxplot(log(d$Body_mass_female_mean), horizontal = TRUE)
stripchart(log(d$Body_mass_female_mean), col = "blue")
stripchart(log(d$Body_mass_female_mean), col = "blue", vertical = TRUE)
boxplot(log(d$Body_mass_female_mean))
stripchart(log(d$Body_mass_female_mean),
           method = "jitter",
           col = "blue",
           vertical = TRUE,
           add = TRUE)
stripchart(log(d$Body_mass_female_mean),
           method = "jitter",
           col = "blue",
           vertical = TRUE,
           add = TRUE)

boxplot(data = d, log(Body_mass_female_mean) ~ Family)

stripchart(log(d$Body_mass_female_mean), col = "blue", add = TRUE)

stripchart(log(d$Body_mass_female_mean) ~ d$Family,
           method = "jitter",
           col = "blue",
           vertical = TRUE,
           add = TRUE)

p <- ggplot(
  data = d,
  aes(x = Family,
      y = log(Body_mass_female_mean))
  ) +
  geom_boxplot(na.rm = TRUE)
p
p <- p +
  geom_jitter(
    color = "blue",
    width = 0.1
  )
p

p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p + ylab("log(Female Body Mass)")
p

hist(log(d$Body_mass_female_mean))
hist(log(d$Body_mass_female_mean), freq = FALSE)
lines(
  density(log(d$Body_mass_female_mean), na.rm = TRUE),
  col = "blue"
)
abline(v = quantile(
  log(d$Body_mass_female_mean),
  prob = c(0.05, 0.95),
  na.rm = TRUE
),
col = "red")
p <- ggplot(
  data = d,
  aes(log(Body_mass_female_mean))
)
p

(p + geom_histogram(binwidth = 1, center = 0.5))


(p + geom_histogram(bins = 9))
(p + geom_histogram(bins = 9, aes(y = ..density..)))

(p + geom_histogram(bins = 9, aes(y = ..density..)) +
    geom_density())


(p + geom_histogram(bins = 9) +
  geom_density())

(p + geom_histogram(bins = 9, aes(y = ..density..)) +
    geom_density() +
    geom_vline(xintercept =
                 quantile(
                   log(d$Body_mass_female_mean),
                   prob = c(0.05, 0.95),
                   na.rm = TRUE
                 ),
               color = "red"
    )
)

plot(x = log(d$Body_mass_female_mean),
     y = log(d$Brain_Size_Female_Mean))



library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE) # creates a "tibble"

p <- ggplot(data = d,
            aes(x = log(Body_mass_female_mean),
                y = log(Brain_Size_Female_Mean)
               )
           )

p

(p + geom_point(na.rm = TRUE))

# new aesthetic to color points
p <- p + geom_point(aes(color = factor(Family)), na.rm = TRUE)

p

# modify axes
p <- p + xlab("log(Female Body Mass)") +
  ylab("log(Female Brain Size)")
p

# add legend
p <- p + theme(legend.position = "bottom", legend.title = element_blank())

# plot the object
p

(p <- p + geom_smooth(method = "lm", na.rm = TRUE))

library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE) # creates a "tibble"

s <- d[d$Family == "Hominidae" & d$Mass_Dimorphism > 2,]
s
# filter
s <- d[d$Family == "Hominidae" & d$Mass_Dimorphism > 2,]
s <- filter(d, Family == "Hominidae" & Mass_Dimorphism > 2)

# select
s <- d[, c("Family", "Genus", "Body_mass_male_mean")]
dim(s)

s <- select(d, c("Family", "Genus", "Body_mass_male_mean"))

s <- d |>
  select(c("Family", "Genus", "Body_mass_male_mean")) |>
  filter(Family == "Hominidae")

dim(s)


%>%



dim(s)




# order
s <- d[order(d$Family,d$Genus,-d$Body_mass_male_mean),]
s <- arrange(d, Family,Genus,desc(Body_mass_male_mean))

# summarize
s <- aggregate(
  d$Body_mass_female_mean ~ d$Family,
  FUN="mean", na.rm=TRUE)

s <- summarize(
  group_by(d, Family),
  avgF = mean(Body_mass_female_mean, na.rm=TRUE))

# piping
s <- group_by(d, Family) |> # or %>%
  summarise(
    avgF = mean(Body_mass_female_mean, na.rm = TRUE))

s <- d |> # or %>%
  group_by(Family) |> # or %>%
  summarise(
    avgF = mean(Body_mass_female_mean, na.rm = TRUE))

# add variable
d$Taxonomy = paste0(d$Family, "-", d$Genus, "-", d$Species)
d <- d |> mutate(Taxonomy = paste0(Family, "-", Genus, "-", Species))
head(d)


# relocate to front
d <- d |> relocate(Taxonomy)

#relocate to specific position
d <- d |> relocate(Taxonomy, .after = Species)
d <- d |> relocate(Taxonomy, .before = Superfamily)

# relocate using select()
g <- d |> select(Taxonomy, c("Body_mass_female_mean", "Species"))





f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv"
d <- read_csv(f, col_names = TRUE) # creates a "tibble"

names(d)






d$genres

d <- d |> mutate(comedy = if_else(grepl("Comedy", genres), TRUE, FALSE))

d$comedy

d <- d |> mutate(comedy = if_else(str_detect(genres, "Comedy"), TRUE, FALSE))
d <- d |> relocate(comedy, .after = primaryTitle)
s <- d |> summarize(count = n(), comedies = sum(comedy, na.rm = TRUE))

for (i in 1:10) {
  print(i)
}

i <- 1
while (i <= 10) {
  print(i)
  i <- i + 1
}

runtime <- 0

for (i in 1:nrow(d)) {
  runtime <- runtime + d[i,]$runtimeMinutes
}

sum(d$runtimeMinutes, na.rm = TRUE)
runtime <- d |> summarize(sum = sum(runtimeMinutes, na.rm = TRUE))

papers <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/papers.csv"

creators <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/creators.csv"

p <- read_csv(papers, col_names = TRUE)
c <- read_csv(creators, col_names = TRUE)

p <- p |>
  separate_wider_delim(cols = Author,
                       delim = ";",
                       names = c("First Author", "A2", "A3", "A4"),
                       too_few = "align_start", too_many = "drop") |>
  mutate(A2 = str_trim(`A2`, "both"),
         A3 = str_trim(`A3`, "both"),
         A4 = str_trim(`A4`, "both"))

c <- c |>
  distinct()


library(fuzzyjoin)

find_pubs <-
  tibble(name = c("Abbott, David"))

inner <- inner_join(p, find_pubs, by = c("First Author" = "name"))

find_pubs <-
  tibble(partialName = c("^Abbott"))

inner_fuzzy <- regex_inner_join(p, find_pubs, by = c("First Author" = "partialName"))
