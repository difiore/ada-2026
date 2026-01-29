library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/CPDS-1960-2014-reduced.csv"
cpds <- read_csv(f, col_names = TRUE)

# https://data.austintexas.gov/Utilities-and-City-Services/Austin-311-Public-Data/xwdj-i9he/about_data

f <-"https://raw.githubusercontent.com/difiore/ada-datasets/main/Austin_311_Public_Data_20251230_small.csv"
austin311 <- read_csv(f, col_names = TRUE)

f <- "https://utexas.instructure.com/courses/1438384/files/folder/Course%20Media?preview=89344161"

path <- "/Users/ad26693/Desktop/img.png"

# Download the file
download.file(url = f, destfile = path)
