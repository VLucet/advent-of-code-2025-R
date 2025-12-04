library(tidyverse)
library(testthat)

read_data <- function(input_file) {

}

# Part 1

test_dat <- read_data("inputs/day4_test.txt")
dat <- read_data("inputs/day4.txt")

part1 <- function(d) {

}

part1(test_dat)
test_that("Test data", expect_equal(part1(test_dat), 13))
part1(dat)

# Part 2

part2 <- function(d) {

}

part2(test_dat)
test_that("Test data", expect_equal(part2(test_dat), 0))
part2(dat)