library(tidyverse)
library(testthat)

read_data <- function(input_file) {
  read_lines(input_file) |>
    str_split(",") |> unlist() |>
    str_split("-")
}

# Part 1

test_dat <- read_data("inputs/day2_test.txt")
dat <- read_data("inputs/day2.txt")

part1 <- function() {

}

test_that(part1(test_dat), 1227775554)
part1(dat)

# Part 2

part2 <- function() {

}

test_that(part2(test_dat), 0)
part2(dat)