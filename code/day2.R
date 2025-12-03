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

part1 <- function(d) {
  
  d_num <- lapply(d, as.numeric)
  ranges <- lapply(d_num, \(x) x[1]:x[2])

  is_invalid <- function(x) {
    x_str <- as.character(x)
    p1 <- str_sub(x_str, start = 0, end = floor(nchar(x_str)/2))
    p2 <- str_sub(x_str, start = floor(nchar(x_str)/2)+1, end = nchar(x))
    eq <- p1 == p2
    return (eq)
  }

  invalid_sum <- lapply(ranges, \(range) {
    sum(range[sapply(range, is_invalid)], na.rm=T)
  })

  return(sum(unlist(invalid_sum), na.rm=T))
}

part1(test_dat)
test_that("Test data", expect_equal(part1(test_dat), 1227775554))
part1(dat)

# Part 2

part2 <- function(d) {
  
  d_num <- lapply(d, as.numeric)
  ranges <- lapply(d_num, \(x) x[1]:x[2])

  is_invalid <- function(x) {
    # x_str <- as.character(x)
    # p1 <- str_sub(x_str, start = 0, end = floor(nchar(x_str)/2))
    # p2 <- str_sub(x_str, start = floor(nchar(x_str)/2)+1, end = nchar(x))
    # eq <- p1 == p2
    # return (eq)
  }

  invalid_sum <- lapply(ranges, \(range) {
    sum(range[sapply(range, is_invalid)], na.rm=T)
  })

  return(sum(unlist(invalid_sum), na.rm=T))
}

test_that(part2(test_dat), 4174379265)
part2(dat)

seq(from = 0, by = 2, to = 11-2)
seq(from = 2, by = 2, to = 11)
