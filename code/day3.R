library(tidyverse)
library(testthat)

read_data <- function(input_file) {
  read_lines(input_file) |>
    str_split("") |>
    lapply(as.numeric)
}

# Part 1

test_dat <- read_data("inputs/day3_test.txt")
dat <- read_data("inputs/day3.txt")

find_voltage <- function(bank, combn) {
  # browser()
  pair_list <- combn(bank, combn, simplify = FALSE)
  pair_list_chars <- lapply(pair_list, \(p) {
    as.numeric(paste0(p, collapse = ""))
  }) |> unlist() |> max()
}

part1 <- function(d) {
  voltages <- lapply(d, find_voltage, combn = 2) |> unlist()
  # print(voltages)
  sum(voltages)
}

part1(test_dat)
test_that("Test data", expect_equal(part1(test_dat), 357))
part1(dat)

# Part 2

parse_voltage <- \(x) paste0(x, collapse = "") |> as.numeric()

find_voltage2 <- function(bank, l_voltage = 12) {
  
  # Get length of bank
  l_bank <- length(bank)

  # Start with first element
  voltage <- bank[1]
  bank <- bank[-1]

  while (length(bank) > 0) {

    greater <- which(bank[1] > voltage)

    if (length(greater) > 0) {
      while (length(greater > 1)) {
        if((l_voltage - greater[1]) < length(bank)) {
          voltage <- append(voltage[0:(greater[1]-1)], bank[1])
          break
        } else {
          if (length(greater) == 1) {
            voltage <- append(voltage, bank[1])
          }
        }
        greater <- greater[-1]
      }
    } else {
      voltage <- append(voltage, bank[1])
    }

    # Remove from bank
    bank <- bank[-1]
  }

  return(parse_voltage(voltage[1:l_voltage]))
}

part2 <- function(d) {
  voltages <- lapply(d, find_voltage2) |> unlist()
  sum(voltages)
}

test_that("Test data", expect_equal(part2(test_dat), 3121910778619))
part2(dat) |> as.character()
