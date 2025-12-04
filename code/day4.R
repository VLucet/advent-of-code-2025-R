library(tidyverse)
library(testthat)

pad_matrix <- function(m, nrows, ncols) {
  out <- matrix(NA, nrow = nrows, ncol = ncols)
  out[2:(nrow(out)-1), 2:(ncol(out)-1)] <- m
  out
}

read_data <- function(input_file) {
  m <-read_lines(input_file) |>
    str_replace_all(pattern = fixed("."), replacement = "0") |>
    str_replace_all(pattern = fixed("@"), replacement = "1") |>
    str_split("") |> sapply(as.numeric) |> t() 
  pad_matrix(m, nrow(m)+2, ncol(m)+2)
}

# Part 1

test_dat <- read_data("inputs/day4_test.txt")
dat <- read_data("inputs/day4.txt")

get_ngb <- function(x, nc, nr) {
  c(x-1, x+1, 
    x-nc-1, x-nc, x-nc+1, 
    x+nc+1, x+nc, x+nc-1
  )
}

part1 <- function(d) {

  non_NA <- which(d == 1)
  ngbs <- lapply(non_NA, get_ngb, nc = ncol(d), nr = nrow(d))
  vals <- lapply(ngbs, \(x, mat = d) mat[x])
  sums <- lapply(vals, \(x) sum(x, na.rm = T))

  return(sum(sums<4))

}

part1(test_dat)
test_that("Test data", expect_equal(part1(test_dat), 13))
part1(dat)

# Part 2

part2 <- function(d) {
  
  total_sum <- 0
  
  while(TRUE) {

    non_NA <- which(d == 1)
    ngbs <- lapply(non_NA, get_ngb, nc = ncol(d), nr = nrow(d))
    vals <- lapply(ngbs, \(x, mat = d) mat[x])
    sums <- lapply(vals, \(x) sum(x, na.rm = T))
    below_4 <- non_NA[which(sums<4)]
    d[below_4] <- 0

    the_sum <- sum(sums<4)

    total_sum <- total_sum + the_sum

    if (the_sum == 0) break

  }

  return(total_sum)
}

part2(test_dat)
test_that("Test data", expect_equal(part2(test_dat), 43))
part2(dat)
