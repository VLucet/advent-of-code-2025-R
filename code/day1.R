library(tidyverse)

# PART 1

dat_test <- read_csv("inputs/day1_test.txt", col_names = F) |>
  set_names("inst")

dat_test_processed <- dat_test |>
  mutate(dir = str_sub(inst, 0, 1), 
         num = as.numeric(str_sub(inst, 2)), 
         count = ifelse(dir == "L", -num, num))

total <- 50
password <- 0
for (i in 1:length(dat_test_processed$count)) {
  total = (total + dat_test_processed$count[i])%%100
  if (total == 0) password + 1 -> password
  print(total)
}
total
password

dat <- read_csv("inputs/day1.txt", col_names = F) |>
  set_names("inst")

dat_processed <- dat |>
  mutate(dir = str_sub(inst, 0, 1), 
         num = as.numeric(str_sub(inst, 2)), 
         count = ifelse(dir == "L", -num, num))

total <- 50
password <- 0
for (i in 1:length(dat_processed$count)) {
  total = (total + dat_processed$count[i])%%100
  if (total == 0) password + 1 -> password
  print(total)
}
total
password

# PART 2
  
compute_password <- function(vals) {
  
  total <- 50
  password <- 0

  for (i in 1:length(vals)) {

    print(sprintf("Total is %s", total))
    
    val <- vals[i]
    print(sprintf("Val is %s", val))
    
    val_quotient <- floor(abs(val) / 100)
    print(sprintf("Val quotient is %s", val_quotient))
    print(sprintf(" + %s", abs(val_quotient)))
    password <- password + abs(val_quotient)

    val_remainder <- (abs(val) %% 100) * sign(val)
    print(sprintf("Val remainder is %s", val_remainder))

    remainder <- (total + val_remainder) %% 100
    print(sprintf("Remainder is %s", remainder))
    if (remainder == 0) {
      print("+ 1 !")
      password + 1 -> password
    }

    quotient <- (total + val_remainder) %/% 100
    print(sprintf("Quotient is %s", quotient))
    
    p <- ifelse((remainder == 0 || total == 0) && abs(quotient) >= 1, 1, 0)
    print(sprintf("Penalty is %s", p))
    print(sprintf(" + %s", abs(quotient) - p))
    password + abs(quotient) - p -> password

    total <- remainder
    print("****")
    
  }
  
  return(password)
}

compute_password(dat_test_processed$count)
compute_password(dat_processed$count)
