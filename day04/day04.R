library(tidyverse)

test = "day04/test.txt"
input = "day04/input.txt"

## Part 1

parse = function(file, req = c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")) {
  readr::read_file(file) %>%
    str_trim() %>%
    str_split("\n\n") %>%
    .[[1]] %>%
    str_split(" |\n") %>%
    purrr::map(str_split, ":") %>%
    purrr::map(
      function(x) {
        map_chr(x, 2) %>%
          set_names(map(x, 1))
      }
    )
}

is_valid_format = function(file, req = c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")) {
  file %>%
    parse() %>%
    purrr::map_lgl(
      ~ all(req %in% names(.x)) & all(names(.x) %in% c(req, "cid"))
    )
}

sum( is_valid_format(test) )
sum( is_valid_format(input) )

## Part 2

is_valid_value = function(key, val) {
  #print(c(key, val))
  if (key == "byr") {      # (Birth Year) - four digits; at least 1920 and at most 2002.
    val = as.integer(val)
    (val >= 1920 & val <= 2002)
  }
  else if (key == "iyr") { # (Issue Year) - four digits; at least 2010 and at most 2020.
    val = as.integer(val)
    (val >= 2010 & val <= 2020)
  }
  else if (key == "eyr") { # (Expiration Year) - four digits; at least 2020 and at most 2030.
    val = as.integer(val)
    (val >= 2020 & val <= 2030)
  }
  else if (key == "hgt") { # (Height) - a number followed by either cm or in:
    units = substr(val, nchar(val)-1, nchar(val))
    val = as.integer(substr(val, 1, nchar(val)-2))
    if      (units == "cm") (val >= 150 & val <= 193)
    else if (units == "in") (val >= 59 & val <= 76)
    else                    (FALSE)
  }
  else if (key == "hcl") { # (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    grepl("#[0-9a-f]{6}", val)
  }
  else if (key == "ecl") { # (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    val %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  }
  else if (key == "pid") { # (Passport ID) - a nine-digit number, including leading zeroes.
    grepl("^\\d{9}$", val)
  }
  else if (key == "cid") { # (Country ID) - ignored, missing or not.
    TRUE
  }
}

is_valid_data = function(file) {
  file %>%
    parse() %>%
    purrr::map_lgl(
      function(entry) {
        all( purrr::map2_lgl(names(entry), entry, is_valid_value) )
      }
    )
}

is_valid_data("day04/invalid.txt")
is_valid_data("day04/valid.txt")


sum( is_valid_format(input) &  is_valid_data("day04/input.txt") )
