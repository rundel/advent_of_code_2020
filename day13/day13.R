library(tidyverse)

file = "day13/test.txt"

parse = function(file) {
  d = file %>%
    readLines() %>%
    strsplit(",")

  d[[3]] = seq_along(d[[2]])-1
  d[[3]][ d[[2]] == "x" ] = "x"


  purrr::map(
    d,
    ~ .x[.x != "x"] %>% as.numeric()
  ) %>%
    setNames(c("depart", "bus", "offset"))
}

## Part 1

f = function(file) {
  d = parse(file)

  wait = function(dep, bus) {
    mod = dep %% bus
    if (mod == 0)
      return(0)

    (dep - mod) + bus - dep
  }

  waits = purrr::map_dbl(d$bus, wait, dep = d$depart)

  i = which.min(waits)

  c(d$bus[i], waits[i], d$bus[i] * waits[i])
}

f("day13/test.txt")
f("day13/input.txt")

## Part 2

# Brute force approach
g_bf = function(file, debug = FALSE) {
  d = parse(file)

  i = which.max(d$bus)
  val = d$bus[i]
  offs = d$offset - d$offset[i]

  dig = 1
  repeat {
    if (debug)
      cat(val, "\n")
    if (all((val + offs) %% d$bus < 1e-6)) {
      break
    }
    val = val + d$bus[i]
  }

  print(val - d$offset[i])
}

g_bf("day13/test.txt")
g_bf("day13/test2.txt")
g_bf("day13/test3.txt")
g_bf("day13/test4.txt")
g_bf("day13/test5.txt")
g_bf("day13/test6.txt")

# Too slow
#g_bf("day13/input.txt")

# Use chinese remainder solver from numbers

g_crt = function(file) {
  d = parse(file)

  numbers::chinese(
    a = -d$offset,
    m = d$bus
  )
}

g_crt("day13/test.txt")
g_crt("day13/test2.txt")
g_crt("day13/test3.txt")
g_crt("day13/test4.txt")
g_crt("day13/test5.txt")
g_crt("day13/test6.txt")

# Incorrect due to precision issues
g_crt("day13/input.txt") %>% formatC(digits=16)


# Using 128 bit floats instead

g_crt2 = function(file) {
  d = parse(file)

  # Modified version of the function that allows the use of Rmpfr values
  source("day13/chinese.R")

  chinese(
    a = -d$offset,
    m = d$bus
  )
}

g_crt2("day13/input.txt")
