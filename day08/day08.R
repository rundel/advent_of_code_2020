library(tidyverse)

file = "day08/test.txt"


parse = function(file) {
  readr::read_delim(file, delim =  " ", col_names = c("inst", "value")) %>%
    mutate(visits = 0)
}

run = function(d) {
  cur = 1

  accumulator = 0

  while (d$visits[cur] == 0 & cur <= nrow(d)) {
    d$visits[cur] = d$visits[cur] + 1


    if (d$inst[cur] == "nop") {
      cur = cur + 1
    } else if (d$inst[cur] == "acc") {
      accumulator = accumulator + d$value[cur]
      cur = cur + 1
    } else if (d$inst[cur] == "jmp") {
      cur = cur + d$value[cur]
    } else {
      stop("Bad instruction")
    }
  }

  list(acc = accumulator, cur = cur)
}

## Part 1

f = function(file) {
  d = parse(file)
  run(d)$acc
}

f("day08/test.txt")
f("day08/input.txt")


## Part 2

swap = function(x) {
  if (x == "nop") "jmp"
  else if (x == "jmp") "nop"
  else stop("error")
}

g = function(file) {
  d = parse(file)

  for(i in which(d$inst %in% c("nop", "jmp"))) {
    d_fix = d
    d_fix$inst[i] = swap(d_fix$inst[i])

    r = run(d_fix)

    if (r$cur == nrow(d_fix)+1) {
      cat("inst", i, ":", r$acc)
      break
    }
  }
}

g("day08/test.txt")
g("day08/input.txt")
