library(tidyverse)

test = c(0,3,6)
input = c(7,12,1,0,16,2)

## Part 1

f = function(x, end = 2020) {
  res = c(x, rep(NA, end-length(x)))

  for (i in seq(length(x)+1, end)) {
    j = which(res[i-1] == res[1:(i-2)])

    if (length(j) == 0)
      res[i] = 0
    else
      res[i] = i-1 - max(j)
  }

  res
}

f(test, 10) %>% tail(10)
f(test) %>% tail(10)

f(input)

f(input, 1e4) %>% plot()


## Part 2

# Works but too slow
#
# g = function(x, end = 2020) {
#   res = list()
#   for(i in 1:(length(x)-1)) {
#     res[[as.character(x[i])]] = i
#   }
#
#   cur_val = as.character( x[length(x)] )
#
#   for (i in seq(length(x), end-1)) {
#     if (is.null(res[[cur_val]])) {
#       new_val = 0
#     } else {
#       new_val = i - res[[cur_val]]
#     }
#
#     res[[cur_val]] = i
#
#     cur_val = as.character(new_val)
#   }
#
#   cur_val
# }

g2 = function(x, end = 2020) {
  res = integer()
  for(i in 1:(length(x)-1)) {
    res[ x[i] + 1 ] = i
  }

  cur_val = x[length(x)]

  for (i in seq(length(x), end-1)) {
    if (is.na(res[cur_val + 1])) {
      new_val = 0
    } else {
      new_val = i - res[cur_val + 1]
    }

    res[cur_val + 1] = i
    cur_val = new_val
  }

  cur_val
}

g2(test, 30000000)
g2(input, 30000000)

