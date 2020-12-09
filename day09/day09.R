library(tidyverse)

file = "day09/test.txt"
preamb = 5


f = function(file, preamb) {
  nums = readLines(file) %>%
    as.numeric()

  for(i in (preamb+1):length(nums)) {
    val = nums[i]
    p = nums[(i-preamb):(i-1)]

    check = expand.grid(x=p, y=p) %>%
      {.$x + .$y} %>%
      {. == val}

    if (!any(check))
      return(nums[i])
  }
  stop("oops")
}

(val_test = f("day09/test.txt", 5))
(val_input = f("day09/input.txt", 25))

## Part 2

g = function(file, val) {
  nums = readLines(file) %>%
    as.numeric()

  n = length(nums)

  i = 1
  j = 2
  sum = sum(nums[i:j])

  while(j < n) {
    if (sum == val) {
      break
    } else if (sum > val) {
      sum = sum - nums[i]
      i = i+1
    } else if (sum < val) {
      sum = sum + nums[j+1]
      j = j+1
    }

  }

  print(min(nums[i:j]) + max(nums[i:j]))
}

g("day09/test.txt", val_test)
g("day09/input.txt", val_input)
