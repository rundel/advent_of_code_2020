library(tidyverse)

## Part 1

parse_rules = function(file) {
  readr::read_file(file) %>%
    str_trim() %>%
    str_split("\n") %>%
    .[[1]] %>%
    str_match("(\\w+ \\w+) bags contain (.*)") %>%
    { setNames(as.list(.[,3]), .[,2]) } %>%
    purrr::map(
      function(x) {
        if (x == "no other bags.")
          return(NA)

        str_remove_all(x, " bags?\\.?") %>%
          str_split(", ") %>%
          .[[1]] %>%
          str_match("(\\d+) (.*)") %>%
          { setNames(.[,2], .[,3]) }
      }
    )
}


f = function(file) {

  rules = parse_rules(file)

  can_has_gold = function(color, wanted = "shiny gold") {
    possible_colors = names(rules[[color]])

    #cat(color, "->", possible_colors, "\n")

    if (any(is.na(possible_colors)))
      return(FALSE)

    if (wanted %in% possible_colors)
      return(TRUE)

    for(color in possible_colors) {
      if (can_has_gold(color, wanted))
        return(TRUE)
    }

    return(FALSE)
  }

  purrr::map_lgl(names(rules), can_has_gold)
}


f("day07/test.txt") %>% sum()
f("day07/input.txt") %>% sum()


## Part 2


g = function(file) {
  rules = parse_rules(file)

  count_bags = function(color) {
    next_color = names(rules[[color]])
    n = as.numeric(rules[[color]])

    if (any(is.na(n))) {
      return(1)
    }

    sum(n * purrr::map_dbl(next_color, count_bags)) + 1
  }

  count_bags("shiny gold")-1
}

g("day07/test.txt")
g("day07/test2.txt")
g("day07/input.txt")
