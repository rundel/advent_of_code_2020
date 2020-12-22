library(tidyverse)

file = "day22/test.txt"

parse = function(file) {
  read_file(file) %>%
    str_trim() %>%
    str_split("\n\n", simplify = TRUE) %>%
    str_split("\n") %>%
    {setNames(
      map(., ~ as.integer(.x[-1])),
      map_chr(., ~ str_remove_all(.x[1], "[ :]"))
    )}
}

score = function(d, winner = NULL) {
  if (is.null(winner))
    i = which(map_int(d, length) != 0)
  else
    i = winner

  sum( d[[i]] * rev(seq_along(d[[i]])) )
}

## Part 1

f = function(file) {
  d = parse(file)

  while(!any(map_int(d, length) == 0)) {
    if (d[[1]][1] > d[[2]][1]) {
      win = 1
    } else if (d[[1]][1] < d[[2]][1]) {
      win = 2
    } else {
      stop("Tie!")
    }
    lose = win %% 2 + 1

    d[[win ]] = c(d[[win ]][-1], d[[win]][1], d[[lose]][1])
    d[[lose]] =   d[[lose]][-1]

    #print(d)
  }

  d
}

f("day22/test.txt") %>% score()
f("day22/input.txt") %>% score()


## Part 2

rec_combat = function(d, level=1) {
  prev_decks = list()

  cur_level = c(level, 1)

  while(!any(map_int(d, length) == 0)) {

    if (any(map_lgl(prev_decks, identical, y = d))) {
      return(
        list(winner = 1, deck = d)
      )
    }

    p1 = d[[1]][1]
    p2 = d[[2]][1]
    l1 = length(d[[1]])
    l2 = length(d[[2]])

    if (p1 <= l1 - 1 & p2 <= l2 - 1) {
      sub_deck = list(
        Player1 = d[[1]][1+seq_len(p1)],
        Player2 = d[[2]][1+seq_len(p2)]
      )

      cat("Starting game ", cur_level, "\n")
      win = rec_combat(sub_deck, cur_level)$winner
      cur_level[length(cur_level)] = cur_level[length(cur_level)] + 1

    } else if (p1 > p2) {
      win = 1
    } else if (p1 < p2) {
      win = 2
    } else {
      stop("Tie!")
    }
    lose = win %% 2 + 1

    prev_decks[[length(prev_decks) + 1]] = d

    d[[win ]] = c(d[[win ]][-1], d[[win]][1], d[[lose]][1])
    d[[lose]] =   d[[lose]][-1]
  }

  list(
    winner = which(map_int(d, length) != 0),
    deck = d
  )
}

g = function(file) {
  d = parse(file)

  rec_combat(d)
}


g("day22/test2.txt")
g("day22/test.txt") %>% {score(.$deck, .$winner)}

# Slow but it gets there eventually
g("day22/input.txt") %>% {score(.$deck, .$winner)}
