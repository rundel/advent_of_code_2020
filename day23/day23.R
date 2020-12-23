library(tidyverse)

test = "389125467"
input = "496138527"

wrap = function(x, n) {
  (x %% (n+1))+ (x %/% (n+1))
}

f = function(data, moves, pad = NULL) {
  d = str_split(data, "")[[1]] %>% as.integer()

  if (!is.null(pad)) {
    d = c(d, (max(d)+1):pad )
  }

  n = length(d)

  cur_cup_i = 1
  cur_cup_val = d[1]

  for(i in seq_len(moves)) {
    #cat("Move ", i, "\n")
    #cat("Cur : ",  cur_cup_val, "\n")
    #cat("Cups: ", d, "\n")

    sel_i = wrap(cur_cup_i + 1:3, n)

    # Pick up
    sel = d[sel_i]
    d = d[-sel_i]

    #cat("Pick up: ", sel, "\n")

    # Find Dest
    dest_cup_val = d[d < cur_cup_val]
    if (length(dest_cup_val) == 0) {
      dest_cup_val = max(d)
    } else {
      dest_cup_val = max(dest_cup_val)
    }

    dest_cup_i = which(d == dest_cup_val)

    #cat("Dest: ", dest_cup_val, "\n\n")

    # Insert
    d = c(d[seq_len(dest_cup_i)], sel, d[-seq_len(dest_cup_i)])

    cur_cup_i = which(d == cur_cup_val)
    cur_cup_i = wrap(cur_cup_i+1, n)
    cur_cup_val = d[cur_cup_i]


    #d = if (d[1] == 1) {
    #  d
    #} else {
    #  i = which(d == 1) - 1
    #  c(d[-seq_len(i)], d[seq_len(i)])
    #}
    #print(d)
  }

  res = if (d[1] == 1) {
    d
  } else {
    i = which(d == 1) - 1
    c(d[-seq_len(i)], d[seq_len(i)])
  }

  paste(res[-1], collapse = "")
}

f(test, 10)
f(test, 100)



## Part 2


g_too_slow = function(data, moves, n_cups = length(vals)) {

  to_vals = function(x) {
    seq_along(x)[order(x)]
  }


  vals = str_split(data, "")[[1]] %>% as.integer()

  stopifnot(n_cups >= length(vals))

  d = seq_len(n_cups)
  d[vals] = seq_along(vals)

  n = length(d)

  cur_cup_i = 1
  cur_cup_val = which(d == 1)

  for(i in seq_len(moves)) {
    #cat("Move ", i, "\n")
    #cat("Cur : ",  cur_cup_val, "\n")
    #cat("Cups: ", to_vals(d), "\n")

    # Pick up
    sel_i = wrap(cur_cup_i + 1:3, n)
    sel = c(
      which(d == sel_i[1]),
      which(d == sel_i[2]),
      which(d == sel_i[3])
    )

    #cat("Pick up: ", sel, "\n")


    # Find Dest
    dest_cup_val = cur_cup_val-1
    while(dest_cup_val %in% c(0,sel)) {
      if (dest_cup_val == 0)
        dest_cup_val = n
      else
        dest_cup_val = dest_cup_val - 1
    }

    #cat("Dest: ", dest_cup_val, "\n")


    # Insert

    #cat("d  : ", d, "\n")
    #cat("val: ", to_vals(d), "\n\n")

    d[d > cur_cup_i] = d[d > cur_cup_i] - 3

    dest_cup_i = d[dest_cup_val]

    d[d > dest_cup_i] = d[d > dest_cup_i] + 3
    d[sel] = dest_cup_i + 1:3
    d = wrap(d, n)

    #cat("d  : ", d, "\n")
    #cat("val: ", to_vals(d), "\n\n")

    # Update

    cur_cup_i = wrap(d[cur_cup_val] + 1, n)
    cur_cup_val = which(d == cur_cup_i)

    # Status
    #print(to_vals(d))
  }

  d
}




g = function(data, moves, n_cups = 0) {
  vals = str_split(data, "")[[1]] %>% as.integer()

  if (length(vals) < n_cups)
    vals = c(vals, (max(vals)+1):n_cups)

  to_vals = function(x, n = length(x)) {
    j = 1
    for(i in seq_len(n)) {
      cat(j, " ", sep="")
      j = x[j]
    }
    cat("\n")
  }

  # d is constructed such that d[i] is the value that follows i in the circle

  new = lead(vals)
  new[length(vals)] = vals[1]

  d = seq_along(vals)
  d[vals] = new

  n = length(d)

  cur = vals[1]

  for(i in seq_len(moves)) {
    #cat("Move ", i, "\n")
    #cat("Cur : ",  cur, "\n")
    #cat("Cups: ")
    #to_vals(d)

    sel1 = d[cur]
    sel2 = d[sel1]
    sel3 = d[sel2]

    #cat("pick up: ", sel1, sel2, sel3, "\n")

    d[cur] = d[sel3]

    dest = cur-1
    while(dest %in% c(0, sel1, sel2, sel3)) {
      if (dest == 0)
        dest = n
      else
        dest = dest - 1
    }

    #cat("Dest: ", dest, "\n\n")

    end = d[dest]
    d[dest] = sel1
    d[sel3] = end

    cur = d[cur]
    #to_vals(d)
  }

  c(d[1], d[ d[1] ])
}

g(test, 1e7, 1e6)
g(input, 1e7, 1e6) %>% prod() %>% formatC(digits=16)
