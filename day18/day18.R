library(tidyverse)

## Part 1

# Eval without parens
eval_expr = function(expr, pat = "\\d+ [+*] \\d+") {
  expr = force(expr)

  while(str_detect(expr, pat)) {
    val = str_match(expr, pat) %>%
      parse(text = .) %>%
      eval()

    expr = str_replace(expr, pat, as.character(val))
  }

  expr
}

eval_paren_expr = function(expr) {
  pat = "\\(([^(]+?)\\)"

  while(str_detect(expr, pat)) {
    m = str_match(expr, pat)[,2]
    val = eval_expr(m)

    expr = str_replace(expr, pat, val)
    #print(expr)
  }

   as.numeric(eval_expr(expr))
}

f = function(file) {
  readLines(file) %>%
    purrr::map_dbl(
      eval_paren_expr
    )
}

f("day18/test.txt")
f("day18/input.txt") %>%
  sum() %>%
  formatC(digits=16)


## Part 2

eval_adv_expr = function(expr) {
  expr %>%
    eval_expr("\\d+ [+] \\d+") %>%
    eval_expr("\\d+ [*] \\d+")
}

eval_paren_adv_expr = function(expr) {
  pat = "\\(([^(]+?)\\)"

  while(str_detect(expr, pat)) {
    val = str_match(expr, pat)[,2] %>%
      eval_adv_expr()

    expr = str_replace(expr, pat, val)
    #print(expr)
  }

  expr %>%
    eval_adv_expr() %>%
    as.numeric()
}

g = function(file) {
  readLines(file) %>%
    purrr::map_dbl(
      eval_paren_adv_expr
    )
}

g("day18/test.txt")
g("day18/input.txt") %>%
  sum() %>%
  formatC(digits=16)
