# Based on functions from the numbers package - modified to allow usage of higher precision Rmpfr values.

extGCD = function (a, b)  {
  stopifnot(length(a) == 1, floor(a) == ceiling(a),
            length(b) == 1, floor(b) == ceiling(b))
  sign_ab = sign(c(a, b))

  A11 = abs(a); A12 = 1; A13 = 0
  A21 = abs(b); A22 = 0; A23 = 1

  while (A11 * A21 != 0) {
    if (A11 > A21) {
      m = A11 %/% A21

      A11 = A11 - m * A21
      A12 = A12 - m * A22
      A13 = A13 - m * A23
    } else {
      m = A21 %/% A11

      A21 = A21 - m * A11
      A22 = A22 - m * A12
      A23 = A23 - m * A13
    }
  }
  if (A11 == 0)
    g <- c(A21, A22, A23)
  else
    g <- c(A11, A12, A13)

  c(1, sign(c(a, b))) * g
}

modinv = function (n, m) {
  v = extGCD(n, m)
  if (v[1] == 0 || v[1] > 1)
    NA
  else if (v[2] >= 0)
    v[2]
  else
    v[2] + m
}

chinese = function(a, m, prec_bits = 128) {
  m = Rmpfr::mpfr(d$bus, precBits = prec_bits)

  n = length(m)
  M = prod(m)

  x = Rmpfr::mpfr(0, precBits = prec_bits)
  for (i in 1:n) {
    Mmi = prod(m[-i])
    mmi = modinv(Mmi, m[i])
    x = x + a[i] * Mmi * mmi
  }
  x %% M
}
