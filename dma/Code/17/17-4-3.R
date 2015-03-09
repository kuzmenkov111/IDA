## error correcting codeword for i out of k
ecc <- function(i, k)
{
  if (i>1)
    c(rev(1-c(ecc(i-1, k-1), 1)), ecc(i-1,k-1))
  else
    rep(1, 2^(k-1)-1)
}

  # error-correcting code for 3 codewords
t(sapply(1:3, ecc, 3))
  # error-correcting code for 4 codewords
t(sapply(1:4, ecc, 4))
  # error-correcting code for 5 codewords
t(sapply(1:5, ecc, 5))
