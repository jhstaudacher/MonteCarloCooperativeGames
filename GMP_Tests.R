#
library(gmp)

a <- as.bigz(2)
b <- a^100
print(b)

r <- urand.bigz(nb=1, size=200)
print(r)

q <- as.bigq(r)
print(q)
q <- q / urand.bigz(nb=1)
print(q)

n <- as.bigz("1076929543711580542541091315755961471200577763675069272740403")
l <- ceiling(log2.bigz(n))
r <- urand.bigz(nb=1, size=l)
r <- as.bigq(r)
ratio <- r / as.bigz(2)^l # maybe -1?
result <- round0(ratio * n)
print(result)

leftPadZeros <- function(s, len) {
  required_zeros_count <- len - nchar(s)
  required_zeros <- paste0(rep("0", required_zeros_count), collapse="")
  paste0(required_zeros, s)
}

bigz_fromICoalitionToCoalition <- function(n, index) {
  bin_str <- as.character(as.bigz(index - 1), b=2)
  bin_str <- leftPadZeros(bin_str, n)
  bits <- as.integer(strsplit(bin_str, "")[[1]])
  rev(bits)
}

bigz_fromICoalitionToCoalition2 <- function(n, index) {
  bin_str <- as.character(as.bigz(index - 1), b=2)
  bits <- as.integer(strsplit(bin_str, "")[[1]])
  # left pad with zeros then reverse
  rev(c(rep(0, n - length(bits)), bits))
}

sample(seq.int(1, 2^58), 1)
