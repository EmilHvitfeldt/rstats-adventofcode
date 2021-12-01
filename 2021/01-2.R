input <- scan("2021/01-input")

sum3 <- slider::slide_dbl(input, mean, .before = 2)

sum(diff(sum3) > 0)

# Simpler solution thanks to https://twitter.com/trang1618
# This works because `lag` happens to use the same direction
# as `.before`
#
# sum(diff(input, lag = 3) > 0)
