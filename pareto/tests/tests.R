library(pareto)
# test dpareto
stopifnot(all.equal(dpareto(3, -2, 1), NaN))
stopifnot(all.equal(dpareto(3, 2, -1), NaN))
stopifnot(all.equal(dpareto(3, 2, 1), 0.2222222222))
stopifnot(all.equal(dpareto(1, 2, 3), 0))
stopifnot(all.equal(dpareto(3:5, 2, 1), c(0.2222222222, 0.125, 0.08)))
stopifnot(all.equal(dpareto(1:5, 2, 1), c(0, 0, 0.2222222222, 0.125, 0.08)))
stopifnot(all.equal(dpareto(6, 2:4, 1), 
                    c(0.05555555556, 0.08333333333, 0.11111111111)))
stopifnot(all.equal(log(dpareto(1:5, 2, 1)), dpareto(1:5, 2, 1, log = TRUE)))
stopifnot(all.equal(dpareto(1:6, 1:2, 1), 
                    c(0, 0, 0.11111111111, 0.125, 0.04, 0.05555555556)))
stopifnot(all.equal(dpareto(1, 2, 1:2), c(0, 0)))

# test ppareto
stopifnot(all.equal(ppareto(3, -2, 1), NaN))
stopifnot(all.equal(ppareto(3, 2, -1), NaN))
stopifnot(all.equal(ppareto(3, 2, 1), 0.33333333333))
stopifnot(all.equal(ppareto(1, 2, 3), 0))
stopifnot(all.equal(ppareto(3:5, 2, 1), c(0.33333333333, 0.5, 0.6)))
stopifnot(all.equal(ppareto(1:5, 2, 1), c(0, 0, 0.33333333333, 0.5, 0.6)))
stopifnot(all.equal(ppareto(1:5, 2, 1, lower.tail = FALSE), 
                    c(1, 1, 0.66666666667, 0.5, 0.4)))
stopifnot(all.equal(ppareto(6, 2:4, 1), c(0.66666666667, 0.5, 0.33333333333)))
stopifnot(all.equal(log(ppareto(1:5, 2, 1)), ppareto(1:5, 2, 1, log.p = TRUE)))
stopifnot(all.equal(log(ppareto(1:5, 2, 1, lower.tail = FALSE)), 
                    ppareto(1:5, 2, 1, lower.tail = FALSE, log.p = TRUE)))
stopifnot(all.equal(ppareto(1:6, 1:2, 1), 
                    c(0, 0, 0.66666666667, 0.5, 0.8, 0.66666666667)))
stopifnot(all.equal(ppareto(1, 2, 1:2), c(0, 0)))

# test qpareto
stopifnot(all.equal(qpareto(0.5, -2, 1), NaN))
stopifnot(all.equal(qpareto(0.5, 2, -1), NaN))
stopifnot(all.equal(qpareto(3, 2, 1), NaN))
stopifnot(all.equal(qpareto(-1, 2, 1), NaN))
stopifnot(all.equal(qpareto(0.5, 2, 1), 4))
stopifnot(all.equal(qpareto(1, 2, 3), Inf))
stopifnot(all.equal(qpareto(0, 2, 3), 2))
stopifnot(all.equal(qpareto(seq(0, 1, 0.2), 2, 1), 
                    c(2, 2.5, 3.33333333333, 5, 10, Inf)))
stopifnot(all.equal(qpareto(seq(0, 1, 0.2), 2, 1, lower.tail = FALSE), 
                    c(Inf, 10, 5, 3.33333333333, 2.5, 2)))
stopifnot(all.equal(qpareto(0.2, 2:4, 1), c(2.5, 3.75, 5)))
stopifnot(all.equal(qpareto(seq(0, 1, 0.2), 2, 1), 
                    qpareto(log(seq(0, 1, 0.2)), 2, 1, log.p = TRUE)))
stopifnot(all.equal(qpareto(seq(0, 1, 0.2), 2, 1, lower.tail = FALSE), 
                    qpareto(log(seq(0, 1, 0.2)), 2, 1, 
                            lower.tail = FALSE, log.p = TRUE)))
stopifnot(all.equal(qpareto(seq(0, 1, 0.2), 1:2, 1), 
                    c(1, 2.5, 1.66666666667, 5, 5, Inf)))
stopifnot(all.equal(qpareto(0, 2, 1:2), c(2, 2)))