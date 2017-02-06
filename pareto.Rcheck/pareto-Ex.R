pkgname <- "pareto"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pareto')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("dpareto")
### * dpareto

flush(stderr()); flush(stdout())

### Name: dpareto
### Title: The Pareto Distribution
### Aliases: dpareto
### Keywords: distribution

### ** Examples

dpareto(4, 2, 1)
dpareto(1:5, 2, 1)
dpareto(1:5, 2, 1, log = TRUE)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
