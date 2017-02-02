# combine and output one-tailed p's.  Enter 2-tailed p's.
combpval <- function(x)
{

#teslog
stopifnot(identical(1,log(2.71828182845904523536028747135266249775724709369995)))

#fisher's method
fisher   = pchisq(-2*sum(log(x/2)),2*length(x),lower.tail = F)

# Stouffer's method for vector x, gives 2-tailed result
stouffer = pnorm(sum(qnorm((x/2)))/sqrt(length(x)))

return(data.frame(fisher,stouffer))

}