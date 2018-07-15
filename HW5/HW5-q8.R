#HW5-q8:


mydata = matrix(
  c(301,353,558,502,155,153),
  nrow = 3,
  ncol = 2
)

rownames(mydata) <- c("Price","Design","Color")
colnames(mydata) <- c("Male","Female")

chisq.test(mydata)