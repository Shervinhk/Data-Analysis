#HW5-q6:

mydata = matrix(
    c(151,802,753,252,603,55,603,405,408),
    nrow = 3,
    ncol = 3
)

rownames(mydata) <- c("Small","Medium","Large")
colnames(mydata) <- c("Always","Sometime","Never")

chisq.test(mydata)