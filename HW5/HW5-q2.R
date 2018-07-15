#HW5-q2:


mydata = data.frame(score = c(50, 50, 60, 70, 75, 80, 90, 85,55, 75, 80, 90, 105, 65),
                    type = factor(c(rep("A",8), rep("B",6))))

t.test(score~type, data=mydata, var.equal=TRUE)

coin::oneway_test(score~type, data=mydata, distribution="exact")


