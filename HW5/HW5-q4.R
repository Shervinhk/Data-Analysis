#HW5-q4:

mydata = data.frame(score = c(510,720,930,754,105,925,735,753,685,730,745,875,610),
                    type = factor(c(rep("A",5), rep("B",4),rep("C",4))))

kruskal.test(score ~ type, data = mydata)

