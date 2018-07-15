#HW5-q3:
#using willcoxon test:

mydata = data.frame(score = c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774,
                              718, 904,517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709,
                              688, 787, 780, 901),
                    type = factor(c(rep("A",15), rep("B",15))))

wilcox.test(score~type, data=mydata)



