#HW 07:
#Question 4:
library(boot)
q =glm.diag.plots(new_myglm_second, 
                  glmdiag = glm.diag(new_myglm_second),
                  labels=NULL)
q