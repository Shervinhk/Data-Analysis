#-----------------------------------------------
pic = flip(readImage("/Users/shervin/Desktop/Data Analysis/HW9'/Unknown.jpg"))

red.weigth   = .2989; green.weigth = .587; blue.weigth  = 0.114

img = red.weigth * imageData(pic)[,,1] +
  green.weigth * imageData(pic)[,,2] + blue.weigth  * imageData(pic)[,,3]
image(img, col = grey(seq(0, 1, length = 256)))
#-----------------------------------------------
#PCA
pca.img = prcomp(img, scale=TRUE)

plot(summary(pca.img)$importance[3,], type="l",
     ylab="%variance explained", xlab="nth component (decreasing order)")

#-----------------------------------------------
#With trial and error 115 is the best number:
abline(h=0.99,col="red");abline(v = 115,col="red",lty=3)
size = c()
n = 200
for (i in 1:n){
  chosen.components = 1:i
  feature.vector = pca.img$rotation[,chosen.components]
  compact.data = t(feature.vector) %*% t(img)
  
  temp = (object.size(compact.data) + object.size(feature.vector))/1000000
  size = c(size,temp)
  approx.img = t(feature.vector %*% compact.data) 
}
temp1 = data.frame(PC = 1:n,size = size)

check = data.frame(P = 1:n,value = size)

check %>% hchart(hcaes(x = P , y = value), type = "line") %>% 
  hc_title(text = "Size - number of PCs") %>% 
  hc_yAxis(title = list(text = "Mean Value")) %>% 
  hc_xAxis(text = "PCs")
#-----------------------------------------------
#PCA Picture:
chosen.components = 1:115
feature.vector = pca.img$rotation[,chosen.components]
compact.data = t(feature.vector) %*% t(img)
temp = (object.size(compact.data) + object.size(feature.vector))/1000000
approx.img = t(feature.vector %*% compact.data) 
image(approx.img, col = grey(seq(0, 1, length = 256)))
#-----------------------------------------------
