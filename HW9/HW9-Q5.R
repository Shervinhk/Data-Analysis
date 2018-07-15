#HW9
# Question 5:
library(plyr)
file_namse <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/') %>% str_replace('.csv', '')
file_paths <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/', full.names = TRUE)

data_pca = list()
length = data.frame(length =0)
for( i in 1:length(file_namse))
{
data_pca[[i]] <- read_csv(file_paths[i]) %>% select(Open)
length[i,1] = length(data_pca[[i]]$Open)
}
max_length = max(length)

m = which(length == max_length)

data_pca_new = list()
for (i in m)
{
  data_pca_new[[i]] = data_pca[[i]]
}
data_pca_new_check =compact(data_pca_new)

big_data_pca = do.call(cbind,data_pca_new_check)

K = prcomp(big_data_pca)

h_c = data.frame(check = cumsum(K$sdev)/sum(K$sdev) ,
                 P= 1:length(K$sdev) )
h_c %>% hchart( hcaes(x = P , y = check),type = "scatter" ) %>% 
  hc_title(text = "Variance of PCA") %>% 
  hc_xAxis(title = list(text ="PC")) %>% 
  hc_yAxis(title = list(text="Variance"))

#How much the first 3 PC s keep is :
h_c$check[3]













