file_namse <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/') %>% str_replace('.csv', '')
file_paths <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/', full.names = TRUE)
constitutes = read.csv("/Users/shervin/Downloads/class_data 2/constituents.csv",stringsAsFactors = FALSE)
detach(package:plyr)
library(quantmod)
#---------------------------------------
Sector = data.frame(Sector=c(0))
for (i in 1: length(file_namse))
{
  if( sum(constitutes$Symbol == file_namse[i]) !=0)
  {
    Sector[i,1] = constitutes$Sector[which(constitutes$Symbol == file_namse[i] )]
  }
  else
  {
    Sector[i,1] = -1
  }
  
}
#---------------------------------------
data <- read.csv(file_paths[1],stringsAsFactors=FALSE) %>% select(Date, Open)

stock =data.frame(Date =data$Date, Open=data$Open,Company =file_namse[1],
                  Sector = Sector[1,1])
#---------------------------------------
for( i in 2:length(file_namse))
{
data <- read.csv(file_paths[i],stringsAsFactors=FALSE) %>% select(Date, Open)

new_stock = data.frame(Date =data$Date, Open=data$Open,Company =file_namse[i],
                   Sector = Sector[i,1])
stock = rbind(stock,new_stock)
}
#---------------------------------------

stock_check = as.data.frame(stock)
names = c("Health Care","Industrials","Consumer Discretionary",
          "Information Technology","Consumer Staples","Utilities",
          "Financials","Real Estate","Materials",
          "Energy","-1")
#---------------------------------------
#Finding all the averages for all the sectors:
stock %>% filter(Sector == names[1]) %>% 
  group_by(Date) %>% 
  summarise(Average_Open = mean(Open)) -> temp

for (i in 2:length(names))
{
  stock %>% filter(Sector == names[i]) %>% 
    group_by(Date) %>% 
    summarise(Average_Open = mean(Open)) -> temp_new
  temp = cbind(temp,temp_new$Average_Open)
}
#---------------------------------------
indexes = read.csv("/Users/shervin/Downloads/class_data 2/indexes.csv" )
indexes = na.omit(indexes)
#---------------------------------------
#Joining indexes and our data:
merged = merge(indexes, temp, by = "Date")
merged$Date <- NULL

#---------------------------------------
#PCA:
merged_pca = prcomp((merged))
summary(merged_pca)
#---------------------------------------
#biplot:
biplot(merged_pca,cex = 0.8)
#---------------------------------------

