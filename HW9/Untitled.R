# HW09:
#Question 1
library(readr)
library(dplyr)
library(stringr)
library(highcharter)
library(EBImage)
library(quantmod)
# OHLC time series, Open High Low Closed prices
# analysing the stocks
# we want to unify all files and data.


file_namse <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/') %>% str_replace('.csv', '')
file_paths <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/', full.names = TRUE)

data <- read_csv(file_paths[1]) %>% select(Date, Close)


data$Date <- as.numeric(data$Date)
max = max(data$Date)
min = min(data$Date)

dif = max-min
one_year_count = floor(dif/(365)-1)


find_close <- function(data,n)
{
  #----------------------------
  #n_year:
  data$Date <- as.numeric(data$Date)
  min = min(data$Date)
  max = max(data$Date)
  dif = max-min
  
  one_year_count = floor(dif/(n*365))
  if(one_year_count<1){return(0)}
  
  year = which(data$Date == min ) 
  average = data.frame(Average = c(0))
  average[1,1] = mean(data$Close[year])
  
  benefit = data.frame(one = c(0))

  for (i in 2:one_year_count)
  {
  year = which(data$Date %in% c((min+(i-1)*n*365-10):(min+(i-1)*n*365+10)) )  
  average[i,1] = mean(data$Close[year])
  
  benefit[i-1,1] = (average[i,1] - average[i-1,1])/(average[i,1])
  }
  benefit = na.omit(benefit)
  max_benefit = max(benefit)
  return(max_benefit)
  #----------------------------
}

constitutes = read.csv("/Users/shervin/Downloads/class_data 2/constituents.csv",stringsAsFactors = FALSE)

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


datalist = list()
 
one_year = find_close(data,1)
two_year = find_close(data,2)
five_year = find_close(data,5)
Values = data.frame( OneYear = one_year,
                     TwoYears = two_year,
                     FiveYears = five_year, Company = file_namse[1])
                       

datalist[[1]] = Values
for ( i in 2:length(file_namse))
{
data_ext <- read_csv(file_paths[i]) %>% select(Date, Close)
one_year = find_close(data_ext,1)
two_year = find_close(data_ext,2)
five_year = find_close(data_ext,5)
Values_new = data.frame( OneYear = one_year,
                     TwoYears = two_year,
                     FiveYears = five_year, Company = file_namse[i] )

datalist[[i]] <- Values_new
}

big_data = do.call(rbind, datalist)
big_data_final = cbind(big_data,Sector)

big_data %>% 
        group_by(OneYear) %>% 
        select(Company) %>% 
        arrange(desc(OneYear))-> one_year_top
one_year_top[1:10,2]

big_data %>% 
  group_by(TwoYears) %>% 
  select(Company) %>% 
  arrange(desc(TwoYears))-> two_year_top
two_year_top[1:10,2]

big_data %>% 
  group_by(FiveYears) %>% 
  select(Company) %>% 
  arrange(desc(FiveYears))-> five_year_top
five_year_top[1:10,2]

names = c("Health Care","Industrials","Consumer Discretionary",
          "Information Technology","Consumer Staples","Utilities",
          "Financials","Real Estate","Materials",
          "Energy","-1")

for ( i in 1:length(names))
{
big_data_final %>% 
  filter(Sector== names[i]) %>% 
  group_by(OneYear) %>% 
  select(Company) %>% 
  arrange(desc(OneYear)) -> Sector_one_year
  Sector_one_year[1:10,2]
  
  big_data_final %>% 
    filter(Sector== names[i]) %>% 
    group_by(TwoYears) %>% 
    select(Company) %>% 
    arrange(desc(TwoYears)) -> Sector_two_year
  Sector_two_year[1:10,2]
  
  big_data_final %>% 
    filter(Sector== names[i]) %>% 
    group_by(FiveYears) %>% 
    select(Company) %>% 
    arrange(desc(FiveYears)) -> Sector_five_year
  Sector_five_year[1:10,2]
}
  


