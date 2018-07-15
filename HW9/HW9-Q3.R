#HW9
#Question 3
file_namse <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/') %>% str_replace('.csv', '')
file_paths <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/', full.names = TRUE)

#----------------------------------
#Concatanting all the data:
datalist_volume = list()
for ( i in 1:length(file_namse))
{
  datalist_volume[[i]] <- read_csv(file_paths[i]) %>% select(Date,Volume,High,Low)
}

#----------------------------------
#Finding the highest Volume(high-low) in all the days. the highest is the desired one.

big_data_volume = do.call(rbind,datalist_volume)
big_data_volume %>% 
  group_by(Date) %>% 
  summarise(total_Volume = sum(Volume*(High-Low))) %>% 
  arrange(desc(total_Volume))-> big_data_volume_final
#----------------------------------
#Highest rate:
big_data_volume_final[1,1] #2008-10-10"
sprintf("The highest rate was 2008-10-10")

