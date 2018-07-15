#HW9:
#We will check whether this assumptions is good or not:
#--------------------------------------
file_namse <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/') %>% str_replace('.csv', '')
file_paths <- list.files('/Users/shervin/Downloads/class_data 2/stock_dfs/', full.names = TRUE)

data <- read_csv(file_paths[1]) %>% select(Date, Close,Open)
data$Date = as.Date(data$Date) #turning into date
#--------------------------------------
#Defning the function that finds the values for the 13th day:
check_thirteen <- function(data)
{
  data %>% mutate(days = as.integer(format(Date,"%d"))) %>% filter(days == 13) %>% 
    mutate(value = Close - Open) -> final
  return(final$value)
}
#--------------------------------------
hist = list()
for (i in 1:length(file_namse))
{
  data_out <- read_csv(file_paths[i]) %>% select(Date, Close,Open)
  hist[[i]] = check_thirteen(data_out)
}

hist_ogram = unlist(hist)
#--------------------------------------
#Histogram:
h <- hist(hist_ogram, plot = FALSE,breaks = 10)
hchart(h) %>% 
  hc_title(text = "Histogram of Value in every 13th day of the Month") %>% 
  hc_xAxis(title = list(text = "value")) %>% 
  hc_yAxis(title = list(text = "Number of times"))

#--------------------------------------
#If the values of positive are different than negatives
t.test(hist_ogram,mu=0) #t.test rejects the hypothesis that 13th is "unlucky" 

