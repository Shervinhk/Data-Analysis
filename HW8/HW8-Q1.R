#HW8-Q1
#---------------------------
#Required Packages:
library(stringr)
library(wordcloud)
library(tm)
library(gutenbergr)
library(tidytext)
library(gsubfn)
library(wordcloud2)
library(ngram)
library(dplyr)
library(highcharter)
library("SnowballC")
library("RColorBrewer")
#---------------------------
#Finding all the books of Charles Dickens:
gutenberg_works(author == "Dickens, Charles")

#---------------------------
# Downloading the Novels:(14 books)
books <- gutenberg_download(c(580,
                              730,
                              967,
                              700,
                              917,
                              968,
                              821,
                              766,
                              1023,
                              786,
                              963,
                              98,
                              1400,
                              883,
                              564), meta_fields = "title")

#---------------------------
# Books and Novels of Charles Dickens:
ThePickwickPapers = gutenberg_download(580)
OliverTwist = gutenberg_download(730)
NicholasNickleby = gutenberg_download(967)
TheOldCuriosityShop = gutenberg_download(700)
BarnabyRudge = gutenberg_download(917)
MartinChuzzlewit = gutenberg_download(968)
DombeyandSon = gutenberg_download(821)
DavidCopperfield =gutenberg_download(766)
BleakHouse =gutenberg_download(1023)
HardTimes =gutenberg_download(786)
LittleDorrit =gutenberg_download(963)
ATaleofTwoCities = gutenberg_download(7869)
GreatExpectations = gutenberg_download(1400)
OurMutualFriend = gutenberg_download(883)
TheMysteryofEdwinDrood =gutenberg_download(564)

#---------------------------
#List of books and their names:
books_list =data.frame(ID= c(580,
               730,
               967,
               700,
               917,
               968,
               821,
               766,
               1023,
               786,
               963,
               98,
               1400,
               883,
               564)  ,Name=
c( "ThePickwickPapers","OliverTwist",
"NicholasNickleby", "TheOldCuriosityShop","BarnabyRudge","MartinChuzzlewit","DombeyandSon",
"DavidCopperfield",
"BleakHouse",
"HardTimes",
"LittleDorrit",
"ATaleofTwoCities",
"GreatExpectations",
"OurMutualFriend",
"TheMysteryofEdwinDrood"))

#---------------------------
# Using the function that we used in class:
words <- function(i,k)
{
  x = gutenberg_download(k)
  x = x %>% 
    str_replace_all("\"","") %>% 
    str_replace_all("[[:punct:]]","") %>% 
    str_split(pattern = "\\s") %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F)
  colnames(x) = c("word","count")
  x = x %>%
    filter(!str_to_lower(word) %in% stop_words$word) %>% 
    filter(str_length(word)>1) %>% 
    filter(!str_detect(word,"\\d")) %>%
    mutate(Book = books_list[i,2]) %>% 
    arrange(desc(count)) %>% 
    mutate(proper = !word %in% str_to_lower(word)) 
   dickens_list = x
  return(dickens_list)
}

#---------------------------
#Binding the books:
n = length(books_list$ID)
out = list()

for( i in 1:n)
{
out[[i]] = words(i,books_list[i,1])
}
dickens = bind_rows(out)
dickens$word <- tolower(dickens$word)

#---------------------------
#Sorting: 
dickens %>% 
  group_by(word) %>% 
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))-> dickens_sorted
dickens_sorted = dickens_sorted[-c(4),]

#---------------------------
#Plotting the most used words:
dickens_sorted[1:10,] %>% 
  hchart("column",hcaes(x = word, y = total_count)) %>% 
  hc_add_theme(hc_theme_ffx()) %>% 
  hc_yAxis(title =list(text = "Total Number of Repitions"))

#---------------------------


