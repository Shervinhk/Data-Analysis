#HW8-Q8:
#---------------------------
#Collectiing the book and cleaning it:
Oliver_Twist = gutenberg_download(730)
Oliver_Twist[[2]][1:117] <- " " #removing contents page

Oliver_Twist_new = Oliver_Twist %>%  
  str_replace_all("\"","") %>% 
  str_replace_all("[[:punct:]]","") %>% 
  str_to_lower()
chapters =str_split(Oliver_Twist_new, "CHAPTER")
#---------------------------
# Extracting the 1-grams for every chapter:
r_2 =length(chapters[[2]])
ngram_dickens = list()
index = list()
for (i in 2:r_2)
{
   k = chapters[[2]][i] %>%
     str_to_lower() %>% 
     str_split(pattern = "\\s") %>% 
     unlist() %>% 
     table() %>% 
     as.data.frame(stringsAsFactors = F)
   colnames(k) = c("word","count")
   
   k = k %>%
     filter(!str_to_lower(word) %in% stop_words$word) %>% 
     filter(str_length(word)>1) %>% 
     filter(!str_detect(word,"\\d")) %>%
     arrange(desc(count)) %>% 
     mutate(proper = !word %in% str_to_lower(word))
   
   hc_chapter <- highchart() %>% 
     hc_add_series(name = "frequency of single words",data = k[1:50,],
                   type = "column",
                   mapping = hcaes(x = word, y = count),
                   color = "blue") %>% 
     hc_xAxis(title = list(text = "Name of the Words"),
              categories = k$word) %>% 
     hc_yAxis(title = list(text = "Frequency")) %>% 
     hc_title(text =sprintf("Frequency of words in Oliver Twist, Chapter %s"
                            ,i))
   hc_chapter
}

#---------------------------
#Extracting 2-grams for every chapter:
ngram_dickens = list()
r_2 =length(chapters[[2]])
index = list()
for (i in 1:r_2)
{
  b = 0 
  ngram_dickens = chapters[[2]][i] %>%
    str_to_lower()
  ngram_dickens= get.phrasetable(ngram(ngram_dickens,n=2))
  ngram_dickens_dataframe= as.data.frame(ngram_dickens)
  
  row_chapter_n2 = nrow(ngram_dickens_dataframe)

  for( p in 1:3)
  {
  for ( j in 1:row_chapter_n2)
  {
    tempe =ngram_dickens_dataframe$ngrams[j]
    tempe_split = str_split(tempe," ")
    tempe_split = unlist(tempe_split)
    if(sum( tempe_split %in% stop_words$word))
    {
      index[b] = j
      b = b+1
    }
    ngram_dickens_dataframe_2 = ngram_dickens_dataframe[-index,]
  }
  }
  
  hc_chapter <- highchart() %>% 
    hc_add_series(name = "frequency of bigrams",data = ngram_dickens_dataframe_2[1:50,],
                  type = "column",
                  mapping = hcaes(x = ngrams, y = freq),
                  color = "blue") %>% 
    hc_xAxis(title = list(text = "Name of the Words"),
             categories = ngram_dickens_dataframe_2$ngrams) %>% 
    hc_yAxis(title = list(text = "Frequency")) %>% 
    hc_title(text =sprintf("Frequency of bigram in Oliver Twist, Chapter %s"
                           ,i))
  hc_chapter
  
}
#---------------------------


