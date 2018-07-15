#HW8-Q7:
#---------------------------
#We will first extract the books:
dickens_charles = gutenberg_download(books_list[,1])

#---------------------------
#Now we will remove the uncessary punctuations and give a sign for the places
#that have ", . ! @" this sign is "!!!!!" we will need this sign:

dickens_charles_str = dickens_charles %>%  
  str_replace_all("\"","") %>% 
  str_replace_all("[[:punct:]]"," \\!!!!!")
dickens_charles_str <- sapply(dickens_charles_str,tolower)
#--------------------------- 
#Using the N-grqm package we will find the 2 word:
ng <- ngram(dickens_charles_str,n=2) #ng object
ngram = get.phrasetable(ng)
ngrams = as.data.frame(ngram) #making it as a data frame

#--------------------------- 
# length or ngram:
J = length(ngrams$ngrams)

#--------------------------- 
# we will find those that have punctuations using the statment 
# we said in the upper lines:
check_ngram = str_extract(ngrams$ngrams,"\\!") # removing punctuations
indexes = which(check_ngram==c("!")) # indexes of those we dont want

#--------------------------- 
# Removed indesxes:
ngrams_dickens = ngram[-indexes,]
J2 = length(ngrams_dickens$ngrams)
ngrams_dickens$ngrams = sapply(ngrams_dickens$ngrams,tolower)
#--------------------------- 
# Now we will find the words that start with he/she:
rows_dickens = nrow(ngrams_dickens)
index_dickens = list()
b = 1
for(i in 1:rows_dickens)
{
  tempe =ngrams_dickens$ngrams[i]
  tempe_split = str_split(tempe," ")
  tempe_split = unlist(tempe_split)
  if(sum(tempe_split[1] %in% c("he","she")) )
  {
    index_dickens[b] = i
    b = b+1
  }
}

index_dickens = unlist(index_dickens)
ngrams_dickens_he_she = ngrams_dickens[index_dickens,]
ngrams_dickens_he_she

rows_he_she = nrow(ngrams_dickens_he_she)
for( k in 1:100)
{
for ( i in 1:500)
{
  tempe =ngrams_dickens_he_she$ngrams[i]
  tempe_split = str_split(tempe," ")
  tempe_split = unlist(tempe_split)
  if(sum(tempe_split[2] %in% stop_words$word))
    ngrams_dickens_he_she = ngrams_dickens_he_she[-i,]
}
}
ngrams_dickens_he_she = ngrams_dickens_he_she[-11,]

hc_he_she <- highchart() %>% 
  hc_add_series(name = "frequency of verbs",data = ngrams_dickens_he_she[1:30,],
                type = "column",
                mapping = hcaes(x = ngrams, y = freq),
                color = "navy blue") %>% 
  hc_xAxis(title = list(text = "Name of the Two Words"),
           categories = ngrams_dickens_he_she$ngrams) %>% 
  hc_yAxis(title = list(text = "Frequency")) %>% 
  hc_title(text ="Frequency of verbs in Les Miserables")

hc_he_she 

ngrams_dickens_he_she_new = ngrams_dickens_he_she
verbs_rows = nrow(ngrams_dickens_he_she)
for( i in 1:verbs_rows)
{
  tempe =ngrams_dickens_he_she$ngrams[i]
  tempe_split = str_split(tempe," ")
  tempe_split = unlist(tempe_split)
  ngrams_dickens_he_she_new$ngrams[i] = tempe_split[2]
  
}
ngrams_dickens_he_she_new = as.data.frame(ngrams_dickens_he_she_new)
ngrams_dickens_he_she_new %>% 
  group_by(ngrams) %>% 
  summarise(freq = sum(freq)) %>% 
  arrange(desc(freq))-> ngrams_dickens_he_she_new_count

hc_he_she_new <- highchart() %>% 
  hc_add_series(name = "frequency of verbs",data = ngrams_dickens_he_she_new_count[1:30,],
                type = "column",
                mapping = hcaes(x = ngrams, y = freq),
                color = "blue") %>% 
  hc_xAxis(title = list(text = "Name of the Verbs"),
           categories = ngrams_dickens_he_she_new_count$ngrams) %>% 
  hc_yAxis(title = list(text = "Frequency")) %>% 
  hc_title(text ="Frequency of verbs in Charles Dickens workds")

hc_he_she_new




index = list()
b = 1
rows = nrow(ngrams2)
for ( i in 1:rows)
{
  tempe =ngrams2$ngrams[i]
  tempe_split = str_split(tempe," ")
  tempe_split = unlist(tempe_split)
  if(sum(tempe_split %in% stop_words$word))
    ngrams2 = ngrams2[-i,]
}

rows_ngram = nrow(ngrams2)
for(i in 1:100)
{
  tempe =ngrams2$ngrams[i]
  tempe_split = str_split(tempe," ")
  tempe_split = unlist(tempe_split)
  if(sum(tempe_split[1] %in% c("he","she")) )
  {
      index[b] = i
        b = b+1
  }
}

index = unlist(index)
ngrams_he = ngrams2[index,]

rows_he = nrow(ngrams_he)

index_he = list()
b = 1
for (i in 1:rows_he)
{
  tempe =ngrams_he$ngrams[i]
  tempe_split = str_split(tempe," ")
  tempe_split = unlist(tempe_split)
  if(sum(tempe_split[1] %in% c("he","she")) )
  {
    index_he[b] = i
    b = b+1
  }
}
index_he = unlist(index_he)
ngrams_he_she = ngrams_he[index_he,]

hc_he_she <- highchart() %>% 
  hc_add_series(name = "frequency of verbs",data = ngrams_he[1:30,],
                type = "column",
                mapping = hcaes(x = ngrams, y = freq),
                color = "navy blue") %>% 
  hc_xAxis(title = list(text = "Name of the Two Words"),
           categories = ngrams2$ngrams) %>% 
  hc_yAxis(title = list(text = "Frequency")) %>% 
  hc_title(text ="Frequency of verbs in Les Miserables")

hc_he_she 
