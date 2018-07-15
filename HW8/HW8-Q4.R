#HW8-Q4:

for (i in 1:15)
{
  
  book = out[[1]]
  book$word <- tolower(book$word)  
  book %>% group_by(word) %>% 
    summarise(count = sum(count)) %>% 
    arrange(desc(count)) -> book_count
  
  book_count %>% 
    mutate( is.sentiment = (word %in% sentiments$word) ) -> book_sentiment
  
  book_sentiment %>% 
    filter(is.sentiment == TRUE) -> book_sentiment
  row_sentiment = length(book_sentiment$word)
  
  for( i in 1:row_sentiment)
  {
    book_sentiment$type[i] = sentiments$sentiment[which(
                             sentiments$word==book_sentiment$word[i])]
    
  }
  book_sentiment = na.omit(book_sentiment)
  book_sentiment %>% 
    select(word,count,type) %>% 
    filter(type == "positive") -> book_sentiment_p
  
  book_sentiment %>% 
    select(word,count,type) %>% 
    filter(type == "negative") -> book_sentiment_n
  
  book_sentiment %>% 
    select(word,count,type) %>% 
    filter( !(type %in% c("negative","positive"))) -> book_sentiment_feel
  
  binding = rbind(book_sentiment_p[1:20,],book_sentiment_n[1:20,])

  
layout(matrix(c(1, 2), nrow=2), heights=c(2, 3))
par(mar=rep(0, 4))  
plot.new()
text(x=0.5, y=0.01, sprintf("Negative vs Positive of the book %s",books_list[i,2]))
wordcloud(binding$word,binding$count,
          random.order=FALSE, rot.per=0.1, 
          ordered.colors=TRUE,
          colors=brewer.pal(3, "Dark2")[factor(binding$type)],
          main="Title",
          scale=c(8,.3))
name = factor(factor(binding$type),levels(factor(binding$type))[c(2,1)])
legend("topright",
       legend = levels(factor(binding$type)),
       text.col=brewer.pal(8, "Dark2")[unique(name)])

hc_sentiment <- highchart() %>% 
  hc_add_series(name = "frequency of two words",data = book_sentiment_feel[1:8,],
                type = "bar",
                mapping = hcaes(x = word, y = count),
                colorByPoint = TRUE) %>% 
  hc_xAxis(title = list(text = "Name of the  Words"),
           categories = book_sentiment_feel$word) %>% 
  hc_yAxis(title = list(text = "Frequency")) %>% 
  hc_title(text =sprintf("Sentiment in %s",books_list[i,2]))
hc_sentiment

}