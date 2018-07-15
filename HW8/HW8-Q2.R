#HW8-Q2:

#---------------------------
#Sorting:
dickens_sorted %>% 
  select(word = word, freq = total_count) -> dickens_sorted_plot
#---------------------------
#wordcloud2 library:
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
#---------------------------
#Plotting All of them:
wordcloud2(dickens_sorted_plot,size = 0.2,
           figPath = "/Users/shervin/Desktop/untitled folder 3/Unknown.png")

#---------------------------
# The first 200:
wordcloud2(dickens_sorted_plot[1:200,],size = 0.2,
            figPath = "/Users/shervin/Desktop/untitled folder 3/Unknown.png")

