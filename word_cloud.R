library(tm)
library(wordcloud2)

library(tidyverse); library(stopwords); library(wordcloud); 
library(reshape);library(scales); library(quanteda); 
library(quanteda.textplots); library(quanteda.textstats); 
library(wesanderson); library(highcharter); library(gghighlight)
library(htmlwidgets); library(networkD3); library(tidytext); library(reshape2)

df <- read.csv('data/grad_school_advice.csv', header = T)

df <- df |> select(text)

df_tokenized <- unnest_tokens(tbl = df, input = text, output = word)

stop_words <- get_stopwords(source = "smart")
stop_words

df_no_stop_words <- anti_join(df_tokenized, stop_words)
df_no_stop_words

words <- df_no_stop_words %>% count(word, sort = TRUE)

words <- words |> dplyr::rename(freq = n)

letterCloud(words, word = "Justin")

cl <- colorRampPalette(c('purple', 'cyan', 
                         'red', 'yellow',
                         'magenta'))

wordcloud2(words, size = 0.5, 
           shape = 'triangle', 
           minSize = 1, 
           color = cl(100))
