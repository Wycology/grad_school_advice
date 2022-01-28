library(tidyverse); library(stopwords); library(wordcloud); 
library(reshape);library(scales); library(quanteda); 
library(quanteda.textplots); library(quanteda.textstats); 
library(wesanderson); library(highcharter); library(gghighlight)
library(htmlwidgets); library(networkD3); library(tidytext); library(reshape2)

df <- read.csv('data/grad_school_advice.csv', header = T)

head(df)

nyako <- df |> select(text)

nyako_tokenized <- unnest_tokens(tbl = nyako, input = text, output = word)

stp_wrds <- get_stopwords(source = "smart")
stp_wrds

nyako_no_stp_wrds <- anti_join(nyako_tokenized, stp_wrds)
nyako_no_stp_wrds

nyako_no_stp_wrds %>% count(word, sort = TRUE)

nyako_no_stp_wrds %>% count(word, sort = TRUE) %>%
  filter(n > 250) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(fill = 'purple')  +
  labs(y = "Words Used", x = "Frequency of use") +
  gghighlight(word %in% c('guys', 'yeah', 'video')) +
  theme(text = element_text(size = 24))

anti_join(nyako_no_stp_wrds, stp_wrds, by = 'word') %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 500))

nyako_no_stp_wrds %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = 'n', fill = 0) %>% 
  comparison.cloud(colors = c("red", "blue"), max.words = 1000)

pattern <- read.csv('pattern.csv')
pattern %>% arrange(-Views) %>% 
  #filter(Views <= 150000) %>% 
  ggplot() + 
  geom_col(aes(reorder(Date, Views), Views), fill = 'magenta') + 
  gghighlight(Date == '8/5/2021') +
  coord_flip()