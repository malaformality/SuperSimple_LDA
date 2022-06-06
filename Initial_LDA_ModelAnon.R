# Latent Dirichlet Allocation (LDA) Topic Model
# Charlize Theron
# 5/12/2020

library(tidyverse)
library(tidytext)
# library(textmineR)


data <- readxl::read_xlsx("CompensaationSurvey.xlsx")

data <- data %>%
  select(text = `what can we do to show appreciation, other than cash??`) %>%
  filter(is.na(text) == FALSE) %>%
  slice(2:n()) %>%
  mutate(id = row_number())

## Normal old tokenization and cleaning (stop words, punctuation, etc)
data_tokens <- data %>%
  unnest_tokens(word, text) 

data_tokens$word <- gsub('[[:punct:]]+', '', data_tokens$word)

# data_tokens <- 
token_counts <- data_tokens %>% 
  filter(nchar(word) != 1,
         word != "") %>% 
  anti_join(stop_words) %>%
  count(id, word) 



## Model Building! -- creating the Document Term Matrix (DTM)

tokens_dtm <- token_counts %>%
  cast_dtm(id, word, n)

library(topicmodels)

comments_lda <- LDA(tokens_dtm, k = 8, control = list(seed = 1234))

comments_lda_td <- tidy(comments_lda)

#top terms per topic
top_terms <- comments_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_col() +
  scale_x_reordered() +
  facet_wrap( ~ topic, scales = "free_x")

comments_lda_gamma <- tidy(comments_lda, matrix = "gamma")

comments_lda_gamma %>%
  separate(document, c("title","chapter"), sep = "_", convert = TRUE)

comments_lda_gamma
