list.of.packages <- c("tm", "topicmodels", "tidytext", "dplyr", "ggplot2", "wordcloud", "RColorBrewer", "tidyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)


library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tidyr)

df <- read.csv("C:/Users/Dell/Downloads/data science/ids_final_project_group_08_news_clean.csv", stringsAsFactors = FALSE)
head(df)

table(df$Category)
names(df)

corpus <- Corpus(VectorSource(df$Description_FinalTokens))



corpus <- tm_map(corpus, content_transformer(tolower))      
corpus <- tm_map(corpus, removePunctuation)                   
corpus <- tm_map(corpus, removeNumbers)                       
corpus <- tm_map(corpus, removeWords, stopwords("english"))   
corpus <- tm_map(corpus, stripWhitespace)                     



dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.95)  

print(dtm)



k <- 5  
set.seed(1234)  
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))




topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

print(top_terms)



doc_topics <- tidy(lda_model, matrix = "gamma")
head(doc_topics)


topic_labels_df <- top_terms %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 3) %>%
  summarise(label = paste(term, collapse = ", ")) %>%
  ungroup()

print(topic_labels_df) 


top_terms_labeled <- top_terms %>%
  left_join(topic_labels_df, by = "topic")

print(top_terms_labeled)  


topic_beta_summary <- top_terms %>%
  group_by(topic) %>%
  summarise(
    mean_beta = mean(beta),
    median_beta = median(beta),
    sd_beta = sd(beta),
    max_beta = max(beta),
    min_beta = min(beta),
    n_terms = n()
  )

print(topic_beta_summary)


tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)


word_freq_stats <- summary(word_freqs_df$freq)
print(word_freq_stats)
top_words_freq <- head(word_freqs_df, 20)
print(top_words_freq)



install.packages("entropy")
library(entropy)

library(entropy)

doc_topic_summary <- doc_topics %>%
  group_by(document) %>%
  summarise(entropy = entropy::entropy(gamma))

print(head(doc_topic_summary))


