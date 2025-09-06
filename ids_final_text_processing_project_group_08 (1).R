library(rvest)
library(httr)
library(dplyr)

base_url <- "https://www.foxnews.com"
category_paths <- c("us", "politics", "world", "entertainment", "sports")

all_articles <- list()

extract_articles <- function(category) {
  cat("\nProcessing category:", category, "\n")
  articles <- data.frame()
  
  for (page in 1:5) {
    url <- paste0(base_url, "/", category, "?page=", page)
    
    res <- try(GET(url, user_agent("Mozilla/5.0")), silent = TRUE)
    if (inherits(res, "try-error") || status_code(res) != 200) {
      cat(" Failed to load:", url, "\n")
      next
    }
    
    html <- read_html(content(res, as = "text", encoding = "UTF-8"))
    cards <- html_nodes(html, "main article a")
    links <- unique(html_attr(cards, "href"))
    links <- links[grepl("^/[^/]+/[^/]+", links)]
    links <- paste0(base_url, links)
    
    for (link in head(links, 20)) {
      article_page <- tryCatch(read_html(link), error = function(e) NULL)
      if (is.null(article_page)) next
      
      title <- article_page %>%
        html_node("h1") %>%
        html_text(trim = TRUE)
      
      if (is.na(title) || title == "") next
      
      raw_paragraphs <- article_page %>%
        html_nodes("div.article-body p") %>%
        html_text(trim = TRUE)
      
      clean_paragraphs <- raw_paragraphs[nchar(raw_paragraphs) > 30]
      description <- paste(clean_paragraphs, collapse = " ")
      description <- substr(description, 1, 500)
      
      time_node <- article_page %>% html_node("time")
      if (!is.na(time_node)) {
        date_attr <- html_attr(time_node, "datetime")
        date_text <- html_text(time_node, trim = TRUE)
        
        if (!is.na(date_attr) && nchar(date_attr) > 0) {
          date <- substr(date_attr, 1, 10)
        } else if (!is.na(date_text) && grepl("[A-Za-z]+ \\d{1,2}, \\d{4}", date_text)) {
          matched <- regmatches(date_text, regexpr("[A-Za-z]+ \\d{1,2}, \\d{4}", date_text))
          date <- format(as.Date(matched, "%B %d, %Y"), "%Y-%m-%d")
        } else {
          date <- NA
        }
      } else {
        date <- NA
      }
      
      if (!is.na(description) && nchar(description) > 30) {
        articles <- rbind(articles, data.frame(
          Title = title,
          Description = description,
          Date = date,
          Category = category,
          URL = link,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  return(articles)
}

for (cat in category_paths) {
  all_articles[[cat]] <- extract_articles(cat)
}

final_df <- bind_rows(all_articles)
write.csv(final_df, "foxnews_articles.csv", row.names = FALSE)
cat("\nSaved to 'foxnews_articles.csv' with titles, descriptions, dates, and categories.\n")

install.packages(c("dplyr", "stringr", "textclean", "tm", "textstem", "hunspell", "textdata", "emoji"))

library(dplyr)
library(stringr)
library(textclean)
library(tm)
library(textstem)
library(hunspell)
library(emoji)

df <- read.csv("foxnews_articles.csv")

head(df)

has_emoji <- function(text) {
  grepl("[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F1E0-\U0001F1FF]", text, perl = TRUE)
}

has_emoticon <- function(text) {
  grepl("[:;=8xX][-~oO*']?[)(DdpPoO/|\\\\]", text)
}

emoji_in_title <- any(sapply(df$Title, has_emoji))
emoticon_in_title <- any(sapply(df$Title, has_emoticon))

emoji_in_desc <- any(sapply(df$Description, has_emoji))
emoticon_in_desc <- any(sapply(df$Description, has_emoticon))

cat("Emoji in Title:", emoji_in_title, "\n")
cat("Emoticon in Title:", emoticon_in_title, "\n")
cat("Emoji in Description:", emoji_in_desc, "\n")
cat("Emoticon in Description:", emoticon_in_desc, "\n")

has_contraction <- function(text) {
  grepl("\\b\\w+['’`]\\w+\\b", text)
}

contraction_in_title <- any(sapply(df$Title, has_contraction))
contraction_in_desc <- any(sapply(df$Description, has_contraction))

cat("Contraction in Title:", contraction_in_title, "\n")
cat("Contraction in Description:", contraction_in_desc, "\n")

df <- df %>%
  mutate(
    Title_Expanded = replace_contraction(Title),
    Description_Expanded = replace_contraction(Description)
  )

cat("\n After Contraction Expansion:\n")
head(df[1, c("Title","Description","Title_Expanded", "Description_Expanded")])

remove_emoticons <- function(text) {
  gsub("[:;=8xX][-~oO*']?[)(DdpPoO/|\\\\]", "", text)
}

df <- df %>%
  mutate(
    Cleaned_Title = Title_Expanded %>%
      remove_emoticons() %>%
      tolower() %>%
      str_replace_all("<.*?>", "") %>%
      str_replace_all("http\\S+|www\\S+", "") %>%
      str_replace_all("[^[:alnum:]\\s]", " ") %>%
      str_squish(),
    
    Cleaned_Description = Description_Expanded %>%
      remove_emoticons() %>%
      tolower() %>%
      str_replace_all("<.*?>", "") %>%
      str_replace_all("http\\S+|www\\S+", "") %>%
      str_replace_all("[^[:alnum:]\\s]", " ") %>%
      str_squish()
  )

cat("Cleaning HTML, URLs, Special Chars & Emoticons:\n")
head(df[, c("Cleaned_Title", "Cleaned_Description")])

misspelled_title_words <- hunspell(df$Cleaned_Title)
misspelled_desc_words <- hunspell(df$Cleaned_Description)

unique_title_mistakes <- unique(unlist(misspelled_title_words))
unique_desc_mistakes <- unique(unlist(misspelled_desc_words))

cat(" Unique Misspelled Words in Title:\n")
print(unique_title_mistakes)

cat("Unique Misspelled Words in Description:\n")
print(unique_desc_mistakes)

head(df[, c("Cleaned_Title", "Cleaned_Description")])

df <- df %>%
  mutate(
    Title_Tokens = strsplit(Cleaned_Title, " "),
    Description_Tokens = strsplit(Cleaned_Description, " ")
  )

cat("\n After Tokenization:\n")
head(df[, c("Title_Tokens", "Description_Tokens")])

all_tokens <- unlist(c(df$Title_Tokens, df$Description_Tokens))
all_tokens <- all_tokens[nchar(all_tokens) > 0]
unique_words <- unique(all_tokens)

cat("Total unique words:", length(unique_words), "\n")

head(unique_words, length(unique_words))

stop_words <- stopwords("en")

has_stopword <- function(text) {
  words <- unlist(strsplit(tolower(text), "\\s+"))
  any(words %in% stop_words)
}

stopword_in_title <- any(sapply(df$Title_Cleaned, has_stopword))
stopword_in_desc <- any(sapply(df$Description_Cleaned, has_stopword))

cat("Stopwords in Title:", stopword_in_title, "\n")
cat("Stopwords in Description:", stopword_in_desc, "\n")

df <- df %>%
  mutate(
    Title_Tokens_NoStop = lapply(Title_Tokens, function(words) words[!tolower(words) %in% stop_words]),
    Description_Tokens_NoStop = lapply(Description_Tokens, function(words) words[!tolower(words) %in% stop_words])
  )

cat("\n After Stop Word Removal:\n")
head(df[, c("Title_Tokens_NoStop", "Description_Tokens_NoStop")])

df <- df %>%
  mutate(
    Title_FinalTokens = lapply(Title_Tokens_NoStop, lemmatize_words),
    Description_FinalTokens = lapply(Description_Tokens_NoStop, lemmatize_words)
  )

df[1, c("Title_Tokens_NoStop", "Title_FinalTokens", "Description_Tokens_NoStop", "Description_FinalTokens")]

df_export <- df %>%
  mutate(
    Title_Tokens = sapply(Title_Tokens, paste, collapse = " "),
    Description_Tokens = sapply(Description_Tokens, paste, collapse = " "),
    Title_Tokens_NoStop = sapply(Title_Tokens_NoStop, paste, collapse = " "),
    Description_Tokens_NoStop = sapply(Description_Tokens_NoStop, paste, collapse = " "),
    Title_FinalTokens = sapply(Title_FinalTokens, paste, collapse = " "),
    Description_FinalTokens = sapply(Description_FinalTokens, paste, collapse = " ")
  )

write.csv(df_export, "Fox News_preprocessed.csv", row.names = FALSE)