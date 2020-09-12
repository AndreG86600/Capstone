
library(tidytext)
library(tidyverse)
library(stringr)
library(knitr)

#' ## Load the Data
#+ DataLoading

#' English Repository Files
blogs_file   <- "final/en_US/en_US.blogs.txt"
news_file    <- "final/en_US/en_US.news.txt"
twitter_file <- "final/en_US/en_US.twitter.txt"  

#' Read the data files
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

#' Create dataframes
blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)

#' ## Sample the data
#+ DataSampling
set.seed(1001)
sample_pct <- 0.10

blogs_sample <- blogs %>%
        sample_n(., nrow(blogs)*sample_pct)
news_sample <- news %>%
        sample_n(., nrow(news)*sample_pct)
twitter_sample <- twitter %>%
        sample_n(., nrow(twitter)*sample_pct)

#' Create tidy repository
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)

#' Clean up
rm(list = c("blogs", "blogs_file", "blogs_sample","news", "news_file",     
            "news_sample", "sample_pct", "twitter","twitter_file", 
            "twitter_sample"))

#' ## Clean the sample data
#' Create filters: non-alphanumeric's, url's, repeated letters(+3x)
#+ Data Cleaning
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
#' and ngrams.
clean_sample <-  repo_sample %>%
        mutate(text = str_replace_all(text, replace_reg, "")) %>%
        mutate(text = str_replace_all(text, replace_url, "")) %>%
        mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
        mutate(text = iconv(text, "ASCII//TRANSLIT"))

rm(list = c("repo_sample"))

#' ## Create all n-grams
#+ Ngrams 
#' Bigrams
bigram_repo <- clean_sample  %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' Trigrams
trigram_repo <- clean_sample  %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' Quadgrams
quadgram_repo <- clean_sample  %>%
        unnest_tokens(quadgram, text, token = "ngrams", n = 4)

#' Quintgrams
quintgram_repo <- clean_sample  %>%
        unnest_tokens(quintgram, text, token = "ngrams", n = 5)

#' Sextgrams
sextgram_repo <- clean_sample  %>%
        unnest_tokens(sextgram, text, token = "ngrams", n = 6)


#' ## Reduce n-grams files
#+ ReduceNgrams 
#' Bigrams
bigram_cover <- bigram_repo %>%
        count(bigram) %>%  
        filter(n > 10) %>%
        arrange(desc(n))  
rm(list = c("bigram_repo"))

#' Trigrams
trigram_cover <- trigram_repo %>%
        count(trigram) %>%  
        filter(n > 10) %>%
        arrange(desc(n))  
rm(list = c("trigram_repo"))

#' Quadgrams
quadgram_cover <- quadgram_repo %>%
        count(quadgram) %>%  
        filter(n > 10) %>%
        arrange(desc(n))  
rm(list = c("quadgram_repo"))

#' Quintgrams
quintgram_cover <- quintgram_repo %>%
        count(quintgram) %>%  
        filter(n > 10) %>%
        arrange(desc(n))  
rm(list = c("quintgram_repo"))

#' Sextgrams
sextgram_cover <- sextgram_repo %>%
        count(sextgram) %>%  
        filter(n > 10) %>%
        arrange(desc(n))  
rm(list = c("sextgram_repo"))

#' ## Separate words
#+ NgramWords 
bi_words <- bigram_cover %>%
        separate(bigram, c("word1", "word2"), sep = " ")
bi_words

tri_words <- trigram_cover %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ")
tri_words

quad_words <- quadgram_cover %>%
        separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quad_words

quint_words <- quintgram_cover %>%
        separate(quintgram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
quint_words

sext_words <- sextgram_cover %>%
        separate(sextgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
sext_words

#' Create Ngram Matching Functions
bigram <- function(input_words){
        num <- length(input_words)
        filter(bi_words, 
               word1==input_words[num]) %>% 
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 2)) %>%
                as.character() -> out
        ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
        num <- length(input_words)
        filter(tri_words, 
               word1==input_words[num-1], 
               word2==input_words[num])  %>% 
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 3)) %>%
                as.character() -> out
        ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
        num <- length(input_words)
        filter(quad_words, 
               word1==input_words[num-2], 
               word2==input_words[num-1], 
               word3==input_words[num])  %>% 
                top_n(1, n) %>%
                filter(row_number() == 1L) %>%
                select(num_range("word", 4)) %>%
                as.character() -> out
        ifelse(out=="character(0)", trigram(input_words), return(out))
}

#' Create User Input and Data Cleaning Function; Calls the matching functions
ngrams <- function(input){
        # Create a dataframe
        input <- data_frame(text = input)
        # Clean the Inpput
        replace_reg <- "[^[:alpha:][:space:]]*"
        input <- input %>%
                mutate(text = str_replace_all(text, replace_reg, ""))
        # Find word count, separate words, lower case
        input_count <- str_count(input, boundary("word"))
        input_words <- unlist(str_split(input, boundary("word")))
        input_words <- tolower(input_words)
        # Call the matching functions
        out <- ifelse(input_count == 1, bigram(input_words), 
                      ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
        # Output
        return(out)
}

#' User Input and Program Ouput
input <- "In case of a"
ngrams(input)
