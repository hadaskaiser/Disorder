#install.packages("sentimentr")
#install.packages("quanteda.textstats")
library(quanteda)
library(dplyr)
library(textclean)
library(sentimentr)
library(quanteda.textstats)



#load data
redit_data<- read.csv(choose.files())
redit_data <- redit_data %>% mutate(row_number = row_number())



tweeter_bipolar<-read.csv(choose.files())
tweeter_anxity<- read.csv(choose.files())
tweeter_general<- rbind(tweeter_anxity, tweeter_bipolar)
head(tweeter_general)


# Function to clean text
tidy_text <- function(text) {
  text <- textclean::replace_non_ascii(text)
  text <- textclean::replace_contraction(text)
  text <- textclean::replace_symbol(text)
  text <- textclean::strip(text)
  text <- gsub("\\s+", " ", text) # Remove extra spaces
  text <- trimws(text)
  return(text)
}
#cleaning for sentmentr dictionary
clean_redit<-tidy_text(redit_data[,2])
clean_tweet<- tidy_text(tweeter_general[,2])
#sentmentr 
sentiment_dic_redit<- sentiment_by(clean_redit) # dic results reddit

sentiment_dic_tweeter<- sentiment_by(clean_tweet) # dic results tweeter
#need to be saved and saved in the environment 
##################################


############ reddit ###############################################

#################Corpus

redit_corp <- corpus(redit_data,
                     docid_field = "row_number",
                     text_field = "comment_body")

#########Token

redit_tokens <- tokens(redit_corp,
                       remove_punct = TRUE,
                       remove_url = TRUE,
                       remove_numbers = T,
                       remove_separators  = T,
                       remove_symbols = T)

reddit_dfm <- dfm(redit_tokens)

############ Twitter ###############################################

#################Corpus

tweet_corp <- corpus(tweeter_general,
                     docid_field = "row_number",
                     text_field = "column2")

#########Token

tweet_tokens <- tokens(tweet_corp,
                       remove_punct = TRUE,
                       remove_url = TRUE,
                       remove_numbers = T,
                       remove_separators  = T,
                       split_tags = T)


tweet_dfm <- dfm(tweet_tokens)

######################### DFMs to use run dictionaries on! #####################
reddit_dfm
tweet_dfm


########dictionary 2 

#install.packages("remotes")
library(remotes)
remotes::install_github("quanteda/quanteda.sentiment")
library(quanteda.sentiment)

############

hashtag <- readr::read_tsv(choose.files(), col_names = c( "token","emotion", "score"))
#anger,anticipation,disgust,fear,joy,sadness,surprise,trust

########################
# Define the target emotions
emotions <- c("anger", "joy", "fear", "sadness", "disgust")

# Function to compute valence for a single emotion
get_valence_for_emotion <- function(emotion, dfm, lexicon_df) {
  # Create a dictionary with words for this emotion
  emotion_dict <- dictionary(list(
    emotion = lexicon_df$token[lexicon_df$emotion == emotion]
  ))
  
  # Assign valence scores to the dictionary
  valence(emotion_dict) <- list(
    emotion = lexicon_df$score[lexicon_df$emotion == emotion]
  )
  
  # Compute valence scores
  val_df <- textstat_valence(dfm, emotion_dict, normalization = "all") |>
    rename(!!emotion := sentiment)
  
  return(val_df)
}

#################redit
# Apply the function for each emotion
valence_list <- lapply(emotions, get_valence_for_emotion, 
                       dfm = reddit_dfm, 
                       lexicon_df = hashtag)

# Merge all emotion valence results by document
reddit_valence <- Reduce(function(x, y) full_join(x, y, by = "doc_id"), valence_list)

# Join with original corpus metadata (if needed)
reddit_valence <- reddit_valence |>
  right_join(convert(redit_corp, to = "data.frame"), by = "doc_id")
#need to be saved and saved in the environment 
####################twitter
# Apply the function for each emotion
valence_list <- lapply(emotions, get_valence_for_emotion, 
                       dfm = tweet_dfm, 
                       lexicon_df = hashtag)

# Merge all emotion valence results by document
tweet_valence <- Reduce(function(x, y) full_join(x, y, by = "doc_id"), valence_list)


# Join with original corpus metadata (if needed)
tweet_valence <- tweet_valence |>
  right_join(convert(tweet_corp, to = "data.frame"), by = "doc_id")
#need to be saved and saved in the environment 
