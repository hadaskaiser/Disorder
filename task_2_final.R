
library(dplyr)
library(tidyr)
library(ggplot2)
library(quanteda)
library(textclean)
library(sentimentr)
library(quanteda.textstats)
library(ggbeeswarm)
library(remotes)
library(car)

setwd("~/Documents/Masters/YearOne/Semester2/Seminar_DS/Data")
#load("~/Documents/Masters/YearOne/Semester2/Seminar_DS/Task 2/line150.RData")
set.seed(64)

data_reddit <- read.csv("~/Documents/Masters/YearOne/Semester2/Seminar_DS/Data/reddit.csv", header = TRUE) %>%
  mutate(row_number = row_number())

twitter_bipolar <- read.csv("~/Documents/Masters/YearOne/Semester2/Seminar_DS/Data/bipolar_tweets.csv", header = TRUE)

twitter_anxity <- read.csv("~/Documents/Masters/YearOne/Semester2/Seminar_DS/Data/anxiety_tweets.csv", header = TRUE)

sampled_bipolar <- sample_n(twitter_bipolar, 3000)
sampled_anxiety <- sample_n(twitter_anxity, 3000)

data_twitter <- bind_rows(sampled_bipolar, sampled_anxiety) %>%
  mutate(row_number = row_number())

data_twitter <- data_twitter %>%
  rename(tweet = column2, group = column3)


tidy_text <- function(text) {
  text <- textclean::replace_non_ascii(text)
  text <- textclean::replace_contraction(text)
  text <- textclean::replace_symbol(text)
  text <- textclean::strip(text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

clean_reddit <- tidy_text(data_reddit[,2])
clean_twitter <- tidy_text(data_twitter[,1])

sentiment_dic_reddit <- sentiment_by(clean_reddit)
sentiment_dic_reddit <- cbind(data_reddit, sentiment_dic_reddit[,c(2,4)]) %>%
  rename("doc_id" = row_number)
sentiment_dic_reddit_filtered <- sentiment_dic_reddit %>%
  filter(abs(ave_sentiment) > 0.0005)

sentiment_dic_twitter <- sentiment_by(clean_twitter)
sentiment_dic_twitter <- cbind(data_twitter, sentiment_dic_twitter[,c(2,4)]) %>%
  rename("doc_id" = row_number)

sentiment_dic_twitter_filtered <- sentiment_dic_twitter %>%
  filter(abs(ave_sentiment) > 0.0005)

corp_reddit <- corpus(data_reddit, docid_field = "row_number", text_field = "comment_body")
token_reddit <- tokens(corp_reddit, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
dfm_reddit <- dfm(token_reddit)

corp_twitter <- corpus(data_twitter, docid_field = "row_number", text_field = "tweet")
token_twitter <- tokens(corp_twitter, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, split_tags = TRUE)
dfm_twitter <- dfm(token_twitter)

remotes::install_github("quanteda/quanteda.sentiment")
library(quanteda.sentiment)
hashtag <- read.csv("~/Documents/Masters/YearOne/Semester2/Seminar_DS/Data/NRC-Emotion-Intensity-Lexicon-v1.csv", header = TRUE)
emotions <- c("anger", "joy", "fear", "sadness", "disgust")

get_valence_for_emotion <- function(emotion, dfm, lexicon_df) {
  emotion_dict <- dictionary(list(emotion = lexicon_df$token[lexicon_df$emotion == emotion]))
  valence(emotion_dict) <- list(emotion = lexicon_df$score[lexicon_df$emotion == emotion])
  textstat_valence(dfm, emotion_dict, normalization = "all") %>% rename(!!emotion := sentiment)
}

valence_list_reddit <- lapply(emotions, get_valence_for_emotion, dfm = dfm_reddit, lexicon_df = hashtag)

NRC_reddit <- Reduce(function(x, y) full_join(x, y, by = "doc_id"), valence_list_reddit) %>%
  right_join(convert(corp_reddit, to = "data.frame"), by = "doc_id")

valence_list_twitter <- lapply(emotions, get_valence_for_emotion, dfm = dfm_twitter, lexicon_df = hashtag)

NRC_twitter <- Reduce(function(x, y) full_join(x, y, by = "doc_id"), valence_list_twitter) %>%
  right_join(convert(corp_twitter, to = "data.frame"), by = "doc_id")


# Results & Visualizations


summary_stats_reddit_sen_dic <- sentiment_dic_reddit %>%
  group_by(Group) %>%
  summarise(
    amount = n(),
    min_sent = round(min(ave_sentiment, na.rm = TRUE),3),
    max_sent = round(max(ave_sentiment, na.rm = TRUE),3),
    mean_sent = round(mean(ave_sentiment, na.rm = TRUE),3),
    median_sent = round(median(ave_sentiment, na.rm = TRUE),4),
    sd_sent = round(sd(ave_sentiment, na.rm = TRUE),3),
    .groups = "drop"
  )

print(summary_stats_reddit_sen_dic)

(significant_reddit_sen_dic<- leveneTest(sentiment_dic_reddit$ave_sentiment ~ Group, data = sentiment_dic_reddit))

ggplot(sentiment_dic_reddit, aes(x = Group, y = ave_sentiment, color = Group)) +
  geom_quasirandom(width = 0.4, alpha = 0.64, varwidth = TRUE) +  
  labs(title = "Average Sentiment by Group",
       x = "Group", y = "Average Sentiment") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")



summary_stats_reddit_sen_dic_fil <- sentiment_dic_reddit_filtered %>%
  group_by(Group) %>%
  summarise(
    amount = n(),
    min_sent = round(min(ave_sentiment, na.rm = TRUE),3),
    max_sent = round(max(ave_sentiment, na.rm = TRUE),3),
    mean_sent = round(mean(ave_sentiment, na.rm = TRUE),3),
    median_sent = round(median(ave_sentiment, na.rm = TRUE),3),
    sd_sent = round(sd(ave_sentiment, na.rm = TRUE),3),
    .groups = "drop"
  )

print(summary_stats_reddit_sen_dic_fil)

ggplot(sentiment_dic_reddit_filtered, aes(x = Group, y = ave_sentiment, color = Group)) +
  geom_quasirandom(width = 0.4, alpha = 0.64, varwidth = TRUE) +  
  labs(title = "Average Sentiment by Group (filtered)",
       x = "Group", y = "Average Sentiment") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")
                      
summary_stats_twitter_sen_dic <- sentiment_dic_twitter %>%
  group_by(group) %>%
  summarise(
    count = n(),
    min_sent = round(min(ave_sentiment, na.rm = TRUE),3),
    max_sent = round(max(ave_sentiment, na.rm = TRUE),3),
    mean_sent = round(mean(ave_sentiment, na.rm = TRUE),3),
    median_sent = round(median(ave_sentiment, na.rm = TRUE),3),
    sd_sent = round(sd(ave_sentiment, na.rm = TRUE),3),
    .groups = "drop"
  )

print(summary_stats_twitter_sen_dic)

(significant_twitter_sen_dic<- leveneTest(sentiment_dic_twitter$ave_sentiment ~ group, data = sentiment_dic_twitter))

summary(significant_twitter_sen_dic)

ggplot(sentiment_dic_twitter, aes(x = group, y = ave_sentiment, color = group)) +
  geom_quasirandom(width = 0.4, alpha = 0.64, varwidth = TRUE) +  
  labs(title = "Average Sentiment by Group",
       x = "Group", y = "Average Sentiment") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")

summary_stats_twitter_sen_dic_fil <- sentiment_dic_twitter_filtered %>%
  group_by(group) %>%
  summarise(
    min_sent = round(min(ave_sentiment, na.rm = TRUE),3),
    max_sent = round(max(ave_sentiment, na.rm = TRUE),3),
    mean_sent = round(mean(ave_sentiment, na.rm = TRUE),3),
    median_sent = round(median(ave_sentiment, na.rm = TRUE),3),
    sd_sent = round(sd(ave_sentiment, na.rm = TRUE),3),
    .groups = "drop"
  )

print(summary_stats_twitter_sen_dic_fil)

ggplot(sentiment_dic_twitter_filtered, aes(x = group, y = ave_sentiment, color = group)) +
  geom_quasirandom(width = 0.4, alpha = 0.64, varwidth = TRUE) +  
  labs(title = "Average Sentiment by Group (filtered)",
       x = "Group", y = "Average Sentiment") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")

summary_stats_NRC_reddit <- NRC_reddit %>%
  group_by(Group) %>%
  summarise(
    amount = n(),
    Anger = round(mean(anger, na.rm = TRUE),digits = 3),
    Joy = round(mean(joy, na.rm = TRUE),digits = 3),
    Sadness = round(mean(sadness, na.rm = TRUE),digits = 3),
    Disgust = round(mean(disgust, na.rm = TRUE),digits = 3),
    Fear = round(mean(fear, na.rm = TRUE),digits = 3),
    .groups = "drop"
  )

print(summary_stats_NRC_reddit)

reg_NRC_reddit_anger <- lm(NRC_reddit$anger ~ Group,NRC_reddit)
reg_NRC_reddit_joy <- lm(NRC_reddit$joy ~ Group,NRC_reddit)
reg_NRC_reddit_sadness <- lm(NRC_reddit$sadness ~ Group,NRC_reddit)
reg_NRC_reddit_disgust <- lm(NRC_reddit$disgust ~ Group,NRC_reddit)
reg_NRC_reddit_fear <- lm(NRC_reddit$fear ~ Group,NRC_reddit)


summary(reg_NRC_reddit_anger)
summary(reg_NRC_reddit_joy)
summary(reg_NRC_reddit_sadness)
summary(reg_NRC_reddit_disgust)
summary(reg_NRC_reddit_fear)

NRC_reddit$Group <- as.factor(NRC_reddit$Group)
reg_log_NRC_reddit <- glm(NRC_reddit$Group ~ NRC_reddit$anger+ NRC_reddit$joy+ NRC_reddit$sadness+ NRC_reddit$disgust+ NRC_reddit$fear,family=binomial(link='logit'))

summary(reg_log_NRC_reddit)



reddit_long <- NRC_reddit %>%
  select(doc_id, anger, joy, fear, sadness, disgust, Group) %>%
  pivot_longer(cols = c("anger", "joy", "fear", "sadness", "disgust"),
               names_to = "emotion", values_to = "intensity")


ggplot(reddit_long, aes(x = emotion, y = intensity, color = Group)) +
  geom_quasirandom(dodge.width = 0.7, alpha = 0.6) +
  labs(title = "Emotion Intensity by Group (Reddit)", x = "Emotion", y = "Intensity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


summary_stats_NRC_twitter <- NRC_twitter %>%
  group_by(group) %>%
  summarise(
    amount = n(),
    Anger = round(mean(anger, na.rm = TRUE),digits = 3),
    Joy = round(mean(joy, na.rm = TRUE),digits = 3),
    Sadness = round(mean(sadness, na.rm = TRUE),digits = 3),
    Disgust = round(mean(disgust, na.rm = TRUE),digits = 3),
    Fear = round(mean(fear, na.rm = TRUE),digits = 3),
    .groups = "drop"
  )

print(summary_stats_NRC_twitter)




reg_NRC_twitter_anger <- lm(NRC_twitter$anger ~ group,NRC_twitter)
reg_NRC_twitter_joy <- lm(NRC_twitter$joy ~ group,NRC_twitter)
reg_NRC_twitter_sadness <- lm(NRC_twitter$sadness ~ group,NRC_twitter)
reg_NRC_twitter_disgust <- lm(NRC_twitter$disgust ~ group,NRC_twitter)
reg_NRC_twitter_fear <- lm(NRC_twitter$fear ~ group,NRC_twitter)

summary(reg_NRC_twitter_anger)
summary(reg_NRC_twitter_joy)
summary(reg_NRC_twitter_sadness)
summary(reg_NRC_twitter_disgust)
summary(reg_NRC_twitter_fear)

NRC_twitter$group <- as.factor(NRC_twitter$group)
reg_log_NRC_twitter <- glm(NRC_twitter$group ~ NRC_twitter$anger+ NRC_twitter$joy+ NRC_twitter$sadness+ NRC_twitter$disgust+ NRC_twitter$fear,family=binomial(link='logit'))
summary(reg_log_NRC_twitter)



twitter_long <- NRC_twitter %>%
  select(doc_id, anger, joy, fear, sadness, disgust, group) %>%
  pivot_longer(cols = c("anger", "joy", "fear", "sadness", "disgust"),
               names_to = "emotion", values_to = "intensity")


ggplot(twitter_long, aes(x = emotion, y = intensity, color = group)) +
  geom_quasirandom(dodge.width = 0.7, alpha = 0.6) +
  labs(title = "Emotion Intensity by Group (Twitter)", x = "Emotion", y = "Intensity") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

