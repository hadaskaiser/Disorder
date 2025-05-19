# Required packages
# install.packages("sentimentr")
# install.packages("quanteda.textstats")
# install.packages("ggwordcloud")
# install.packages("ggiraph")

# Load necessary libraries
library(quanteda)          # For text analysis
library(dplyr)             # For data manipulation
library(textclean)         # For cleaning text
library(sentimentr)        # For sentiment analysis (although not used in this snippet)
library(quanteda.textstats) # For statistical analysis of text data
library(tidyr)             # For reshaping data
library(ggplot2)           # For creating plots
library(ggiraph)           # For interactive ggplot graphics
library(ggrepel)           # For non-overlapping text labels in plots
library(stringr)           # For string manipulation
library(ggwordcloud)       # For creating word clouds
library(readr)             # For reading data files

# --------------------- Load and prepare data ---------------------

# Reddit data loading and preparation
reddit_data <- read_csv("reddit_posts_and_comments_edited.csv", show_col_types = FALSE) %>%
  mutate(row_number = row_number()) # Add a row number as a document identifier

# Twitter data loading and preparation
twitter_bipolar <- read_csv("bipolar_tweets.csv",show_col_types = FALSE)   # Load bipolar tweets
twitter_anxiety <- read_csv("anxiety_tweets.csv",show_col_types = FALSE)   # Load anxiety tweets

# Sample an equal number of tweets from each group
sampled_bipolar <- sample_n(twitter_bipolar, 3000)
sampled_anxiety <- sample_n(twitter_anxiety, 3000)

# Combine the sampled Twitter data and prepare it for analysis
data_twitter <- bind_rows(sampled_bipolar, sampled_anxiety) %>%
  mutate(row_number = row_number()) %>% # Add a row number
  rename(tweet = column2, Group = column3) # Rename columns for clarity

# --------------------- Create corpus and tokens ---------------------

# Reddit: Create a corpus object
reddit_corp <- corpus(reddit_data, docid_field = "row_number", text_field = "comment_body")

# Reddit: Tokenize the corpus (break text into words) and apply cleaning
reddit_tokens <- tokens(reddit_corp,
                        remove_punct = TRUE,
                        remove_url = TRUE,
                        remove_numbers = TRUE,
                        remove_separators = TRUE,
                        remove_symbols = TRUE
)

# Reddit: Create a Document-Feature Matrix (DFM)
reddit_dfm <- dfm(reddit_tokens)

# Twitter: Create a corpus object
twitter_corp <- corpus(data_twitter, docid_field = "row_number", text_field = "tweet")

# Twitter: Tokenize the corpus and apply cleaning (including splitting hashtags)
twitter_tokens <- tokens(twitter_corp,
                         remove_punct = TRUE,
                         remove_url = TRUE,
                         remove_numbers = TRUE,
                         remove_separators = TRUE,
                         split_tags = TRUE # Important for Twitter hashtags
)

# Twitter: Create a Document-Feature Matrix (DFM)
twitter_dfm <- dfm(twitter_tokens)

# --------------------- Reddit Analysis: Frequency & Keyness ---------------------

# Reddit: Calculate word frequencies by group
anxiety_vs_bipolar_Reddit <- reddit_dfm %>%
  textstat_frequency(groups = Group)

head(anxiety_vs_bipolar_Reddit)

# Reddit: Reshape the frequency table for comparison
anxiety_vs_bipolar_Reddit <- anxiety_vs_bipolar_Reddit %>%
  filter(group %in% c("anxiety", "bipolar")) %>%
  pivot_wider(
    id_cols = "feature",
    names_from = "group",
    values_from = "frequency",
    names_prefix = "count_"
  ) %>%
  mutate(
    freq_anxiety = count_anxiety / sum(count_anxiety, na.rm = TRUE), # Calculate relative frequency in anxiety group
    freq_bipolar = count_bipolar / sum(count_bipolar, na.rm = TRUE), # Calculate relative frequency in bipolar group
    anxiety_bipolar_ratio = freq_anxiety / freq_bipolar # Calculate the ratio of frequencies
  )

head(anxiety_vs_bipolar_Reddit)

# Reddit: Create an interactive frequency-frequency plot
p <- anxiety_vs_bipolar_Reddit %>%
  mutate(
    # Calculate the average frequency for the size of the points
    common = (freq_anxiety + freq_bipolar) / 2,
    # Remove single quotes which can cause issues with HTML tooltips
    feature = str_replace_all(feature, "'", "`")
  ) %>%
  ggplot(aes(
    x = anxiety_bipolar_ratio,
    y = common,
    label = feature,
    color = anxiety_bipolar_ratio,
    tooltip = feature,
    data_id = feature
  )) +
  geom_point_interactive() + # Interactive points
  geom_text_repel_interactive(size = 2) + # Non-overlapping interactive labels
  scale_y_continuous(
    trans = "log2", # Logarithmic scale for y-axis to handle wide frequency range
    breaks = ~.x,
    minor_breaks = ~ 2^(seq(0, log2(.x[2]))),
    labels = c("Rare", "Common")
  ) +
  scale_x_continuous(
    trans = "log10", # Logarithmic scale for x-axis (ratio)
    limits = c(1 / 10, 10),
    breaks = c(1 / 10, 1, 10),
    labels = c(
      "10x More Common\nin bipolar related reddit",
      "Equal Proportion",
      "10x More Common\nin anxiety related reddit"
    )
  ) +
  scale_color_gradientn(
    colors = c("#023903", "#318232", "#E2E2E2", "#9B59A7", "#492050"), # Color gradient based on the ratio
    trans = "log2", # Apply log scale to color as well
    guide = "none" # Hide the color legend
  ) +
  labs(
    title = "Words in anxiety and bipolar reddit",
    x = "",
    y = "Total Frequency",
    color = ""
  ) +
  coord_fixed(ratio = 1 / 8) + # Fix the aspect ratio
  theme_minimal() # Use a minimal theme

# Make the ggplot interactive with tooltips
girafe_options(
  girafe(ggobj = p),
  opts_tooltip(css = "font-family:sans-serif;font-size:1em;color:Black;") # Style the tooltips
)

# Reddit: Calculate keyness (identify words that are significantly more frequent in one group)
reddit_keyness_dfm <- reddit_dfm %>%
  dfm_trim(min_docfreq = 30) %>% # Keep words appearing in at least 30 documents
  dfm_subset(Group %in% c("anxiety", "bipolar")) # Subset to the two groups

reddit_keyness <- textstat_keyness(reddit_keyness_dfm,
                                   docvars(reddit_keyness_dfm, "Group") == "anxiety", # Compare anxiety group against bipolar group
                                   measure = "lr" # Use likelihood ratio test
)

# Reddit: Create a word cloud of key words
set.seed(2) # For reproducibility of the word cloud layout
reddit_keyness %>%
  filter(p < 0.05) %>% # Filter for words with a p-value less than 0.05 (significant difference)
  arrange(desc(abs(G2))) %>% # Arrange by the absolute value of the keyness statistic
  ggplot(aes(
    label = feature,
    size = G2,
    color = G2 > 0, # Color based on which group the word is more associated with
    angle_group = G2 > 0 # Angle words differently based on the group
  )) +
  geom_text_wordcloud_area(eccentricity = 1, show.legend = TRUE) +
  scale_size_area(max_size = 30, guide = "none") +
  scale_color_discrete(name = "", labels = c("More in bipolar", "More in anxiety")) +
  labs(caption = "Only words with a significant difference (p < 0.05) were included (Reddit).") +
  theme_void() # Remove plot background and axes

# --------------------- Twitter Analysis: Frequency & Keyness ---------------------

# Twitter: Calculate word frequencies by group
anxiety_vs_bipolar_twitter <- twitter_dfm %>%
  textstat_frequency(groups = Group)

head(anxiety_vs_bipolar_twitter)

# Twitter: Reshape the frequency table for comparison
anxiety_vs_bipolar_twitter <- anxiety_vs_bipolar_twitter %>%
  filter(group %in% c("anxiety", "bipolar")) %>%
  pivot_wider(
    id_cols = "feature",
    names_from = "group",
    values_from = "frequency",
    names_prefix = "count_"
  ) %>%
  mutate(
    freq_anxiety = count_anxiety / sum(count_anxiety, na.rm = TRUE),
    freq_bipolar = count_bipolar / sum(count_bipolar, na.rm = TRUE),
    anxiety_bipolar_ratio = freq_anxiety / freq_bipolar
  )

head(anxiety_vs_bipolar_twitter)

# Twitter: Create an interactive frequency-frequency plot
p_twitter <- anxiety_vs_bipolar_twitter %>%
  mutate(
    common = (freq_anxiety + freq_bipolar) / 2,
    feature = str_replace_all(feature, "'", "`")
  ) %>%
  ggplot(aes(
    x = anxiety_bipolar_ratio,
    y = common,
    label = feature,
    color = anxiety_bipolar_ratio,
    tooltip = feature,
    data_id = feature
  )) +
  geom_point_interactive() +
  geom_text_repel_interactive(size = 2) +
  scale_y_continuous(
    trans = "log2",
    breaks = ~.x,
    minor_breaks = ~ 2^(seq(0, log2(.x[2]))),
    labels = c("Rare", "Common")
  ) +
  scale_x_continuous(
    trans = "log10",
    limits = c(1 / 10, 10),
    breaks = c(1 / 10, 1, 10),
    labels = c(
      "10x More Common\nin bipolar Twitter",
      "Equal Proportion",
      "10x More Common\nin anxiety Twitter"
    )
  ) +
  scale_color_gradientn(
    colors = c("#023903", "#318232", "#E2E2E2", "#9B59A7", "#492050"),
    trans = "log2",
    guide = "none"
  ) +
  labs(
    title = "Words in anxiety and bipolar Twitter posts",
    x = "",
    y = "Total Frequency",
    color = ""
  ) +
  coord_fixed(ratio = 1 / 8) +
  theme_minimal()

girafe_options(
  girafe(ggobj = p_twitter),
  opts_tooltip(css = "font-family:sans-serif;font-size:1em;color:Black;")
)

# Twitter: Calculate keyness
twitter_keyness_dfm <- twitter_dfm %>%
  dfm_trim(min_docfreq = 30) %>%
  dfm_subset(Group %in% c("anxiety", "bipolar"))

twitter_keyness <- textstat_keyness(twitter_keyness_dfm,
                                    docvars(twitter_keyness_dfm, "Group") == "anxiety",
                                    measure = "lr"
)

# Twitter: Create a word cloud of key words
set.seed(2)
twitter_keyness %>%
  filter(p < 0.05) %>% # Changed p-value threshold to 0.05
  arrange(desc(abs(G2))) %>%
  ggplot(aes(
    label = feature,
    size = G2,
    color = G2 > 0,
    angle_group = G2 > 0
  )) +
  geom_text_wordcloud_area(eccentricity = 1, show.legend = TRUE) +
  scale_size_area(max_size = 30, guide = "none") +
  scale_color_discrete(name = "", labels = c("More in bipolar", "More in anxiety")) +
  labs(caption = "Only words with a significant difference (p < 0.05) were included (Twitter).") +
  theme_void()