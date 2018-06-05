source("libraries.R")
library(sentimentr) # For sentiment analysis
library(ggplot2) # For graph plotting and visualization

print("****************** Reading the dataset .......")

# Reading the dataset
CAvideos = read_csv(file = ca_loc)

##### Now we will analyze the description of the videos 

print("****************** Analyzing the sentiments in the description .......")

# Analyzing the sentiments in the description

# Removing the duplicate entries of description
newCAvideos = CAvideos %>% 
				group_by(description) %>%
				summarise(likes = max(likes, rm.an = T))

desc_score = sentiment(get_sentences(newCAvideos$description)) %>% 
				group_by(element_id) %>%
				summarise(word_count = sum(word_count), sentiment = sum(sentiment))

png("description_score.png")

# Visualizing how the sentiment scores of the description are distributed
ggplot(desc_score) +
  geom_histogram(mapping = aes(x=sentiment),binwidth = .1) +
  theme_bw() + 
  scale_fill_brewer(palette = "Set1") +
  geom_vline(xintercept = 0, color = "red", size = 1.5, alpha = 0.6, linetype = "longdash") +
  labs(title="Description Score",caption="Kartheek") +
  coord_cartesian(xlim = c(-5, 5))

dev.off()
