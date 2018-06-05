source("libraries.R")
library(ggplot2) # For graph plotting and visualization

print("****************** Reading the dataset .......")

# Reading the dataset
CAvideos = read_csv(file = ca_loc)
CAvideos$location = "CA"
USvideos = read_csv(file = us_loc)
USvideos$location = "US"
FRvideos = read_csv(file = fr_loc)
FRvideos$location = "FR"
GBvideos = read_csv(file = gb_loc)
GBvideos$location = "GB"
DEvideos = read_csv(file = de_loc)
DEvideos$location = "DE"

all_videos = rbind(CAvideos, USvideos, FRvideos, GBvideos, DEvideos)

new_all_videos = all_videos %>% 
					group_by(location,title,thumbnail_link) %>%
					summarise(views = max(views, rm.na = T), likes = max(likes, rm.na = T),
						dislikes = max(dislikes,rm.na = T), comment_count = max(comment_count, rm.na = T))
new_all_videos

new_all_videos = new_all_videos %>% 
					group_by(location) %>%
					summarise(Total_views = sum(as.numeric(views)), Total_likes = sum(likes), Total_dislikes = sum(dislikes),
						Total_comments = sum(comment_count))

new_all_videos

png("all_countries_views.png")

ggplot(new_all_videos, aes(reorder(location,-Total_views),Total_views, fill = location)) + 
		geom_bar(stat = "identity") +
		theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
		labs(caption="Kartheek", title="Total views of all countries ") + 
		xlab(NULL)+ylab("Total views count")

dev.off()


png("all_countries_likes.png")

ggplot(new_all_videos, aes(reorder(location,-Total_likes),Total_likes, fill = location)) + 
		geom_bar(stat = "identity") +
		theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
		labs(caption="Kartheek", title="Total likes of all countries ") + 
		xlab(NULL)+ylab("Total likes")

dev.off()


png("all_countries_dislikes.png")

ggplot(new_all_videos, aes(reorder(location,-Total_dislikes),Total_dislikes, fill = location)) + 
		geom_bar(stat = "identity") +
		theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
		labs(caption="Kartheek", title="Total dislikes of all countries ") + 
		xlab(NULL)+ylab("Total dislikes")

dev.off()


png("all_countries_comments.png")

ggplot(new_all_videos, aes(reorder(location,-Total_comments),Total_comments, fill = location)) + 
		geom_bar(stat = "identity") +
		theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
		labs(caption="Kartheek", title="Total comments count of all countries ") + 
		xlab(NULL)+ylab("Total comments count")

dev.off()




