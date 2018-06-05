source("libraries.R")
library(ggplot2) # For graph plotting and visualization
library(lubridate) # For time manipulation
library(sentimentr) # For sentiment analysis

print("****************** Reading the dataset .......")

# Reading the dataset
CAvideos = read_csv(file = ca_loc)
CAvideos_cat = fromJSON(ca_cat_loc)

CAvideos
CAvideos_cat

#### 1. Top trending categories in Canada

# Retrieving the effective number of videos and total likes, dislikes, views and comments as well as 
# the category id of each video
newCAvideos = CAvideos %>% 
	group_by(title, thumbnail_link) %>% 
	summarise(likes = max(likes,rm.na = T), dislikes = max(dislikes,rm.na = T), 
		views = max(views,rm.na = T), comment_count = max(comment_count,rm.na = T), 
		category_id = max(category_id, rm.na = T))

# Assigning each trending video with its corresponding category name
newCAvideos$category = CAvideos_cat$items$snippet$title[match(newCAvideos$category_id,CAvideos_cat$items$id)]

# Fetching the top trending categories along with total count of videos, mean
trending_cat = newCAvideos %>% 
	group_by(Category = category) %>% 
	summarise(Total_Videos = n(), Average_likes = mean(likes), Average_dislikes = mean(dislikes),
		Average_views = mean(views), Average_comments = mean(comment_count))

# If any unknown category is discovered i.e, NA, replace it with others
trending_cat[is.na(trending_cat)] = "Others"

# Print the top trending categories
trending_cat

png("top_trending_cat.png")

# Plotting the bar graph - Total Videos in each category 
ggplot(trending_cat, aes(reorder(Category,-Total_Videos),Total_Videos, fill = Category)) + 
		geom_bar(stat = "identity") +
		theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
		labs(caption="Kartheek", title=" Top trending categories in Canada") + 
		xlab(NULL)+ylab("Total number of trending videos")

dev.off()

png("top_trending_cat_views.png")

# Plotting the graph - analysis average number of views of each category
ggplot(trending_cat, aes(reorder(Category,-Average_views),Average_views, fill = Category)) + 
		geom_bar(stat = "identity") +
		theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
		labs(caption="Kartheek", title=" Top trending categories in Canada") + 
		xlab(NULL)+ylab("Average views count")

dev.off()

# Visualizing the average number of likes, dislikes and comments count of each category

# Getting the number of rows and columns of the dataframe trending cat
nc = ncol(trending_cat)
nr = nrow(trending_cat)

values = c(trending_cat$Average_likes, trending_cat$Average_dislikes, trending_cat$Average_comments)

Category = rep(as.vector(trending_cat$Category),nc-3)
df = data.frame(Category,values)

type = c(rep("Average likes",nr), rep("Average dislikes",nr), rep("Average comments count",nr))

png("metricAnalysisOfCat.png")

# Plotting the graph
ggplot(df, aes(reorder(Category,-values),values)) + 
	geom_bar(aes(fill = type), position = "dodge", stat="identity") +
	theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
	labs(caption="Kartheek", title=" Top trending categories in Canada") + 
		xlab(NULL)+ylab(NULL)

dev.off()


#####2. Now we would try to analyze the relationship b/w 
	#	1. Views vs Likes
	#	2. Likes vs Comments
	#	3. Views vs Comments

#1. Views vs Likes

png("views_vs_likes.png")

# Plotting the scatter plot between views and likes to see how they are distributed
ggplot(newCAvideos, aes(x=views, y=likes, colour = likes)) +
	geom_jitter() + 
	geom_smooth(method = "auto") + 
	labs(caption="Kartheek",title="Views Vs Likes")

# Saving the plot
dev.off()

#2. Likes vs Comments

png("likes_vs_comments.png")

# Plotting the scatter plot between likes and comments count 
# to see how they are distributed
ggplot(newCAvideos, aes(x=likes, y=comment_count, colour = likes)) +
	geom_jitter() + 
	geom_smooth(method = "auto") + 
	labs(caption="Kartheek",title="Likes Vs Comments")

# Saving the plot
dev.off()

#3. Veiws vs Comments

png("views_vs_comments.png")

# Plotting the scatter plot between views and comments count 
# to see how they are distributed
ggplot(newCAvideos, aes(x=views, y=comment_count, colour = views)) +
	geom_jitter() + 
	geom_smooth(method = "auto") + 
	labs(caption="Kartheek",title="Views Vs Comments")

# Saving the plot
dev.off()


#####3. Top trending channels in Canada

# Retrieving the total trending videos published by each channel
newCAvideos = distinct(CAvideos, title, thumbnail_link, channel_title) %>% 
	group_by(channel_title) %>% 
	summarise(Total_Videos = n()) 

png("Top_trending_channels.png")

# Plot the graph

ggplot(newCAvideos %>% 
	arrange(-Total_Videos) %>%
	top_n(10, wt = Total_Videos), 
	aes(reorder(channel_title,-Total_Videos), Total_Videos, fill = channel_title)) +
	geom_bar(stat = "identity") +
	theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
	labs(caption="Kartheek", title=" Top trending channels in Canada") + 
	xlab(NULL)+ylab("Total number of trending videos")

# Saving the plot
dev.off()

# Now we would find the average trending time for the videos of each trending channel

# Finding the number of days each video trended
CAvideos$trending_date = ydm(CAvideos$trending_date)
CAvideos$publish_time = ymd(substr(CAvideos$publish_time,start = 1,stop = 10))
CAvideos$trend_days = CAvideos$trending_date-CAvideos$publish_time

# Now find the average trending time of videos of each channel
newCAvideos = CAvideos %>%
			group_by(channel_title, title, thumbnail_link) %>% 
			summarise(trend_days = max(trend_days))

newCAvideos = newCAvideos %>% 
				group_by(channel_title) %>%  
				summarise(count = n(), trend_days = round(mean(trend_days),digits = 2))

png("Trending_channels_vs_trend_time.png")

# Now we plot the graph of average trending time of the top 10 trending channels 
ggplot(newCAvideos %>% 
	arrange(-count) %>%
	top_n(10, wt = count),
	aes(reorder(channel_title, -trend_days), trend_days, fill = channel_title)) +
	geom_bar(stat = "identity") +
	theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
	labs(caption="Kartheek", title=" Top trending channels in Canada") + 
	xlab(NULL)+ylab("Average trending time (in days)")

dev.off()


#####4. Now we will visualize the frequency of videos trending x number of days

png("Trending_time_of_videos.png")

ggplot(CAvideos %>% filter(trend_days<20),aes(as.factor(trend_days),fill=as.factor(trend_days))) +
	geom_bar()+guides(fill="none") +
	labs(caption="Kartheek",title=" Time between published and trending") +
	xlab("Time (in days)")+ylab("Number of videos")

dev.off()
























