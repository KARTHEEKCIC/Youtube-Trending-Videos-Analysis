source("~/Documents/YoutubeStats/libraries.R")
library(DT)
library(shiny)

# Reading the dataset 
CAvideos = read_csv(file = ca_loc)

# Retrieving the maximum likes of each video in the dataset
disliked_videos = CAvideos[,c("title","thumbnail_link","dislikes","views")] %>% 
				group_by(title,thumbnail_link) %>%
				summarise(percentage_dislikes = round(100*max(dislikes,na.rm = T)/max(views, na.rm = T),digits = 2))

disliked_videos

shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {
    output$tbl = DT::renderDT({ disliked_videos %>%
  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%  
		arrange(-percentage_dislikes) %>% 
 		top_n(10, wt = percentage_dislikes) %>%
  		select(image,title, percentage_dislikes) %>%
  		datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE)) 
  })
}
)
