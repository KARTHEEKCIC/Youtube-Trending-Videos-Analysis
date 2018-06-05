source("~/Documents/YoutubeStats/libraries.R")
library(DT)
library(shiny)

# Reading the dataset 
CAvideos = read_csv(file = ca_loc)

# Retrieving the maximum likes of each video in the dataset
liked_videos = CAvideos[,c("title","thumbnail_link","likes")] %>% 
				group_by(title,thumbnail_link) %>%
				summarise(total_likes = max(likes,na.rm = T),digits = 2)

liked_videos

shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {
    output$tbl = DT::renderDT({ liked_videos %>%
  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%  
		arrange(-total_likes) %>% 
 		top_n(10, wt = total_likes) %>%
  		select(image,title, total_likes) %>%
  		datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE)) 
  })
}
)
