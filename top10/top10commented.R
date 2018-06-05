source("~/Documents/YoutubeStats/libraries.R")
library(DT)
library(shiny)

# Reading the dataset 
CAvideos = read_csv(file = ca_loc)

# Retrieving the maximum likes of each video in the dataset
commented_videos = CAvideos[,c("title","thumbnail_link","comment_count")] %>% 
				group_by(title,thumbnail_link) %>%
				summarise(total_comments = max(comment_count,na.rm = T),digits = 2)

commented_videos

shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {
    output$tbl = DT::renderDT({ commented_videos %>%
  mutate(image = paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%  
		arrange(-total_comments) %>% 
 		top_n(10, wt = total_comments) %>%
  		select(image,title, total_comments) %>%
  		datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE)) 
  }, escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
}
)
