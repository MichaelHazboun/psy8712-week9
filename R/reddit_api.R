# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)


# Data Import and Cleaning

test1<- find_thread_urls(subreddit="rstats", period = "month") #I was following instructions from the Reddit extractor github explanations. This is definitely not in the format that we wanted, but I split them up, because I tried this (since it came first in the explanation) then noticed that I should have done it in a pipe. However, I didn't want to re pull the data because I didn't want to be big (well actually quite short to American standards, but still) bad man and potentially get banned :)
test2<-get_thread_content(test1$url) # continuing to follow the instructions, pulled the content from the urls that I pulled in the previous line. 

rstats_tbl <- tibble( #putting it all into one tibble
  post=test2$threads$title,
  upvote=test2$threads$score, # since none of the data that I pulled had any downvotes, and what you described was more of the posts' score not the total number of upvotes, I chose to use score, but functionally they're the same, in this case because there are no down votes in this sample (sometimes the worst insult is being ignored I guess)
  comments=test2$threads$comments
)

# Visualization

rstats_tbl %>% #made a scatter plot with a to visualize the data.
  ggplot(aes(x=upvote,y=comments))+
  geom_point()+
  geom_smooth(se=F)+ #added a line to somewhat help show the relationship between upvotes and comments. 
  coord_cartesian(xlim=c(0,50),ylim=c(0,50)) + # I chose not to show the whole graph because two large posts shrunk down the rest of the plot by a lot. So I felt this was a better representation of the majority of our data.
  labs(x= "Number of Upvotes", y= "Number of Comments", title = "Scatterplot of comments and upvotes")

# Analysis
wow_cor<- cor.test(rstats_tbl$upvote,rstats_tbl$comments) # finding the relationship between upvotes and comments
actual_cor<- wow_cor$estimate # assigning the correlation to a variable
actual_p<- wow_cor$p.value # assigning the p to a variable
actual_cor; actual_p #printing out the correlation and p value

# Publication
#"The correlation between upvotes and comments was r(121) =.60, p = .00. This test was statistically significant."
clean_cor<-str_remove(formatC(actual_cor, format="f", digits=2), "^0") #Properly formatting the correlation for the publication section
clean_p<-str_remove(formatC(actual_p, format="f", digits=2), "^0") #Properly formatting the p-value for the publication section
test_result <- ifelse(clean_p>=0.05,"was not", "was") # Creating a changing assignment of "was" or "was not" for the publication section


paste0("The correlation between upvotes and comments was r(",wow_cor$parameter,") =",clean_cor,", p = ",clean_p,". This test ",test_result," statistically significant.") #Making a dynamically changing sentence for as instructed!
