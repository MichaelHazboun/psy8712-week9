# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)
library(stringi)


# Data Import and Cleaning
links <- c("https://www.cnbc.com/business/","https://www.cnbc.com/investing/","https://www.cnbc.com/technology/","https://www.cnbc.com/politics/") #making a vector with the links to be used
sections <- c("Business","Investing","Tech","Politics") #making a vector for the sections

cnbc_tbl <- tibble(headline=as.character(), #setting up starting tbl for latter additions
                   length=as.numeric(),
                   source=as.character())
i=1 #setting 1 for the loop
for (i in 1:length(sections)){ #looping over the 4 desired sections
  thing <-read_html(links[i])  #importing all of the links
  thing2 <- html_elements(thing,".Card-title") #pulling the titles/headlines from each link
  thing_text <- html_text(thing2)
  thing_length <- stri_count_words(thing_text)
  thing_source <- sections[i]
  new_tbl <- tibble(headline=thing_text,
                    length=thing_length,
                    source=thing_source)
  cnbc_tbl <- rbind(cnbc_tbl,new_tbl)
}

# Visualization

cnbc_tbl %>% #made a boxplot of the data
  ggplot(aes(x=source,y=length))+
  geom_boxplot()

# Analysis
model1<- lm(length~source, data=cnbc_tbl) #made the model for the anova
anova1<- anova(model1) #ran the anova by plugging the model in and assigned the result to anova1

pvaluehere<- anova1$`Pr(>F)`[1] #pulled the p value (I used [1] because I didn't want to see the NA, purely for my visual satisfaction) and assigned it to pvalue here for further use

# Publication
# The results of an ANOVA comparing lengths across sources was F(3, 130) = 3.90, p = .01. This test was statistically significant.

clean_f_here<-formatC(anova1$`F value`[1], format="f", digits=2) #Properly formatting the F for the publication section
clean_p_here<-str_remove(formatC(pvaluehere, format="f", digits=2), "^0") #Properly formatting the p-value for the publication section
test_result <- ifelse(clean_p_here>0.05,"was not","was")

paste0("The results of an ANOVA comparing lengths across sources was F(",round(anova1$Df[1],digits=0),", ",round(anova1$Df[2],digits=0),") = ",clean_f_here,", p = ", clean_p_here,". This test ", test_result, " statistically significant.")
