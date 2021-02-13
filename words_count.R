### AUTHOR: AHz
### LAST EDIT: 2021-02-13
### WRITTEN IN: R version 3.5.1
### Purpose: Count unique words in a body of text

################################################################################
#  0. SET UP  ####
################################################################################

#install these packages if you don't already have them!
#install.packages("tidyverse")
#install.packages("textreadr")
#install.packages("textclean")
#install.packages("textshape")
library(tidyverse)
library(textreadr)
library(textclean)
library(textshape)

#get and set working directory
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

################################################################################
#  1. LOAD TEXT  ####
################################################################################

#there are a few ways you can add text: 

# 1. Manually paste your text below -- your text goes in between the quotes in c("").

text <- c("paste your really really good (or repetative) text here.
          no need to format or remove anything or change anything. 
          
          just copy and paste.
          
          hyphenated-words will be split into hyphenated and words. 
          
          it'll also understand contractions like I'll is actuall I will. 
          
          (in parentheses) will be separated and parentheses dropped,
          
          as will commas.")


# 2. download your text as an .rtf file and place it in the same location as 
# wherever your R script lives. You can skip over the lines at the 
# beginning by changing the number after skip = . This is going to nest your 
# paragraphs into a list format, so you can extract paragraph 1 by running 
# text[1] in your console. It is best if you use this approach if you want to 
# do a word count by paragraph. 
text <- textreadr::read_rtf("myfile.rtf", skip = 0)


#both options will work with the code in section 2, but only the read_rtf option
#will work with the paragraph by paragraph breakdown. 


################################################################################
#  2. GET WORD COUNT FOR WHOLE DOC -- PASTED TEXT ####
################################################################################

#how many unique words do you have? 
length(unique(unlist(textshape::split_word(text))))

# get a count of how many times you've used each unique word
unique_count_total <- split_word(replace_contraction(text)) %>% 
  mtabulate() %>% 
  pivot_longer(names_to = "word", values_to = "count", everything()) %>% 
  group_by(word) %>% 
  summarize(total_count = sum(count)) %>% 
  arrange(desc(total_count))

#get a count of how many times you've used each unique word by paragraph
unique_count_paragraphs <- split_word(text) %>% mtabulate() %>% 
  rownames_to_column(var = "paragraph") %>% 
  pivot_longer(names_to = "word", values_to = "count", !all_of("paragraph")) %>% 
  pivot_wider(names_from = paragraph, values_from = count) %>%
  full_join(unique_count_total) %>% 
  arrange(desc(total_count))
  


