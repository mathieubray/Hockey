library(rvest)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tm)
library(lubridate)
library(tidytext)
library(ggplot2)
library(wordcloud)

# Almost certainly doesn't work anymore...

get.transcripts <- function(url){
  
  # Extract links
  urls <- read_html(url) %>% 
    html_nodes("a") %>%
    html_attr("href")
  
  # Relevant links have the 'show_interview.php?' tag
  relevant.urls <- urls[grepl(urls,pattern="show_interview.php?",fixed=T)] 
  
  # Run text extraction function on each link
  transcripts <- rbind.fill(map(relevant.urls,extract.interview.text))
  
  return(transcripts)
}


extract.interview.text <- function(url){
  
  # Extract text
  text <- read_html(url) %>%
    html_nodes("td") %>%
    html_text()
  
  # Relevant text 
  text.clean <- text[grepl(text,pattern="FastScripts Transcript by ASAP Sports",fixed=T)]
  
  # Split paragraphs, remove whitespace at the beginning and end of each
  text.clean.split <- str_trim(str_split(text.clean,pattern="\n")[[2]])
  
  raw.text <- gsub(text.clean.split[text.clean.split != ""],pattern=" FastScripts Transcript by ASAP Sports",replacement="")
  
  # Extract date
  date <- mdy(raw.text[2])
  
  tokens <- removeNumbers(removePunctuation(unlist(strsplit(gsub('([[:upper:]])',' \\1',raw.text[3]),"[[:space:]]"))))
  token.interview.starts <- min(c(which(tokens=="Q"),which(tokens=="MODERATOR"),which(tokens=="THE"),length(tokens)+1))
  
  tokens <- tokens[1:(token.interview.starts-1)]
  
  common.terms <- c("","San","Jose","California","Pittsburgh","Pennsylvania","Game","Practice","Day",
                    "One","Two","Three","Four","Five","Six","\u0096")
  
  tokens <- tokens[!(tokens %in% common.terms)]
  
  subject <- paste(tokens,collapse=" ")
  
  interview.starts <- min(c(which(str_detect(raw.text,"Q.")),which(str_detect(raw.text,"MODERATOR"))))
  
  after.game <- interview.starts > 4
  
  interview.text <- raw.text[interview.starts:length(raw.text)]
  
  relevant.interview.text <- paste(interview.text[!str_detect(interview.text,"Q.") & !str_detect(interview.text,"MODERATOR")],collapse=" ")
  
  interview.data <- data.frame(Date=date,Subject=subject,AfterGame=after.game,Text=relevant.interview.text,stringsAsFactors=F)
  
  return(interview.data)
  
}

cup.final.url <- paste0("http://www.asapsports.com/show_events.php?category=5&year=2016&title=NHL+STANLEY+CUP+FINAL%3A+SHARKS+v+PENGUINS")

# Extract links
transcript.urls <- read_html(cup.final.url) %>%
  html_nodes("a") %>%
  html_attr("href")

# Relevant links have the 'show_event.php?' tag
relevant.transcript.urls <- transcript.urls[grepl(transcript.urls,pattern="show_event.php?",fixed=T)]

# Run function on links
transcripts <- rbind.fill(map(relevant.transcript.urls,get.transcripts)) %>%
  mutate(Subject = gsub(Subject,pattern="De Boer",replacement="DeBoer"))

write.csv(transcripts,"CoachTranscripts/data/CoachTranscripts.csv",row.names=F)