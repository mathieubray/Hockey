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

write.csv(transcripts,"CoachTranscripts/2015-2016_SCFinal_Transcripts.csv",row.names=F)

###########################################################################################################

transcript <- read.csv("CoachTranscripts/2015-2016_SCFinal_Transcripts.csv",header=T,stringsAsFactors=F)

# Focus on interviews with Mike Sullivan or Peter DeBoer, remove prompts from text

label.function <- function(Date,AfterGame){
  return(ifelse(AfterGame,paste(Date,"Post-Game"),Date))
}

coaches.text <- transcript %>% 
  filter(Subject %in% c("Mike Sullivan","Peter DeBoer")) %>%
  mutate(Text = gsub(Text,pattern="COACH SULLIVAN: ",replacement="")) %>%
  mutate(Text = gsub(Text,pattern="COACH DeBOER: ",replacement="")) %>%
  rowwise() %>%
  mutate(Label = label.function(Date,AfterGame))
  

# Split text into words, filter out 'stop words'
coaches.words <- coaches.text %>%
  unnest_tokens(Word,Text) %>%
  filter(! Word %in% stop_words$word)

# Set team colors
team.colors <- c("#c5b358","#007889") # Pittsburgh Gold and San Jose Teal respectively

## Most Common Words

# Count individual occurrences of each word
coaches.word.count <- coaches.words %>%
  count(Word,Subject) %>%
  ungroup() %>%
  group_by(Subject) %>%
  top_n(20) %>%
  rename(Occurrences = n)
  
# Plots of 20 most common words by coach
ggplot(data=coaches.word.count,aes(x=Word,y=Occurrences,fill=Subject)) +
  facet_wrap(~Subject, scales="free_y") +
  geom_bar(stat="identity",show.legend=F) +
  scale_fill_manual(values=team.colors) +
  xlab("Word") +
  coord_flip() +
  theme_bw(15)

# Word clouds for each coach
coaches.words %>%
  filter(Subject == "Mike Sullivan") %>%
  count(Word) %>%
  with(wordcloud(Word,n,max.words=100,colors=team.colors[1]))

coaches.words %>%
  filter(Subject == "Peter DeBoer") %>%
  count(Word) %>%
  with(wordcloud(Word,n,max.words=100,colors=team.colors[2]))


## Most Different Words

coach.ratios <- coaches.words %>%
  count(Word,Subject) %>%
  rename(Occurrences = n) %>%
  filter(sum(Occurrences) > 5) %>% # Focus on words appearing at least 5 times in the text
  spread(Subject,Occurrences,fill=0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -Word) %>% # Fraction of word usage by each coach
  mutate(LogRatio = log2(`Mike Sullivan`/`Peter DeBoer`)) %>%
  arrange(LogRatio) %>%
  group_by(LogRatio > 0) %>%
  mutate(Team = ifelse(LogRatio > 0,"Pittsburgh","San Jose")) %>%
  top_n(10, abs(LogRatio)) %>% # Get top 10 words in each direction
  ungroup()

# Plot of top 10 words most often used by each coach relative to the other
ggplot(data=coach.ratios,aes(x=reorder(Word,LogRatio),y=LogRatio,fill=Team)) +
  geom_bar(stat="identity",show.legend=F) +
  scale_fill_manual(values=team.colors) +
  xlab("Word") +
  ylab("Log Ratio (Sullivan vs. DeBoer)") +
  coord_flip() +
  theme_bw(15)


# TF-IDF

# Number of occurences of each word by interview
all.coaches.words <- coaches.words %>%
  count(Subject,Word,Label,sort=T) %>%
  ungroup()

# Total words per interview
total.words <- coaches.words %>%
  count(Subject, Label) %>%
  ungroup()

# Get top words by TF-IDF for each interview
top.words.tf <- left_join(all.coaches.words,total.words,by=c("Subject","Label")) %>%
  rename(Occurrences=n.x,Total=n.y) %>%
  mutate(Document = paste(Subject,Label)) %>% # Merge subject and label for use in bind_tf_idf function
  bind_tf_idf(Word,Document,Occurrences) %>%
  arrange(desc(tf_idf)) %>%
  mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
  group_by(Document,Label) %>% 
  top_n(1) %>% 
  ungroup()

ggplot(top.words.tf, aes(Word, tf_idf, fill = Subject)) +
  geom_bar(stat = "identity", show.legend = F) +
  facet_wrap(~Subject, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = team.colors) +
  ylab("TF-IDF") +
  coord_flip() + 
  theme_bw(15)


## Sentiment Categories

# Prepare sentiment dataset
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment) %>%
  rename(Word = word, Sentiment = sentiment)

head(nrc)

# Count the total number of words used by each coach each interview
coaches.words.by.interview <- coaches.words %>%
  group_by(Subject,Label) %>%
  mutate(TotalWords = n()) %>%
  ungroup() %>%
  distinct(Subject, Label, TotalWords)

coaches.word.by.sentiment <- coaches.words %>%
  inner_join(nrc, by = "Word") %>%
  count(Sentiment,Label,Subject) %>% # Count occurrences of each word by each coach in each interview
  rename(Occurrences = n) %>%
  inner_join(coaches.words.by.interview, by = c("Label","Subject")) %>% # Merge total words
  mutate(Ratio = Occurrences/TotalWords)

# Plot the sentiment trajectories over time
ggplot(data=coaches.word.by.sentiment,aes(x=Label,y=Ratio,color=Subject, group=Subject)) +
  facet_wrap(~Sentiment, nrow=5) +
  geom_line(show.legend=F) +
  geom_point(show.legend=F) +
  scale_color_manual(values = team.colors) +
  xlab("Interview") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


## Positive vs. Negative

# Get top 10 positive and negative words used by each coach and plot
sentiment.words <- coaches.words %>%
  inner_join(get_sentiments("bing"), by=c("Word"="word")) %>%
  rename(Sentiment = sentiment) %>%
  count(Word,Subject,Sentiment,sort=T) %>%
  ungroup() %>%
  group_by(Subject,Sentiment) %>%
  top_n(10) %>%
  rename(Occurrences = n)  %>%
  arrange(Subject,Sentiment,Word)

ggplot(data=sentiment.words,aes(x=reorder(factor(Word),Occurrences),y=Occurrences,fill=Subject)) +
  geom_bar(stat="identity",show.legend=F) +
  facet_wrap(~Sentiment + Subject, scales="free_y") +
  scale_fill_manual(values = team.colors) +
  xlab("Word") +
  coord_flip() +
  theme_bw(15)

# Find the difference between number of positive and negative words 
# from each interview for each coach and plot
positive.negative <- coaches.words %>%
  inner_join(get_sentiments("bing"), by=c("Word"="word")) %>%
  rename(Sentiment = sentiment) %>%
  count(Subject, Label, Sentiment) %>%
  spread(Sentiment, n, fill = 0) %>%
  mutate(Score = positive - negative) %>%
  select(-positive,-negative)

ggplot(positive.negative, aes(x=Label, y=Score, fill = Subject)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~Subject) +
  scale_fill_manual(values = team.colors) +
  xlab("Interview") +
  theme_bw(15) + 
  theme(axis.text.x = element_text(angle=90))


## Bigrams

# Collect bigrams from text
bigrams <- coaches.text %>%
  unnest_tokens(Bigram, Text, token="ngrams", n=2) %>%
  separate(Bigram,c("Word1","Word2"),sep=" ")

# Count bigrams, show those that appear at least 5 times
bigram.counts <- bigrams %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word) %>%
  count(Word1,Word2,sort=T) %>%
  rename(Occurrences = n) %>%
  filter(Occurrences >= 5)

bigram.counts

negation.words <- c("not", "no", "never", "without")

# Find words preceded by negation words
negated.words <- bigrams %>%
  filter(Word1 %in% negation.words) %>% # First word of bigram is a negation word
  inner_join(get_sentiments("bing"), by = c(Word2 = "word")) %>% # Second word appears in sentiment data set
  count(Word1, Word2, sentiment, sort = TRUE) %>%
  ungroup()%>%
  rename(Sentiment = sentiment, Occurrences = n)

negated.words



# Bigram Network

bigram.graph <- bigram.counts %>%
 filter(n > 4) %>%
 graph_from_data_frame()

edges <- igraph::as_data_frame(bigram.graph, what="edges") %>%
  select(-n)
vertices <- igraph::as_data_frame(bigram.graph, what="vertices") %>%
  mutate(id=name,label=name)

visNetwork(vertices,edges,main="Generated Network") %>%
 visNodes(shadow = T) %>%
 visEdges(arrows="to")


