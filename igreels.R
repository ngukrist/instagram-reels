setwd("/Users/kris/Desktop/ig_reels")
library(tidyverse)
#import data
reel_data <- read.csv("reel_data.csv", stringsAsFactors = T)
#remove instagram users bc all aggregated
reel_data <- reel_data[reel_data$sender_name!="Instagram User",]

#count data by who sent reel and time
data <- reel_data %>% group_by(sender_name, time) %>%
  summarize(Counts = length(person), .groups='drop')

#top senders by aggregating by who sent
top_sender <- aggregate(data$Counts, list(data$sender_name), FUN=sum)
colnames(top_sender) <- c("sender", "count")
attach(top_sender)
top_sender <- top_sender[order(-count),]
top_sender$sender <- factor(top_sender$sender, levels = top_sender$sender)
row.names(top_sender) <- NULL

#plots total reels sent to me by aggregating over time
sent <- data[(!(grepl("kristina", data$sender_name))),]
sent <- aggregate(data$Counts, list(data$time), FUN=sum)
colnames(sent) <- c("time", "count")
sent$sender <- "not me"

#gets the top senders including me
top_20 <- top_sender[1:21,]
#bar plot top senders
ggplot(top_20, aes(x=sender, y=count))+
  geom_bar(stat="identity", aes(fill=sender))+
  theme_classic()+
  labs(title="top 20 reel senders after me")+
  geom_text(aes(label=count), vjust=-.5)+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
top_20t <- data[data$sender_name %in% top_20$sender,]
top_20t$sender_name <- factor(top_20t$sender_name, levels = top_20$sender)
#plot top 20 senders over time
ggplot(top_20t)+
  geom_point(aes(x=time, y=Counts, color=sender_name))+
  geom_line(aes(x=time, y=Counts, group=sender_name, color=sender_name))+
  labs(title="top 20 reel senders")+
  theme_classic()

#gets only reels sent from me
from_me <- reel_data[grepl("kristina", reel_data$sender_name),]
#group by sent to and time
from_me <- from_me %>% group_by(person, time) %>%
  summarize(Counts = length(sender_name), .groups='drop')
from_me$person <- substring(from_me$person, 1, 15)
isent <- aggregate(from_me$Counts, list(from_me$time), FUN=sum)
colnames(isent) <- c("time", "count")
isent$sender <- "me"

#combine isent and sent
sents <- rbind(isent, sent)
#plots total reels sent by me vs others
ggplot(sents)+
  geom_point(aes(x=time, y=count, color=sender))+
  geom_line(aes(x=time, y=count, group=sender, color=sender))+
  scale_color_manual(values=c("steelblue", "forestgreen"))+
  labs(title="total reels sent")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#top sent to by aggregating by person        
top_sent <- aggregate(from_me$Counts, 
                        by=list(from_me$person), FUN=sum)
colnames(top_sent) <- c("person", "count")
attach(top_sent)
top_sent <- top_sent[order(-count),]
top_sent$person <- factor(top_sent$person, levels = top_sent$person)
row.names(top_sent) <- NULL

#gets top 20 sent to
my_20 <- top_sent[1:20,]
#bar plot of top 20 sent to
ggplot(my_20, aes(x=person, y=count))+
  geom_bar(stat="identity", aes(fill=person))+
  theme_classic()+
  geom_text(aes(label=count), vjust=-.5)+
  labs(title="top 20 reel receivers")+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
my_20t <- from_me[from_me$person %in% my_20$person,]
my_20t$person <- factor(my_20t$person, levels = my_20$person)
#plots top 20 sent to over time
ggplot(my_20t)+
  geom_point(aes(x=time, y=Counts, color=person))+
  geom_line(aes(x=time, y=Counts, group=person, color=person))+
  labs(title="top 20 reel receivers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#groupby dm for me vs other
other <- reel_data
other$sender_name <- grepl("kristina", reel_data$sender_name)
core <- other %>% group_by(person, sender_name, time) %>%
  summarize(Counts = length(person), .groups='drop')

kris <- core[core$sender_name,]
kris$kristina <- kris$Counts
kris$other <- 0
other <- core[!(core$sender_name),]
other$kristina <- 0
other$other <- other$Counts

new <- merge(kris, other, by=cbind("person", "time"))
new$kris <- new$kristina.x + new$kristina.y
new$other <- new$other.x + new$other.y
new <- new[, c("person", "time", "kris", "other")]

#plot reels i sent vs get back
ggplot(new)+
  geom_point(aes(x=kris, y=other), color="grey30")+
  geom_smooth(aes(x=kris, y=other), se=F, method=lm,color="steelblue")+
  labs(title="my reel sent to received ratios (by dm and month)")+
  labs(subtitle="y = 2.3771x + 0.6252")
