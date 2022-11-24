library(knitr)
library(rsample)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(ggthemes)
library(recipes)
library(corrr)
library(parsnip)
library(workflows)
library(dplyr)
library(yardstick)
library(discrim)
library(klaR)
library(gridExtra)

setwd("~/Downloads/PSTAT231/Final_Project") # setting to final proj directory

load('data/eda_wta_df.RData')
load('data/clean_wta_df.RData')

# Basic Distribution 
ggplot(total_wta_df_eda, aes(x=player_age)) + 
  geom_histogram(bins = 20, colour = 4, fill = "white") + xlab("Player Age")+
  ggtitle("Histogram of Player Age") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(total_wta_df_eda, aes(x=player_ht))  + 
  geom_histogram(bins = 20, colour = 4, fill = "white") + xlab("Player Height")+
  ggtitle("Histogram of Player Heights") +  
  theme(plot.title = element_text(hjust = 0.5))

ggplot(total_wta_df_eda, aes(x=minutes))  + 
  geom_histogram(bins = 20, colour = 4, fill = "white") + xlab("Match Length")+
  ggtitle("Histogram of Match Lengths") +  
  theme(plot.title = element_text(hjust = 0.5))

ggplot(total_wta_df_eda, aes(x=minutes, fill= win))  + 
  geom_histogram(bins = 25,alpha = 0.5) +
  xlab("Match Length")+
  ggtitle("Histogram of Match Lengths by Match Outcome") +  
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(total_wta_df_eda, aes(x=minutes))  + 
  geom_histogram(bins = 20, colour = 4, fill = "white") + xlab("Match Length")+
  ggtitle("Histogram of Match Length by GrandSlam") +  
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(tourney_name~ .)

ggplot(total_wta_df_eda, aes(x = tourney_name, y = minutes, color = tourney_name)) +
  geom_violin(trim = FALSE) + xlab("Match Length (minutes)")+
  ggtitle("Histogram of Match Length by Grand Slam") +  
  theme(plot.title = element_text(hjust = 0.5))  +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="black")



# Does the match duration differ by win/losing
ggplot(data=total_wta_df_eda, mapping = aes(x = win, y = minutes)) + 
  geom_boxplot() +
  theme_bw()

# Does match duration differ by grandslam?
ggplot(data=total_wta_df_eda, mapping = aes(x = tourney_name, y = minutes)) + 
  geom_boxplot() +
  theme_bw()

# Which countries have a certain affinity for sufaces?
country_count<- as.data.frame(table(total_wta_df_eda$player_ioc))
newdata <- country_count[order(-country_count$Freq),]

mydat1 <- total_wta_df_eda %>% dplyr::select(player_ioc,win,surface)
mydat1_val <- as.data.frame(mydat1 %>% group_by(player_ioc, surface) %>% count(win)) %>% 
  dplyr::filter(win == 1)

country_win_clay<- mydat1_val  %>%dplyr::filter(surface == 'Clay')  %>% 
  dplyr::select(-c(surface,win)) 

country_win_grass<- mydat1_val  %>%dplyr::filter(surface == 'Grass')  %>% 
  dplyr::select(-c(surface,win)) 

country_win_hard <- mydat1_val  %>%dplyr::filter(surface == 'Hard')  %>% 
  dplyr::select(-c(surface,win)) 

clay_win_graph <- ggplot(data=country_win_clay, aes(x=player_ioc, y=n)) +
  geom_bar(stat="identity", fill = "coral2") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  xlab("Country") + ylab("Number of Wins") + ggtitle("Number of Wins on Clay by Country")

grass_win_graph <- ggplot(data=country_win_grass, aes(x=player_ioc, y=n)) +
  geom_bar(stat="identity", fill = "darkolivegreen3") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  xlab("Country") + ylab("Number of Wins") + ggtitle("Number of Wins on Grass by Country")

hard_win_graph <- ggplot(data=country_win_hard, aes(x=player_ioc, y=n)) +
  geom_bar(stat="identity", fill = "deepskyblue3") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  xlab("Country") + ylab("Number of Wins") + ggtitle("Number of Wins on Hard by Country")

clay_win_graph
grass_win_graph
hard_win_graph

# Top 10 country by surface (wins)
country_win_clay[order(desc(country_win_clay$n)),][0:10,]
country_win_grass[order(desc(country_win_grass$n)),][0:10,]
country_win_hard[order(desc(country_win_hard$n)),][0:10,]

# Numbers of players from each country
table(total_wta_df_eda$player_ioc)


# Height EDA questions 
height_data <-  as.data.frame(total_wta_df_eda %>% dplyr::select(player_ht, win) %>% 
  group_by(player_ht) %>% count(win)) %>% mutate(win = factor(win))


height_data_win<- height_data %>% filter(win==1)
height_data_loss <- height_data %>% filter(win==0)

height_data_win_plot <- ggplot(data=height_data_win, aes(x=player_ht, y=n)) +
  geom_bar(stat="identity", fill = "darksalmon") + 
  xlab("Player Height") + ylab("Number of Wins") + 
  ggtitle("Number of Wins by Player Height") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

height_data_loss_plot <- ggplot(data=height_data_loss, aes(x=player_ht, y=n)) +
  geom_bar(stat="identity", fill = "darksalmon") + 
  xlab("Player Height") + ylab("Number of Losses") + 
  ggtitle("Number of Loss by Player Height") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))

ggplot(height_data, aes(fill=win, y=n, x=player_ht)) + 
  geom_bar(position="stack", stat="identity") + xlab("Player Height") + 
  ylab("Number of Losses") + 
  ggtitle("Number of Match Outcomes by Player Height") + 
  theme(plot.title = element_text(hjust = 0.5))

#number of aces/height
ggplot(data=total_wta_df_eda[1:1759,], aes(x=player_ht, y=p_ace)) +
  geom_bar(stat="identity") + xlab("Player Height") + 
  ylab("Number of Aces") + 
  ggtitle("Number of Aces by Player Height") + 
  theme(plot.title = element_text(hjust = 0.5))


# Serving Questions 
#any relationship between aces and number of double faults? (high risk, high reward)
ggplot(total_wta_df_eda, aes(x=p_ace, y=p_df)) + geom_point()

first_serve_plot<- ggplot(total_wta_df_eda, aes(x=win, y=p_1stWon)) + 
  geom_boxplot(fill = "azure2") + xlab("Match Outcome") + 
  ylab("Number of First Serve Points Won") + 
  ggtitle("Distribution of First Serve Points Won by Match Outcome") + 
  theme(plot.title = element_text(hjust = 0.5))

second_serve_plot<- ggplot(total_wta_df_eda, aes(x=win, y=p_2ndWon)) + 
  geom_boxplot(fill = "azure2") + xlab("Match Outcome") + 
  ylab("Number of Second Serve Points Won") + 
  ggtitle("Distribution of Second Serve Points Won by Match Outcome") + 
  theme(plot.title = element_text(hjust = 0.5))

# Violin
first_serve_violin<- ggplot(total_wta_df_eda, aes(x=win, y=p_1stWon, fill = "azure2")) + 
  xlab("Match Outcome") + geom_violin(trim = FALSE)+ geom_boxplot(width = 0.07) +
  ylab("Number of First Serve Points Won") + 
  ggtitle("Distribution of First Serve Points Won by Match Outcome") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer()

second_serve_violin<- ggplot(total_wta_df_eda, aes(x=win, y=p_2ndWon, fill = "azure2")) + 
  xlab("Match Outcome") + geom_violin(trim = FALSE)+ geom_boxplot(width = 0.07) +
  ylab("Number of Second Serve Points Won") + 
  ggtitle("Distribution of Second Serve Points Won by Match Outcome") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer()
 
grid.arrange(first_serve_plot, second_serve_plot, ncol=2)

ggplot(total_wta_df_eda, aes(x=p_1stWon, y=p_2ndWon, color = win)) +
  geom_point(alpha = 0.2) 

# Player Ranking
ranking_df<- (as.data.frame(total_wta_df_eda %>% dplyr::select(player_rank, opp_rank, win)) %>% 
                             mutate(win = factor(win)))[1:1759,]
                           
for (i in 1:(nrow(ranking_df))){
  if(ranking_df$player_rank[i] < ranking_df$opp_rank[i]){
    ranking_df$player_rank_better[i] = 1
  }
  else{
    ranking_df$player_rank_better[i] = 0
  }
  if(ranking_df$player_rank_better[i] == 1 & ranking_df$win[i] == 1){
    ranking_df$did_better_player_win[i] = 1
  }
  else{
    ranking_df$did_better_player_win[i] = 0
  }
}
  
ggplot(ranking_df, aes(factor(did_better_player_win))) +
  geom_bar(colour = 4, fill = "white") + 
  scale_x_discrete(labels=c("Better Ranked Player Won", "Lower Ranked Player Won")) +
  xlab("") +
  ggtitle("Did the better ranked player win?") +
  theme(plot.title = element_text(hjust = 0.5))

# Does winning first set make you more likely to win match?
first_set_df<- (as.data.frame(total_wta_df_eda %>% dplyr::select(p_set1, o_set1, win)) %>% 
  mutate(win = factor(win)))[1:1759,]

for (i in 1:(nrow(first_set_df))){
  if(first_set_df$p_set1[i] > first_set_df$o_set1[i]){
    first_set_df$wonset1[i] = 1
  }
  else if (first_set_df$p_set1[i] < first_set_df$o_set1[i]){
    first_set_df$wonset1[i] = 0
  }
  
  if(first_set_df$wonset1[i] == 1 & first_set_df$win[i] == 1){
    first_set_df$win_overall_given_firstset[i] = 1
  }
  else{
    first_set_df$win_overall_given_firstset[i] = 0
  }
}

ggplot(first_set_df, aes(factor(win_overall_given_firstset))) +
  geom_bar(colour = 4, fill = "white")  +
  scale_x_discrete(labels=c("Lost First Set and Won Match", "Won First Set and  Won Match")) +
  xlab("") +
  ggtitle("Frequency of First Set Outcomes and Winning a Match") + 
  theme(plot.title = element_text(hjust = 0.5))

# Importance of serves
ggplot(total_wta_df_eda, aes(x=p_1stIn, y=p_1stWon, color = win)) +
  geom_jitter(alpha = 0.3) + xlab("Number of First Serves Made") + 
  ylab("Number of First Serves Points Won") + 
  ggtitle("Relationship Between First Serves Made and Won by Match Outcome")+
  geom_smooth(method=lm, se=TRUE, aes(group=win)) +
  theme(plot.title = element_text(hjust = 0.5))





    

