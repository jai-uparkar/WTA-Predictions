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

setwd("~/Downloads/PSTAT231/Final_Project") # setting to final proj directory

wta_2022_df <-read.csv('data/wta_matches_2022.csv') # 2022 data
wta_2021_df <-read.csv('data/wta_matches_2021.csv') # 2021 data
wta_2020_df <-read.csv('data/wta_matches_2020.csv') # 2020 data
wta_2019_df <-read.csv('data/wta_matches_2019.csv') # 2019 data

wta_df<- rbind(wta_2022_df, wta_2021_df, wta_2020_df, wta_2019_df)

# only interested in predicting grandslams 
wta_df <- wta_df %>% filter( tourney_level == 'G')

# how many missing values for each column
wta_df  %>% summarise_all(~sum(is.na(.))) # doesn't provide accurate information

# missing values for ranking info columns that need to be factors
sum(is.na(as.numeric(wta_df$winner_seed)))/nrow(wta_df) # about 50 percent is missing
sum(is.na(as.numeric(wta_df$loser_seed)))/nrow(wta_df) # about 75 percent is missing
sum(is.na(as.numeric(wta_df$winner_entry)))/nrow(wta_df) # everything is missing
sum(is.na(as.numeric(wta_df$loser_entry)))/nrow(wta_df) # everything is missing
# Takeaway: remove entry columns


# correlation values for these values 
winner_seedc<- as.numeric(wta_df$winner_seed)
loser_seedc <- as.numeric(wta_df$loser_seed)
df_ranking_info<- data.frame(winner_seedc, loser_seedc, wta_df$winner_rank, 
                             wta_df$loser_rank)
M<- cor(na.omit(df_ranking_info))
corrplot(M, type = 'lower')
# Observations: rank and seed are inversely correlated
# Takeway: Remove seed columns

# R redundant information based on GrandSlam data:
# tourney_id, tourney_date, draw_size, tourney_level, best_of, match_num
wta_df <- wta_df  %>% dplyr::select(- c(tourney_id, tourney_date, 
                                        draw_size, tourney_level, 
                                        best_of, match_num))

# R redundant information ranking (based on previous exploration)
wta_df <- wta_df  %>% dplyr::select(- c(winner_seed, loser_seed, winner_entry, 
                                        loser_entry))

#correlation plot to determine extra info
wta_df_numeric<- wta_df %>% dplyr::select(where(is.numeric))
M = cor(na.omit(wta_df_numeric))
corrplot(M, type = 'lower')
# Observations Significant relationships between: id and age, rank and rank points 
# player id relflection physical information about them 

wta_df <- wta_df  %>% dplyr::select(- c(winner_rank_points, loser_rank_points))

# These are the columns we are left with 
colnames(wta_df)
length(table(wta_df$winner_id)) # 212 unique players


# Missingness for player information + Creating lookup table for height
total_ht_missing<- subset(wta_df, is.na(winner_ht) | is.na(loser_ht) )
winnner_ht_missing <-subset(wta_df, is.na(winner_ht))
loser_ht_missing<-subset(wta_df, is.na(loser_ht))
wplayer_ht_missing<- winnner_ht_missing$winner_name
lplayer_ht_missing<- loser_ht_missing$loser_name
total_height_missing <- unique(c(wplayer_ht_missing, lplayer_ht_missing))
heights_vect<- c(170, 173, 178, 182, 170, NA, 168, 170, 170, 175, 169, 182, 
                 165, 175, 164, 164, 164, 166, 178, 172, 175, 173, 170, 180,
                 170, 164, 180, 170, 174, 180, 160, NA, NA, NA, 183, 170, 
                 173, NA, 171, NA, 177, 179, 170, NA, 172, NA, NA, NA, 175, 173, 
                 NA, NA, NA, 157, 172, 173, 157, NA, 170, 165, NA, 183, NA)
height_lookup<- data.frame(name = total_height_missing, height = heights_vect)
save(height_lookup, file = "data/lookup_height.RData")
val<- which( is.na(wta_df$winner_ht) | is.na(wta_df$loser_ht))
for (i in val){
  if (is.na(wta_df[i, 'winner_ht'])){
    winner_name<- wta_df[i, 'winner_name']
    wta_df[i, 'winner_ht'] = height_lookup[height_lookup$name == winner_name, ]$height
  }
  
  if (is.na(wta_df[i, 'loser_ht'])){
    winner_name<- wta_df[i, 'loser_name']
    wta_df[i, 'loser_ht'] = height_lookup[height_lookup$name == winner_name, ]$height
  }
}

wta_df$winner_ht[is.na(wta_df$winner_ht)] <- round(mean(wta_df$winner_ht,na.rm = TRUE))
wta_df$loser_ht[is.na(wta_df$loser_ht)] <- round(mean(wta_df$loser_ht,na.rm = TRUE))

# rows with missingness have all the same match stats missing and are all from the same match
wta_df<- wta_df %>% drop_na(minutes, w_ace, winner_rank, loser_rank)

# dependent on ranking and statistics but independent of player
wta_df <- wta_df  %>% dplyr::select(- c(winner_id, winner_name, loser_id, loser_name))

# how many NA values are remaining
colSums(is.na(wta_df))

#1760 rowa and 33 columns
dim(wta_df)

# NOW CLEAN
unique(wta_df$tourney_name)

# create 2 versions of dataset, one from the loser view, one from winner view (player vs opponent)
# by default we have the winner 
colnames(wta_df)<- str_replace(colnames(wta_df), "winner", "player")
colnames(wta_df)<- str_replace(colnames(wta_df), "loser", "opp")
colnames(wta_df)<- str_replace(colnames(wta_df), "w_", "p_")
colnames(wta_df)<- str_replace(colnames(wta_df), "l_", "o_")

# splitting up the set score
for (i in 1:nrow(wta_df)){
  num_sets = str_count(wta_df$score[i], "-")
  strscore<- unlist(str_split(wta_df$score[i], " "))
  strscore<- unlist(str_split(strscore, "-"))
  new_strscore<- substr(strscore,1,1)[0:2]
  wta_df$p_set1[i]<- new_strscore[1]
  wta_df$o_set1[i]<- new_strscore[2]
  wta_df$total_sets[i]<- num_sets
  wta_df$win[i]<- 1
}

# remove any match that was not completed
x<- as.numeric(wta_df$p_set1)
removeRow_set<- which(is.na(x))
wta_df<- wta_df[-removeRow_set,]

winner_df<- wta_df # dataframe from the winner POV

loser_df<- wta_df # dataframe from the loser POV 
# swapp the opponent and the player 

colnames(loser_df)<- c("tourney_name", "surface","opp_hand", "opp_ht","opp_ioc", 
                       "opp_age", "player_hand" , "player_ht" ,  "player_ioc",  "player_age", "score",  
                       "round",  "minutes", "o_ace" , "o_df" ,"o_svpt", "o_1stIn" , "o_1stWon", "o_2ndWon",
                       "o_SvGms" , "o_bpSaved", "o_bpFaced", "p_ace", "p_df", "p_svpt", "p_1stIn", 
                       "p_1stWon", "p_2ndWon", "p_SvGms" , "p_bpSaved","p_bpFaced","opp_rank", "player_rank", "o_set1","p_set1","total_sets", "win")
loser_df$win<- 0
loser_df2 <- loser_df[, match(colnames(loser_df), colnames(winner_df))] # now columns are in same order

total_wta_df<- rbind(winner_df, loser_df2)

total_wta_df <-total_wta_df  %>% dplyr::select(- c(score))

total_wta_df <- total_wta_df %>% mutate_at(c('p_set1', 'o_set1'), as.numeric)

dummy_2L<- total_wta_df %>% mutate(opp_ioc = factor(opp_ioc), 
                                   player_ioc = factor(player_ioc), 
                                   player_age = base::round(player_age),
                                   opp_age = base::round(opp_age)) %>%
  mutate(round = recode(round, "R128" = 1, "R64" = 2, "R32" = 3, "R16" = 4, "QF" = 5, "SF" = 6, "F" = 7), 
         surface = recode(surface, "Hard" = 1, "Clay" = 2, "Grass" = 3 ))


testing_df <- dummy_2L %>% dplyr::select(where(is.numeric))
M = cor(testing_df)
corr_with_win<- as.data.frame(sort(M['win',], decreasing = T))
View(corr_with_win)
corrplot(M,type = 'lower')
# Takeaway: surface, round, minutes, and total sets have 0 correlation with win, so we can remove them 

total_wta_df_clean <- total_wta_df %>% dplyr::select(- c(surface, round, minutes, total_sets)) %>% 
  mutate(opp_ioc = factor(opp_ioc), player_ioc = factor(player_ioc), win = factor(win),
         player_age = base::round(player_age), opp_age = base::round(opp_age), tourney_name = factor(tourney_name),
         player_hand = factor(player_hand), opp_hand = factor(opp_hand)) 

total_wta_df_eda <- total_wta_df %>% 
  mutate(opp_ioc = factor(opp_ioc), player_ioc = factor(player_ioc), win = factor(win),
         player_age = base::round(player_age), opp_age = base::round(opp_age), tourney_name = factor(tourney_name),
         player_hand = factor(player_hand), opp_hand = factor(opp_hand))

save(total_wta_df_eda, file = 'data/eda_wta_df.RData')
save(total_wta_df_clean, file = 'data/clean_wta_df.RData')



