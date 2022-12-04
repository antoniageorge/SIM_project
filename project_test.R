library(tidyverse)
library(glmnet)
library(gridExtra)
setwd("~/Desktop")
source(file.path("./SIM_project/helper_functions/routines_seminar1.R"))
library("readxl")

# Load datasets
f22 <- read_csv("./SIM_project/players_22.csv")
f21 <- read_csv("./SIM_project/players_21.csv")
f20 <- read_csv("./SIM_project/players_20.csv")
f19 <- read_csv("./SIM_project/players_19.csv")
f18 <- read_csv("./SIM_project/players_18.csv")
f17 <- read_csv("./SIM_project/players_17.csv")
f16 <- read_csv("./SIM_project/players_16.csv")
f15 <- read_csv("./SIM_project/players_15.csv")

# Remove some unnecessary columns.
f22 <- f22 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

f21 <- f21 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

f20 <- f20 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

f19 <- f19 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

f18 <- f18 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

f17 <- f17 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

f16 <- f16 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

f15 <- f15 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))

# check they all have the same length
length(colnames(f22))
length(colnames(f21))
length(colnames(f20))
length(colnames(f19))
length(colnames(f18))
length(colnames(f17))
length(colnames(f16))
length(colnames(f15))


### TIMELINE ###
# Season Starts     (Fall 2020)
# Season Ends         (Spring 2021)
# Game (FIFA 2022) is made and players are rated (Summer 2021) 
# New Teams (Summer 2021) (These come from the 2022 edition of the game because they are from the 2021-2022 season.)
# Predictions are Made for next season (Summer 2021)
# New Season (2021 -> 2022)

# So, we are going to use the FIFA 2022 ratings to predict the 2021-2022 season with the FIFA 2022 rosters. 
# The 2021-2022 points data goes with FIFA 2022, since that is what we are trying to predict.

# Import REAL end of season info (total number of points, wins, losses, etc. for each team)
table22 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2021-2022")
table21 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2020-2021")
table20 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2019-2020")
table19 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2018-2019")
table18 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2017-2018")
table17 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2016-2017")
table16 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2015-2016")
table15 <- read_excel("./Fifa22/LaLiga_results.xlsx", sheet = "2014-2015")

f22
colnames(f22)

f22_sp <- f22 %>% filter(league_name=='Spain Primera Division')
f21_sp <- f21 %>% filter(league_name=='Spain Primera Division')
f20_sp <- f20 %>% filter(league_name=='Spain Primera Division')
f19_sp <- f19 %>% filter(league_name=='Spain Primera Division')
f18_sp <- f18 %>% filter(league_name=='Spain Primera Division')
f17_sp <- f17 %>% filter(league_name=='Spain Primera Division')
f16_sp <- f16 %>% filter(league_name=='Spain Primera Division')
f15_sp <- f15 %>% filter(league_name=='Spain Primera Division')

# Pick only the columns that we want.
cols_to_choose <- c("overall", "potential", "value_eur", "wage_eur", "age", #"dob", 
                    "height_cm", "weight_kg",  "club_name", #"club_team_id",
                    "league_level", "preferred_foot", "weak_foot", "skill_moves", "international_reputation", #"work_rate", #"body_type"                  
                    "release_clause_eur", #"player_tags", #"player_traits", 
                    "pace", 
                    "shooting", "passing", "dribbling", "defending",                  
                    "physic", "attacking_crossing", "attacking_finishing", "attacking_heading_accuracy", 
                    "attacking_short_passing", "attacking_volleys", "skill_dribbling", "skill_curve",                
                    "skill_fk_accuracy", "skill_long_passing", "skill_ball_control", "movement_acceleration",      
                    "movement_sprint_speed", "movement_agility", "movement_reactions", "movement_balance",           
                    "power_shot_power", "power_jumping", "power_stamina", "power_strength",             
                    "power_long_shots", "mentality_aggression", "mentality_interceptions", "mentality_positioning",      
                    "mentality_vision", "mentality_penalties", "mentality_composure", "defending_marking_awareness",
                    "defending_standing_tackle", "defending_sliding_tackle", "goalkeeping_diving", "goalkeeping_handling",
                    "goalkeeping_kicking", "goalkeeping_positioning", "goalkeeping_reflexes", "goalkeeping_speed" 
                    #"ls", "st", "rs","lw", 
                    #"lf", "cf", "rf", "rw",
                    #"lam", "cam", "ram", "lm", 
                    #"lcm", "cm", "rcm", "rm",                       
                    #"lwb", "ldm", "cdm", "rdm",                        
                    #"rwb", "lb", "lcb", "cb",                        
                    #"rcb", "rb", "gk" 
                    ) 

# Make the column selection.
f22_sp <- f22_sp[cols_to_choose]
f21_sp <- f21_sp[cols_to_choose]
f20_sp <- f20_sp[cols_to_choose]
f19_sp <- f19_sp[cols_to_choose]
f18_sp <- f18_sp[cols_to_choose]
f17_sp <- f17_sp[cols_to_choose]
f16_sp <- f16_sp[cols_to_choose]
f15_sp <- f15_sp[cols_to_choose]


# Replace NAs by 0
f22_sp[is.na(f22_sp)] <- 0
f21_sp[is.na(f21_sp)] <- 0
f20_sp[is.na(f20_sp)] <- 0
f19_sp[is.na(f19_sp)] <- 0
f18_sp[is.na(f18_sp)] <- 0
f17_sp[is.na(f17_sp)] <- 0
f16_sp[is.na(f16_sp)] <- 0
f15_sp[is.na(f15_sp)] <- 0

# Group dataframes by team.
f22_sp <- f22_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f21_sp <- f21_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f20_sp <- f20_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f19_sp <- f19_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f18_sp <- f18_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f17_sp <- f17_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f16_sp <- f16_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f15_sp <- f15_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))

# Join dataframes
f22_sp <- left_join(f22_sp, table22, by = c("club_name"="Equipo"))
f21_sp <- left_join(f21_sp, table21, by = c("club_name"="Equipo"))
f20_sp <- left_join(f20_sp, table20, by = c("club_name"="Equipo"))
f19_sp <- left_join(f19_sp, table19, by = c("club_name"="Equipo"))
f18_sp <- left_join(f18_sp, table18, by = c("club_name"="Equipo"))
f17_sp <- left_join(f17_sp, table17, by = c("club_name"="Equipo"))
f16_sp <- left_join(f16_sp, table16, by = c("club_name"="Equipo"))
f15_sp <- left_join(f15_sp, table15, by = c("club_name"="Equipo"))

# Check for NAs in dataframe.
for(i in 1:length(cols_to_choose)){
  print(sum(is.na(f22_sp[i])))
}


# Add a multiplier on points for second division teams going to first division.
# second_div_mult <- 0.5


new_df <- bind_rows(f21_sp, f20_sp, f19_sp, f18_sp, f17_sp, f16_sp, f15_sp)

# Remove some columns
colnames(new_df)
cols_to_drop <- c( "Pos.", "PJ", "G", "E", "P", "GF", "GC", "Dif.", "Des", "Asc")
new_df <- new_df[, !colnames(new_df) %in% cols_to_drop]
f22_sp <- f22_sp[, !colnames(f22_sp) %in% cols_to_drop]

# train
y <- new_df$Pts.
x <- select(new_df, -c("Pts.", "club_name") ) %>% as.matrix() %>% scale()

#test #### NEEDS TO BE SAME SCALE!!
y_te <- f22_sp[c("club_name","Pts.")]
x_te <- select(f22_sp, -c("Pts.", "club_name") ) %>% as.matrix() %>% scale()

## LASSO
fit.lasso= cv.glmnet(x=x, y=y, family = "gaussian", nfolds = 5, alpha=1, type.measure = "mse")

# #auc
lasso.err <- assess.glmnet(fit.lasso, newx=x, newy=y, family='gaussian')
lasso.err 

plot(fit.lasso$glmnet.fit, xvar='lambda')
plot(fit.lasso)

# try model w min value of lambda
b.lasso= as.vector(coef(fit.lasso, s='lambda.min'))
betas = data.frame("word" = c("intercept", colnames(x)), "beta_hat"= round(b.lasso, 5))
betas %>% arrange(desc(abs(beta_hat)))

# not as good but just to try something else
b.lasso= as.vector(coef(fit.lasso, s=0.1))
betas = data.frame("word" = c("intercept", colnames(x)), "beta_hat"= round(b.lasso, 5))
betas %>% arrange(desc(abs(beta_hat)))

# Test model on 22 dataset
predictions <- predict(fit.lasso, newx = x_te)
y_te$pred <-predictions
y_te <- y_te %>% arrange(desc(Pts.))
# For dataset
# compute averages by player
# calculate month of birth for player
# modify goalkeeper stats to 0 for 

# For model
# different values of lambda

