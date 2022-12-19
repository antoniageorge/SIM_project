library(tidyverse)
library(glmnet)
library(gridExtra)
setwd("~/Desktop")
source(file.path("./SIM_project/helper_functions/routines_seminar1.R"))
library("readxl")
library(lme4)
library(arm)

# Load datasets
f22 <- read_csv("./SIM_project/data/players_22.csv")
f21 <- read_csv("./SIM_project/data/players_21.csv")
f20 <- read_csv("./SIM_project/data/players_20.csv")
f19 <- read_csv("./SIM_project/data/players_19.csv")
f18 <- read_csv("./SIM_project/data/players_18.csv")
f17 <- read_csv("./SIM_project/data/players_17.csv")
f16 <- read_csv("./SIM_project/data/players_16.csv")
f15 <- read_csv("./SIM_project/data/players_15.csv")

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
table22 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2021-2022")
table21 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2020-2021")
table20 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2019-2020")
table19 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2018-2019")
table18 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2017-2018")
table17 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2016-2017")
table16 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2015-2016")
table15 <- read_excel("./SIM_project/data/LaLiga_results.xlsx", sheet = "2014-2015")

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
                    "club_position",
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
f22_gp <- f22_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f21_gp <- f21_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f20_gp <- f20_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f19_gp <- f19_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f18_gp <- f18_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f17_gp <- f17_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f16_gp <- f16_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
f15_gp <- f15_sp %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))

# Join dataframes
f22_gp <- left_join(f22_gp, table22, by = c("club_name"="Equipo"))
f21_gp <- left_join(f21_gp, table21, by = c("club_name"="Equipo"))
f20_gp <- left_join(f20_gp, table20, by = c("club_name"="Equipo"))
f19_gp <- left_join(f19_gp, table19, by = c("club_name"="Equipo"))
f18_gp <- left_join(f18_gp, table18, by = c("club_name"="Equipo"))
f17_gp <- left_join(f17_gp, table17, by = c("club_name"="Equipo"))
f16_gp <- left_join(f16_gp, table16, by = c("club_name"="Equipo"))
f15_gp <- left_join(f15_gp, table15, by = c("club_name"="Equipo"))

# Check for NAs in dataframe.
for(i in 1:length(cols_to_choose)){
  print(sum(is.na(f22_gp[i])))
}


# Add a multiplier on points for second division teams going to first division.
# second_div_mult <- 0.5


new_df <- bind_rows(f21_gp, f20_gp, f19_gp, f18_gp, f17_gp, f16_gp, f15_gp)

# Remove some columns
colnames(new_df)
cols_to_drop <- c( "Pos.", "PJ", "G", "E", "P", "GF", "GC", "Dif.", "Des", "Asc")
new_df <- new_df[, !colnames(new_df) %in% cols_to_drop]
f22_gp <- f22_gp[, !colnames(f22_gp) %in% cols_to_drop]

# train
y <- new_df$Pts.
x <- new_df %>% dplyr::select(-c("Pts.", "club_name","league_level")) %>% as.matrix() %>% scale()

#test #### NEEDS TO BE SAME SCALE!!
y_te <- f22_gp[c("club_name","Pts.")]
x_te <- dplyr::select(f22_gp, -c("Pts.", "club_name","league_level") ) %>% as.matrix() %>% scale()

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

# Test base model on 22 dataset
predictions <- predict(fit.lasso, newx = x_te)
y_te$pred <-predictions
y_te <- y_te %>% arrange(desc(Pts.))



# Test other model on 22 dataset
predictions <- predict(fit.lasso, newx = x_te)
y_te$pred <-predictions
y_te <- y_te %>% arrange(desc(Pts.))
# For dataset
# compute averages by player
# calculate month of birth for player
# modify goalkeeper stats to 0 for 

# For model
# different values of lambda


# -----------------------------------
# --- BAYESIAN HIERARCHICAL MODEL ---
# -----------------------------------

fp22_hm <- left_join(f22_sp, table22, by = c("club_name"="Equipo"))
fp21_hm <- left_join(f21_sp, table21, by = c("club_name"="Equipo"))
fp20_hm <- left_join(f20_sp, table20, by = c("club_name"="Equipo"))
fp19_hm <- left_join(f19_sp, table19, by = c("club_name"="Equipo"))
fp18_hm <- left_join(f18_sp, table18, by = c("club_name"="Equipo"))
fp17_hm <- left_join(f17_sp, table17, by = c("club_name"="Equipo"))
fp16_hm <- left_join(f16_sp, table16, by = c("club_name"="Equipo"))
fp15_hm <- left_join(f15_sp, table15, by = c("club_name"="Equipo"))


cols_to_drop <- c( "Pos.", "PJ", "G", "E", "P", "GF", "GC", "Dif.", "Des", "Asc", "league_level")

y_te <- fp22_hm[c("Pts.","club_name")]
hm_data <-bind_rows(fp21_hm, fp20_hm, fp19_hm, fp18_hm, fp17_hm, fp16_hm, fp15_hm)
hm_data<- hm_data[, !colnames(hm_data) %in% cols_to_drop] 
fp22_hm<- fp22_hm[, !colnames(fp22_hm) %in% cols_to_drop] 

# fp22_hm[] <- lapply(fp22_hm, function(x) if(is.numeric(x)){
#   scale(x, center=TRUE, scale=TRUE)
#   } else x)

# Check for NAs in dataframe.
for(i in 1:length(colnames(fp22_hm))){
  print(sum(is.na(fp22_hm[i])))
}
# fp22_hm$ran <- sample(-10:10, 633, rep = TRUE)
# fp22_hm$ranpts <- fp22_hm$Pts.+ fp22_hm$ran

# later add right/left foot as binary variable

hm_datasc <- hm_data %>% 
  dplyr::select(-c(preferred_foot, club_name, Pts., club_position)) %>% 
  scale() %>% 
  data.frame () %>% 
  mutate_if(is.character, as.numeric)
hm_datasc <- cbind(hm_datasc, 'club_name'= hm_data$club_name, 'Pts.'=hm_data$Pts., 'club_position'=hm_data$club_position) 

fp22_hm_sc <- fp22_hm %>% 
  dplyr::select(-c(preferred_foot, club_name, Pts., club_position)) %>% 
  scale() %>% 
  data.frame () %>% 
  mutate_if(is.character, as.numeric)
fp22_hm_sc <- cbind(fp22_hm_sc, 'club_name'= fp22_hm$club_name, 'Pts.'=fp22_hm$Pts., 'club_position'=fp22_hm$club_position) 


hm_datasc <- data.frame(hm_datasc)
fp22_hm_sc <- data.frame(fp22_hm_sc)

str(hm_datasc)


ML_ex <- lmer(Pts. ~age + international_reputation + (1|club_name:club_position) + (1|club_name), data=hm_datasc,  REML = FALSE) 
c<-predict(ML_ex, newdata=fp22_hm_sc)
ML_ex
d <- f22 %>% filter(league_name=='Spain Primera Division')
compare <- cbind(y_te, c, d$short_name)
compare <- compare %>% mutate("diff" = Pts.-c)
mean((compare$diff)^2)



# -----------------------------
# -----------  Value  ---------
# -----------------------------

hm_datasc["value_eur"] <- hm_data$value_eur
fp22_hm_sc["value_eur"] <- fp22_hm$value_eur

ML_ex_o <- lmer(value_eur ~  age + overall + international_reputation + wage_eur + (1|club_position), data=hm_datasc,  REML = FALSE)
v_pred <-predict(ML_ex_o, newdata=fp22_hm_sc)
ML_ex_o

compare_o <- cbind(y_te, v_pred, d[c("short_name", "overall", "value_eur")])
compare_o <- compare_o %>% mutate("diff" = round(value_eur-v_pred))
mean((compare_o$diff)^2)^0.5
# 9026972

# -----------------------------
# -----------  Wage  ----------
# -----------------------------

hm_datasc["wage_eur"] <- hm_data$wage_eur
fp22_hm_sc["wage_eur"] <- fp22_hm$wage_eur

ML_ex_o <- lmer(wage_eur ~  age + overall + international_reputation + (1|club_position) + (1|club_name), data=hm_datasc,  REML = FALSE) 
w_pred <-predict(ML_ex_o, newdata=fp22_hm_sc)
ML_ex_o

compare_w <- cbind(y_te, v_pred, d[c("short_name", "overall", "wage_eur")])
compare_w <- compare_w %>% mutate("diff" = round(wage_eur-w_pred))
mean((compare_w$diff)^2)^0.5
# club_pos: 29473.9
# club_name: 24591.14 
# both:  23680.58             age + overall + international_reputation + (1|club_position) + (1|club_name)









# predict(ML_ex, newx=fp22_hm_sc)
# dim(fp22_hm)
# 
# hm_data$pred <- predict(ML_ex, newx=hm_data)
# hm_data[c("pred", "Pts.")]
# cor(hm_data$pred, hm_data$Pts.)
# 
# 
# grouped <- hm_data %>% group_by(club_name) %>% summarise(across(where(is.numeric), mean))
# cor(grouped$pred, grouped$Pts.)

# TO DO
# add season column and group by season. 
# run on test data and see how well it works on new dataset
