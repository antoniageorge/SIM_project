library(tidyverse)
library(glmnet)
library(gridExtra)
PATH <- '~/Desktop'
source(file.path(PATH,"./Fifa22/helper_functions/routines_seminar1.R"))


f22 <- read_csv("./Fifa22/players_22.csv")
f22 <- f22 %>% select(-c("player_face_url", "club_logo_url", "club_flag_url", 
                         "nation_logo_url", "nation_flag_url", "player_url",
                         "real_face"))
final_table <- read_csv("./Fifa22/fifa2020-2021.csv")
final_table <- final_table[c('Team', 'Pts')]

f22
colnames(f22)

f22_sp <- f22 %>% filter(league_name=='Spain Primera Division')
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

# select specific columns
f22_sp <- f22_sp[cols_to_choose]
for(i in 1:length(cols_to_choose)){
  print(sum(is.na(f22_sp[i])))
}

#compute players per team
ppt <- f22_sp %>% group_by(club_name) %>%count()
ppt

#replace na's with 0
f22_sp[is.na(f22_sp)] <- 0
    
f22_sp <- f22_sp%>% 
  group_by(club_name) %>% 
  summarise(across(where(is.numeric), mean))




final_table
f22_sp$Pts <- c(46, 86, 44, 44, 38, 36, 79, 38, 46, 41, 67*0.5, 53, 82*0.5, 82*0.5, 61, 84, 62, 77, 43, 58)
f22_sp[c("club_name", "Pts")] %>% arrange(desc(Pts))

y <- f22_sp$Pts
x <- select(f22_sp, -c("Pts", "club_name") ) %>% as.matrix() %>% scale()


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


# For dataset
# compute averages by player
# calculate month of birth for player
# modify goalkeeper stats to 0 for 

# For model
# different values of lambda

