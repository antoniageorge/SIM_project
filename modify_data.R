# This file will modify our original datasets and clean them up a little.
# This takes a while so we will do it seperately.


#1) Remove goalie statistics from non-GK players


# Load the data
f22 <- read_csv("./data/players_22_original.csv")
f21 <- read_csv("./data/players_21_original.csv")
f20 <- read_csv("./data/players_20_original.csv")
f19 <- read_csv("./data/players_19_original.csv")
f18 <- read_csv("./data/players_18_original.csv")
f17 <- read_csv("./data/players_17_original.csv")
f16 <- read_csv("./data/players_16_original.csv")
f15 <- read_csv("./data/players_15_original.csv")


# 1) Remove GK statistics from any player that IS NOT a goalkeeper
for(i in 1:nrow(f15)){
  if(f15[i,18]!='GK'|is.na(f15[i,18])){
    f15[i,73] <- 0
    f15[i,74] <- 0
    f15[i,75] <- 0
    f15[i,76] <- 0
    f15[i,77] <- 0
    f15[i,78] <- 0
  }
}

for(i in 1:nrow(f16)){
  if(f16[i,18]!='GK'|is.na(f16[i,18])){
    f16[i,73] <- 0
    f16[i,74] <- 0
    f16[i,75] <- 0
    f16[i,76] <- 0
    f16[i,77] <- 0
    f16[i,78] <- 0
  }
}

for(i in 1:nrow(f17)){
  if(f17[i,18]!='GK'|is.na(f17[i,18])){
    f17[i,73] <- 0
    f17[i,74] <- 0
    f17[i,75] <- 0
    f17[i,76] <- 0
    f17[i,77] <- 0
    f17[i,78] <- 0
  }
}

for(i in 1:nrow(f18)){
  if(f18[i,18]!='GK'|is.na(f18[i,18])){
    f18[i,73] <- 0
    f18[i,74] <- 0
    f18[i,75] <- 0
    f18[i,76] <- 0
    f18[i,77] <- 0
    f18[i,78] <- 0
  }
}

for(i in 1:nrow(f19)){
  if(f19[i,18]!='GK'|is.na(f19[i,18])){
    f19[i,73] <- 0
    f19[i,74] <- 0
    f19[i,75] <- 0
    f19[i,76] <- 0
    f19[i,77] <- 0
    f19[i,78] <- 0
  }
}

for(i in 1:nrow(f20)){
  if(f20[i,18]!='GK'|is.na(f20[i,18])){
    f20[i,73] <- 0
    f20[i,74] <- 0
    f20[i,75] <- 0
    f20[i,76] <- 0
    f20[i,77] <- 0
    f20[i,78] <- 0
  }
}

for(i in 1:nrow(f21)){
  if(f21[i,18]!='GK'|is.na(f21[i,18])){
    f21[i,73] <- 0
    f21[i,74] <- 0
    f21[i,75] <- 0
    f21[i,76] <- 0
    f21[i,77] <- 0
    f22[i,78] <- 0
  }
}

for(i in 1:nrow(f22)){
  if(f22[i,18]!='GK'|is.na(f22[i,18])){
    f22[i,73] <- 0
    f22[i,74] <- 0
    f22[i,75] <- 0
    f22[i,76] <- 0
    f22[i,77] <- 0
    f22[i,78] <- 0
  }
}


# export datasets
write.csv(f22, "data/players_22.csv")
write.csv(f21, "data/players_21.csv")
write.csv(f20, "data/players_20.csv")
write.csv(f19, "data/players_19.csv")
write.csv(f18, "data/players_18.csv")
write.csv(f17, "data/players_17.csv")
write.csv(f16, "data/players_16.csv")
write.csv(f15, "data/players_15.csv")
