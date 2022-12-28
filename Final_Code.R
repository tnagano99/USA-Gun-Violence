library(ggplot2)
library(dplyr)
library(stringr)
library(caTools)
library(ggpubr)
library(car)
library(plm)
library(lmtest)
library(psych)
library(corrplot)
library(fastDummies)
library(MASS)
library(car)
library(methods)
library(boot)
library(tree)
library(rpart)				        
library(rpart.plot)
library(randomForest)
library(GGally)
library(ggfortify)
library(caret)

dir <- "C:/Users/tnaga/Documents/Github/"

gun <- read.csv(paste0(dir, "Gun_Violence.csv"))

################################# Data Cleaning ###########################################

# remove unnecessary columns 
gun[c("incident_id", "state","date", "address", "incident_url", "incident_url_fields_missing", "source_url", "notes", "sources", "participant_name", "participant_age", "participant_relationship", "latitude", "longitude", "location_description", "state_house_district", "state_senate_district", "congressional_district", "gun_stolen", "gun_type")] <- list(NULL)

# check columns with missing values
colSums(is.na(gun))

# check columns with empty strings
colSums(gun == "")

# replace median
gun$n_guns_involved[is.na(gun$n_guns_involved)] <- median(gun$n_guns_involved, na.rm=TRUE)

gun <- gun[!(gun$incident_characteristics == ""),] 
gun <- gun[!(gun$participant_age_group == ""),]
gun <- gun[!(gun$participant_status == ""),]
gun <- gun[!(gun$participant_gender == ""),]
gun <- gun[!(gun$participant_type == ""),]

################################### Feature Engineering ###########################################

# get number of victims and suspects
numUnharmed <- c()
partstatusList<- strsplit(gun$participant_status, split="||", fixed=TRUE)

for (i in 1:length(partstatusList)){
  partstatus <- unlist(partstatusList[i])
  unharmed <- 0
  
  # number of victims an suspects
  for (j in 1:length(partstatus)){
    if (length(partstatus[j]) == 0){
      next
    }
    if (length(partstatus[j]) == 1){
      partstatus <- unlist(strsplit(partstatus, split="|", fixed=TRUE))
    }
    if (length(grep("Unharmed", partstatus[j])) > 0) {
      unharmed <- unharmed + 1
    } else{
      next
    }
  }
  numUnharmed <- c(numUnharmed, unharmed)
}
gun$num_unharmed <- numUnharmed

# get average Age of victims and suspects
numVictims <- c()
numSuspects <- c()

partsList<- strsplit(gun$participant_type, split="||", fixed=TRUE)

for (i in 1:length(partsList)){
  parts <- unlist(partsList[i])
  numVics <- 0
  numSus <- 0
  
  # number of victims an suspects
  for (j in 1:length(parts)){
    if (length(parts[j]) == 0){
      next
    }
    if (length(parts[j]) == 1){
      parts <- unlist(strsplit(parts, split="|", fixed=TRUE))
    }
    if (length(grep("Victim", parts[j])) > 0) {
      numVics <- numVics + 1
    } else{
      numSus <- numSus + 1
    }
  }
  numVictims <- c(numVictims, numVics)
  numSuspects <- c(numSuspects, numSus)
}

gun$num_victims <- numVictims
gun$num_suspects <- numSuspects

# convert incident characteristics to dummy variables
# go through every row and get each unique characteristic
charsUnique <- c()
charsList<- strsplit(gun$incident_characteristics, split="||", fixed=TRUE)
for (i in 1:length(charsList)){
  chars <- unlist(charsList[i])
  for (j in 1:length(chars)){
    if (length(chars[j]) == 0){
      next
    }
    if (chars[j] %in% charsUnique) {
      next
    } else{
      charsUnique <- c(charsUnique, chars[j])
    }
  }
}

# some of the characteristics are delimitted by \
# extract all unique charateristics
cleanUnique <- c()
for (i in 1:length(charsUnique)){
  chars <- unlist(strsplit(charsUnique[i], split="|", fixed=TRUE))
  for (j in 1:length(chars)){
    if (length(chars[j]) == 0){
      next
    }
    if (chars[j] %in% cleanUnique) {
      next
    } else{
      cleanUnique <- c(cleanUnique, chars[j])
    }
  }
}
cleanUnique <- cleanUnique[!is.na(cleanUnique)]

# Assign 1s to all appropriate columns
charMatrix <- matrix(0, nrow(gun), length(cleanUnique))
charsList<- strsplit(gun$incident_characteristics, split="||", fixed=TRUE)
for (i in 1:nrow(gun)){
  chars <- unlist(charsList[i])
  if (length(chars) == 1){
    chars <- unlist(strsplit(chars, split="|", fixed=TRUE))
  }
  idx <- match(chars, cleanUnique)
  charMatrix[i, idx] <- 1
}
df_char <- as.data.frame(charMatrix)
colnames(df_char) <- cleanUnique

# create matrix of dummy variables
# get dummy variables for suspect and victim for gender, age group, status

partsList<- strsplit(gun$participant_type, split="||", fixed=TRUE)

gendSuspects <- c("Male", "Female")
GendList <- strsplit(gun$participant_gender, split="||", fixed=TRUE)
genSuspectsMatrix <- matrix(0, nrow(gun), length(gendSuspects))

for (i in 1:nrow(gun)){
  gends <- unlist(GendList[i])
  if (length(gends) == 1){
    gends <- unlist(strsplit(gends, split="|", fixed=TRUE))
  }
  parts <- unlist(partsList[i])
  if (length(parts) == 1){
    parts <- unlist(strsplit(parts, split="|", fixed=TRUE))
  }
  
  idx <- c()
  for (j in 1:length(gends)){
    if (length(grep("Suspect", parts[j])) > 0) {
      if (length(grep("Male", gends[j])) > 0) {
        idx <- c(idx, 1)
      }
      if (length(grep("Female", gends[j])) > 0) {
        idx <- c(idx, 2)
      }
    }
    
  }
  genSuspectsMatrix[i, idx] <- 1
}
df_gendSuspects <- as.data.frame(genSuspectsMatrix)
gendSuspects <- c("Male_Suspects", "Female_Suspects")
colnames(df_gendSuspects) <- gendSuspects

gendVictims <- c("Male", "Female")
GendList <- strsplit(gun$participant_gender, split="||", fixed=TRUE)
genVictimsMatrix <- matrix(0, nrow(gun), length(gendVictims))

for (i in 1:nrow(gun)){
  gends <- unlist(GendList[i])
  if (length(gends) == 1){
    gends <- unlist(strsplit(gends, split="|", fixed=TRUE))
  }
  parts <- unlist(partsList[i])
  if (length(parts) == 1){
    parts <- unlist(strsplit(parts, split="|", fixed=TRUE))
  }
  
  idx <- c()
  for (j in 1:length(gends)){
    if (length(grep("Victim", parts[j])) > 0) {
      if (length(grep("Male", gends[j])) > 0) {
        idx <- c(idx, 1)
      }
      if (length(grep("Female", gends[j])) > 0) {
        idx <- c(idx, 2)
      }
    }
  }
  genVictimsMatrix[i, idx] <- 1
}
df_gendVictims <- as.data.frame(genVictimsMatrix)
gendVictims <- c("Male_Victims", "Female_Victims")
colnames(df_gendVictims) <- gendVictims


statusVictims <- c("Injured", "Killed", "Unharmed")
StatList <- strsplit(gun$participant_status, split="||", fixed=TRUE)
statusVictimsMatrix <- matrix(0, nrow(gun), length(statusVictims))

for (i in 1:nrow(gun)){
  stats <- unlist(StatList[i])
  if (length(stats) == 1){
    stats <- unlist(strsplit(stats, split="|", fixed=TRUE))
  }
  parts <- unlist(partsList[i])
  if (length(parts) == 1){
    parts <- unlist(strsplit(parts, split="|", fixed=TRUE))
  }
  
  idx <- c()
  for (stat in stats){
    if (length(grep("Victim", parts[j])) > 0) {
      if (length(grep("Injured", stat)) > 0) {
        idx <- c(idx, 1)
      }
      if (length(grep("Killed", stat)) > 0) {
        idx <- c(idx, 2)
      }
      if (length(grep("Unharmed", stat)) > 0) {
        idx <- c(idx, 3)
      }
    }
  }
  statusVictimsMatrix[i, idx] <- 1
}
df_statusVictims <- as.data.frame(statusVictimsMatrix)
statusVictims <-  c("Injured_Victims", "Killed_Victims", "Unharmed_Victims")
colnames(df_statusVictims) <- statusVictims


statusSuspects <- c("Injured", "Killed", "Unharmed", "Arrested")
StatList <- strsplit(gun$participant_status, split="||", fixed=TRUE)
statusSuspectsMatrix <- matrix(0, nrow(gun), length(statusSuspects))

for (i in 1:nrow(gun)){
  stats <- unlist(StatList[i])
  if (length(stats) == 1){
    stats <- unlist(strsplit(stats, split="|", fixed=TRUE))
  }
  parts <- unlist(partsList[i])
  if (length(parts) == 1){
    parts <- unlist(strsplit(parts, split="|", fixed=TRUE))
  }
  
  idx <- c()
  for (stat in stats){
    if (length(grep("Suspect", parts[j])) > 0) {
      if (length(grep("Injured", stat)) > 0) {
        idx <- c(idx, 1)
      }
      if (length(grep("Killed", stat)) > 0) {
        idx <- c(idx, 2)
      }
      if (length(grep("Unharmed", stat)) > 0) {
        idx <- c(idx, 3)
      }
      if (length(grep("Arrested", stat)) > 0) {
        idx <- c(idx, 4)
      }
    }
  }
  statusSuspectsMatrix[i, idx] <- 1
}
df_statusSuspects <- as.data.frame(statusSuspectsMatrix)
statusSuspects <- c("Injured_Suspects", "Killed_Suspects", "Unharmed_Suspects", "Arrested_Suspects")
colnames(df_statusSuspects) <- statusSuspects


ageGroupVictims <- c("Adult 18+", "Child 0-11", "Teen 12-17")
AgeGroupList <- strsplit(gun$participant_age_group, split="||", fixed=TRUE)
ageVictimMatrix <- matrix(0, nrow(gun), length(ageGroupVictims))

for (i in 1:nrow(gun)){
  ages <- unlist(AgeGroupList[i])
  if (length(ages) == 1){
    ages <- unlist(strsplit(ages, split="|", fixed=TRUE))
  }
  parts <- unlist(partsList[i])
  if (length(parts) == 1){
    parts <- unlist(strsplit(parts, split="|", fixed=TRUE))
  }
  
  idx <- c()
  for (age in ages){
    if (length(grep("Victim", parts[j])) > 0) {
      if (length(grep("Adult 18+", ages)) > 0) {
        idx <- c(idx, 1)
      }
      if (length(grep("Child 0-11", ages)) > 0) {
        idx <- c(idx, 2)
      }
      if (length(grep("Teen 12-17", ages)) > 0) {
        idx <- c(idx, 3)
      }
    }
  }
  ageVictimMatrix[i, idx] <- 1
}
df_ageGroupVictims <- as.data.frame(ageVictimMatrix)
ageGroupVictims <- c("Adult 18+_Victims", "Child 0-11_Victims", "Teen 12-17_Victims")
colnames(df_ageGroupVictims) <- ageGroupVictims

ageGroupSuspects <- c("Adult 18+", "Child 0-11", "Teen 12-17")
AgeGroupList <- strsplit(gun$participant_age_group, split="||", fixed=TRUE)
ageSuspectsMatrix <- matrix(0, nrow(gun), length(ageGroupVictims))

for (i in 1:nrow(gun)){
  ages <- unlist(AgeGroupList[i])
  if (length(ages) == 1){
    ages <- unlist(strsplit(ages, split="|", fixed=TRUE))
  }
  parts <- unlist(partsList[i])
  if (length(parts) == 1){
    parts <- unlist(strsplit(parts, split="|", fixed=TRUE))
  }
  
  idx <- c()
  for (age in ages){
    if (length(grep("Suspect", parts[j])) > 0) {
      if (length(grep("Adult 18+", ages)) > 0) {
        idx <- c(idx, 1)
      }
      if (length(grep("Child 0-11", ages)) > 0) {
        idx <- c(idx, 2)
      }
      if (length(grep("Teen 12-17", ages)) > 0) {
        idx <- c(idx, 3)
      }
    }
  }
  ageSuspectsMatrix[i, idx] <- 1
}
df_ageGroupSuspects <- as.data.frame(ageSuspectsMatrix)
ageGroupSuspects <- c("Adult 18+_Suspects", "Child 0-11_Suspects", "Teen 12-17_Suspects")
colnames(df_ageGroupSuspects) <- ageGroupSuspects

# cbind all data frames together
df <- cbind(gun, df_char, df_ageGroupSuspects, df_gendSuspects, df_gendVictims, df_statusSuspects, df_statusVictims)
df[c("incident_characteristics", "participant_age_group", "participant_type", "participant_status", "participant_gender")] <- NULL

names <- colnames(df)
names <- gsub(" ", "_", names, fixed=TRUE)
names <- gsub("/", "_", names, fixed=TRUE)
names <- gsub("-", "", names, fixed=TRUE)
names <- gsub("+", "m", names, fixed=TRUE)
names <- gsub("^", "", names, fixed=TRUE)
names <- gsub(",", "_", names, fixed=TRUE)
names <- gsub("(", "_", names, fixed=TRUE)
names <- gsub(")", "_", names, fixed=TRUE)
names <- gsub("{", "_", names, fixed=TRUE)
names <- gsub("}", "_", names, fixed=TRUE)
names <- gsub("&", "_", names, fixed=TRUE)
names <- gsub(":", "_", names, fixed=TRUE)
names <- tolower(names)
colnames(df) <- names

# number of suspects and number unharmed correlated remove
# check for correlation on all variables
df$num_suspects <- NULL
dummyVars <- df
dummyVars$region<- NULL
cont_cols <- c("n_killed", "n_injured", "num_victims", "num_unharmed", "n_guns_involved")
dummyVars[,cont_cols] <- NULL
dummyVars$num_suspects <- NULL


df_num <- cbind(quantVars,dummyVars)
df_num[,c("city", "large_city", "city_or_county")] <-NULL
corr_mat <- round(cor(df_num),2)

correlated <- c()
for (i in 1:nrow(corr_mat)){
  for (j in i:ncol(corr_mat)){
    if (i == j){
      next
    }else if (corr_mat[i,j] > 0.8){
      correlated <- c(correlated, c(c(i,j), colnames(corr_mat)[i], colnames(corr_mat)[j], corr_mat[i,j]))
    }
  }
}
cor_vars <- as.data.frame(matrix(correlated, ncol = 5, byrow=TRUE))
colnames(cor_vars) <- c("row index", "col index", "row", "col", "value")

# remove adult_suspects, shot__dead__murder__accidental__suicide_ cause correlated with other columns.
cor_cols <- c("shot__dead__murder__accidental__suicide_", "adult_18m_suspects")
df_num[,cor_cols] <- NULL

# school incident and gun_at_school__no_death_injury__elementary_secondary_school mean something similar
# combine with max value from eachto make single school incident column
df_num$school <- pmax(df_num$school_incident, df_num$gun_at_school__no_death_injury__elementary_secondary_school)
df_num[,c("gun_at_school__no_death_injury__elementary_secondary_school", "school_incident")] <- NULL

# accidental_shooting, accidental_shooting__injury and accidental_negligent_discharge all mean similar
df_num$accident <- pmax(df_num$accidental_shooting, df_num$accidental_shooting__injury)
df_num$accident <- pmax(df_num$accident, df_num$accidental_negligent_discharge)
df_num[,c("accidental_shooting__injury", "accidental_negligent_discharge", "accidental_shooting")] <- NULL

# separate numeric and dummy variables
df_quant <- df_num[,c("n_killed", "n_injured", "n_guns_involved", "num_unharmed", "num_victims")]
df_num[,c("n_killed", "n_injured", "n_guns_involved", "num_unharmed", "num_victims")] <- NULL

# convert dummy variables to factors
df_cat <- data.frame(lapply(df_num, factor))
colnames(df_cat) <- colnames(df_num)

# create large city categories
city_counts <- table(df$city)
large_idx <- city_counts > 300
large_city_names <- names(city_counts[large_idx])

# more than 1 gun violence event per week
city300 <- as.numeric(city_counts[large_idx])
names(city300) <- large_city_names


df <- cbind(df_quant, df_cat)
df_raw <- df
df$num_suspects <- NULL

df$city <- df_raw$city_or_county
df$city <- as.character(df$city)

df$large_city <-  ifelse(df$city %in% large_city_names, "High", "Low")
df$large_city <- as.factor(df$large_city)

df$city <- NULL
df$num_suspects <- NULL

# PCA
colnames(df_quant) <- c("Number Killed", "Number Injured", "Number Guns", "Number Unharmed", "Number Victims")
df_scale <- scale(df_quant)
pca <- prcomp(df_scale, scale=TRUE)

# Decision Tree 
set.seed(3)
sample <- sample.split(df$large_city, SplitRatio = 0.9)

df_train <- subset(df, sample==TRUE)
df_test <- subset(df, sample==FALSE)

overfitdtree=rpart(large_city~., data=df_test,control=rpart.control(cp=0.0000001))
printcp(overfitdtree)
best_cp=overfitdtree$cptable[which.min(overfitdtree$cptable[,"xerror"]),"CP"]

# Random forest

# tune the number of predictors at each split
df_X <- df
df_X[,c("large_city")] <- NULL
df_y <- df$large_city
tuneR <- tuneRF(x=df_X, y=df_y, ntreeTry = 100, mytryStart = 11, stepFactor=1, improve=0.000351, trace=TRUE)

# Final model
set.seed(3)
myforest=randomForest(large_city~., ntree=100, data=df_train, mtry = 11, importance=TRUE, cp=0.000351, na.action = na.omit, do.trace=5)
summary(myforest)

rfpreds <- predict(myforest, df_test)
conf_mat <- confusionMatrix(rfpreds, df_test$large_city)

############################################# Plots ##################################################

# visualize all continuous variables as boxplots and histograms
df$num_suspects <- df_raw$num_suspects
cont_cols <- c("n_killed", "n_injured", "num_victims", "num_suspects", "num_unharmed", "n_guns_involved")
names <- c("Number Killed", "Number Injured", "Number Victims", "Number Suspects", "Number Unharmed", "Number of Guns")
for (col in 1:length(cont_cols)) {
  hist(df[,cont_cols[col]], main=names[col], xlab=names[col])
  boxplot(df[,cont_cols[col]], xlab=names[col])
  print(names[col])
  print(table(df[,cont_cols[col]]))
}

# summarize continuous variables
summary(df[,cont_cols])

# convert columns to factors to create barplots
for (col in 1:length(cont_cols)) {
  value <- factor(ifelse(df[,cont_cols[col]] < 2, as.character(df[,cont_cols[col]]), ifelse(df[,cont_cols[col]] < 4, "2-3", ">=4")),levels=c("0","1", "2-3", ">=4"))
  barplot(table(value), main=names[col], xlab=names[col], ylab="Count")
}

# check for correlations
quantVars <- df[,cont_cols]
corr_matrix <- cor(quantVars)
colnames(corr_matrix) <- names
rownames(corr_matrix) <- names
par(mar=c(4,4,4,4))
corrplot(round(corr_matrix,2),method="number", mar=c(0,0,1,0))

# plot all categories of dummy variables
names <- colnames(df_char)
names <- gsub(" ", "_", names, fixed=TRUE)
names <- gsub("/", "_", names, fixed=TRUE)
names <- gsub("-", "", names, fixed=TRUE)
names <- gsub("+", "m", names, fixed=TRUE)
names <- gsub("^", "", names, fixed=TRUE)
names <- gsub(",", "_", names, fixed=TRUE)
names <- gsub("(", "_", names, fixed=TRUE)
names <- gsub(")", "_", names, fixed=TRUE)
names <- gsub("{", "_", names, fixed=TRUE)
names <- gsub("}", "_", names, fixed=TRUE)
names <- gsub("&", "_", names, fixed=TRUE)
names <- gsub(":", "_", names, fixed=TRUE)
names <- tolower(names)
colnames(df_char) <- names

df_char$school <- pmax(df_char$school_incident, df_char$gun_at_school__no_death_injury__elementary_secondary_school)
df_char[,c("gun_at_school__no_death_injury__elementary_secondary_school", "school_incident")] <- NULL

# accidental_shooting, accidental_shooting__injury and accidental_negligent_discharge all mean similar
df_char$accident <- pmax(df_char$accidental_shooting, df_char$accidental_shooting__injury)
df_char$accident <- pmax(df_char$accident, df_char$accidental_negligent_discharge)
df_char[,c("accidental_shooting__injury", "accidental_negligent_discharge", "accidental_shooting")] <- NULL

# df_char, df_ageGroupSuspects, df_gendSuspects, df_gendVictims, df_statusSuspects, df_statusVictims
par(mar=c(15,6,2,2))
tbl <- colSums(df_char == 1)
names(tbl) <- substr(names(tbl), 1, 30)
tmp_names <- c()
for (i in 1:length(names(tbl))){
  tmp_names <- c(tmp_names, gsub( "_", " ", names(tbl)[i], fixed=TRUE))
}

par(mar=c(4,6,2,2))
tbl <- colSums(df_ageGroupSuspects == 1) / nrow(df_ageGroupSuspects)
names(tbl) <- c("Adult 18+", "Child 0-11", "Teen 12-17")
barplot(tbl, main = "Density of Suspects Age Group")
mtext(text="Suspects Age Group", side=1, line=3)
mtext(text="Density", side=2, line=4)

par(mar=c(4,6,2,2))
tbl <- colSums(df_ageGroupVictims == 1) / nrow(df_ageGroupVictims)
names(tbl) <- c("Adult 18+", "Child 0-11", "Teen 12-17")
barplot(tbl, main = "Density of Victims Age Group")
mtext(text="Victims Age Group", side=1, line=3)
mtext(text="Density", side=2, line=4)

par(mar=c(4,6,2,2))
tbl <- colSums(df_gendSuspects == 1) / nrow(df_gendSuspects) 
names(tbl) <- c("Male", "Female")
barplot(tbl, main = "Density of Suspects Gender")
mtext(text="Suspects Gender", side=1, line=3)
mtext(text="Density", side=2, line=4)

par(mar=c(4,6,2,2))
tbl <- colSums(df_gendVictims == 1) / nrow(df_gendVictims)
names(tbl) <- c("Male", "Female")
barplot(tbl, main = "Density of Victims Gender")
mtext(text="Victims Gender", side=1, line=3)
mtext(text="Density", side=2, line=4)

par(mar=c(4,6,2,2))
tbl <- colSums(df_statusSuspects == 1) / nrow(df_statusSuspects)
names(tbl) <- c("Injured", "Killed", 'Unharmed', 'Arrested')
barplot(tbl, main = "Density of Suspects Status")
mtext(text="Suspects Status", side=1, line=3)
mtext(text="Density", side=2, line=4)

par(mar=c(4,6,2,2))
tbl <- colSums(df_statusVictims == 1) / nrow(df_statusVictims)
names(tbl) <- c("Injured", "Killed", 'Unharmed')
barplot(tbl, main = "Density of Victims Status")
mtext(text="Victims Status", side=1, line=3)
mtext(text="Density", side=2, line=4)

# plot PCA
autoplot(pca, data = df_quant, loadings = TRUE, col=ifelse(df$large_city=="High","blue","red"), loadings.label = TRUE)

# Plot random Forest
plotcp(overfitdtree)

###################################### Save R data file ###################################

save(df, df_raw, myforest, df_char, df_ageGroupSuspects, df_gendSuspects, df_gendVictims, df_statusSuspects, df_statusVictims, file = "gun.RData")
