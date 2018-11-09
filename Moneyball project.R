
#Reading the data file
b = read.csv('baseball.csv')
str(b)

#Taking data till 2002(year of Moneyball)
moneyball = subset(b , Year<2002)


#Now observations are reduced to 902 from 1232.

#Lets create a new column in our dataset for run difference.
moneyball$RD = moneyball$RS - moneyball$RA

#chechking structure of data again.
str(moneyball)

#relation between wins and run difference
plot(moneyball$W,moneyball$RD)

#lets run a regression model on win using only RD as a variable

win = lm(W~ RD, data = moneyball)
summary(win)


#lets run regression using three variables mentioned
Runs = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(Runs)

# running regression without batting average.
Runs = lm(RS ~ OBP + SLG , data = moneyball)
summary(Runs)

#using regression to understand runs allowed.
Runsallowed = lm(RA ~ OOBP + OSLG ,data = moneyball)
summary(Runsallowed)

#Finding correlation between a team wins and winning the world series
#making a subset of all the values in b
#Selecting team with playoff ranks as 1 as assiging WSwin(worldseries win = 1) to them 
recent <- subset(b, Year >= 1994 & Year <= 2011)
recent[recent$RankPlayoffs == 1 & is.na(recent$RankPlayoffs) == FALSE, ]
recent$WSwin <- 0
recent[recent$RankPlayoffs == 1 & is.na(recent$RankPlayoffs) == FALSE, "WSwin"] <- 1
# Finding a correlation between world series wins and the wins in regular league.
cor(recent$WSwin, recent$W)

# Now we'll try using logistic regression to predict whether or not a team will win the World Series.
# We will need only those team making to the playoffs so we make a subset.

baseball = subset(b , b$Playoffs==1)

#Lets chaeck teams making to the playoffs overall
playoff = table(baseball$Year)
playoff
names(playoff)

#Adding one column baseball dataset to show number of teams qualifying for playoff for that year.
baseball$NumCompetitors = playoff[as.character(baseball$Year)] 

#Adding a column in baseball dataset for showing team winning the Worldseries.
baseball$worldseries = as.numeric(baseball$RankPlayoffs==1)
table(baseball$worldseries)

#LOGISTIC REGRESSION
#But we are unsure which variables we should use to run the prediction. For this we will use the bivariate model,
#testing with every independent variable to check which one should be included.

model1 = glm(worldseries~Year, data = baseball,family = binomial)
summary(model1)
model2 = glm(worldseries~RS, data = baseball,family = binomial)
summary(model2)
model3 = glm(worldseries~RA, data = baseball,family = binomial)
summary(model3)
model4 = glm(worldseries~W, data = baseball,family = binomial)
summary(model4)
model5 = glm(worldseries~OBP, data = baseball,family = binomial)
summary(model5)
model6 = glm(worldseries~SLG, data = baseball,family = binomial)
summary(model6)
model7 = glm(worldseries~BA, data = baseball,family = binomial)
summary(model7)
model8 = glm(worldseries~RankSeason, data = baseball,family = binomial)
summary(model8)
model9 = glm(worldseries~OOBP, data = baseball,family = binomial)
summary(model9)
model10 = glm(worldseries~SLG, data = baseball,family = binomial)
summary(model10)
model11 = glm(worldseries~NumCompetitors, data = baseball,family = binomial)
summary(model11)
model12 = glm(worldseries~League, data = baseball,family = binomial)
summary(model12)

#Now let's try runnig a model with those significant variables found with bivariate method. 
#Lets find how many turn out to be significant in multivariate model.

model13 = glm(worldseries~ Year + RA + NumCompetitors + RankSeason, data = baseball,family = binomial)
summary(model13)

#Lets check for correlation if there is any between these variables.
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

#Now lets build a two variable to check whether they come up with any significance.
#The two-variable models can be built with the following commands:
  
Model14 = glm(worldseries ~ Year + RA, data=baseball, family=binomial)
Model15 = glm(worldseries ~ Year + RankSeason, data=baseball, family=binomial)
Model16 = glm(worldseries ~ Year + NumCompetitors, data=baseball, family=binomial)
Model17 = glm(worldseries ~ RA + RankSeason, data=baseball, family=binomial)
Model18 = glm(worldseries ~ RA + NumCompetitors, data=baseball, family=binomial)
Model19 = glm(worldseries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)

summary(Model14)
summary(Model15)
summary(Model16)
summary(Model17)
summary(Model18)
summary(Model19)
