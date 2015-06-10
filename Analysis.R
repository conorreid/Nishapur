#This is an examination of the output of the Samanid mint of Nishapur
#the range of this study runs from coins from 279 AH to 387 AH; roughly 100 years
#the coins were found in hoards across Eurasia, and are labeled appropriately
#for all the coins whose weights we did not have, we averaged weights of the leader
#we took out all the coins without leaders for the purpose of this examinination
#the dates will be report in Hegira rather than Gregorian
#this is for the purpose of math, for Hegira dates when converted to Gregorin look like xxx/xx rather than just a straight year
#therefore, the dates of both the coin and TPQ (deposit date) will be AH
#first, let's import and take a look at our data
Nishapur <- read.csv("C:/Users/Conor/Dropbox/TCNJ/MUSE/Nishapur Analysis/Nishapur.csv")
View(Nishapur)
#we have a few variables
#let's visualize some of these variables
attach(Nishapur)
#in this graph, we'll just look at the date
hist(Hegira.Date,breaks=108)
#there appear to be three spikes in this data
#one before 300 AH
#one around 325 AH
#and then the last near 390 AH
#the most production overall seems to be between the first two peaks
#now, let's look at the deposit dates
hist(Hegira.TPQ,breaks=105)
#most of the deposits happen before 400 AH
#and seem to be in between the two peaks of roughly 325 and 380 AH
#let's see if we can surmise any relationship between the date and weight of coins
plot(Hegira.Date, Weight..i)
#doesn't look like much, but as date increases so too does weight, only just
#now let's look and see if the date of the coin and deposit date are related
plot(Hegira.TPQ, Hegira.Date)
#this one seems to show more interesting things, mainly that coins from 350 to 400
#have the largest range of TPQ, with coins earlier or later being deposited 
#around the time of their creation
#let's see if there's any linear relationship present for this one
Dates.and.TPQ <- lm(Hegira.Date ~ Hegira.TPQ)
summary(Dates.and.TPQ)
#looks like there is! The R squared is 26.98%, pretty high
#and shows that for each 1 year increase in TPQ, the coin date increases by 0.56545 years
#we can also see if we can construct a descision tree to predit the date
#that could be a fun exercise
#first, however, we'll need test and train sets
#to do that, let's split up the data in Excel and reimport here
Train <- read.csv("C:/Users/Conor/Dropbox/TCNJ/MUSE/Nishapur Analysis/Train.csv")
Test <- read.csv("C:/Users/Conor/Dropbox/TCNJ/MUSE/Nishapur Analysis/Test.csv")
#with our train and test sets in, let's import the packages we'll need
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
#now, let's construct our tree
attach(Train)
set.seed(442)
firsttree <- rpart(Hegira.Date ~ Hegira.TPQ + Leader + Region + Weight..in.grams.)
#let's look at the tree
fancyRpartPlot(firsttree)
#looks like leader is by far the most important variable for determing date
#that makes sense, as they'd be directly correlated
#let's see how our results did! We'll use this fit to predict the test dates
Prediction <- predict(firsttree, Test)
submit <- data.frame(Hegira.Date = Prediction)
write.csv(submit, file = "firsttree.csv", row.names = FALSE)
#looking at the results, looks like the average is only 5.284 years off from actual!
#that's a pretty low error
#looks like this decision tree might work
#however, descision trees are greedy
#let's construct a random forest to see if there are other hidden variables
randomforest <- randomForest(Hegira.Date ~ Hegira.TPQ + Leader + Region + Weight..in.grams., importance=TRUE, ntree=2000)
#now let's see what variables were most important for this random forest
varImpPlot(randomforest)
#leader is still bny far the most important, but now it looks like weight is too
#and TPQ and region aren't unhelpful, either
#overall, looks like the random forest model might be a better predictor
#let's test it out!
#first, we have to set the levels in Test to that of Train so that the model won't get confused in predicting
levels(Test$Leader) <- levels(Train$Leader)
levels(Test$Region) <- levels(Train$Region)
#now let's predict our model
Prediction <- predict(randomforest, Test)
submit <- data.frame(Hegira.Date = Prediction)
write.csv(submit, file = "randomforest.csv", row.names = FALSE)
#however, looking at the errors, the random forest had an average of off by 24 years
#it appears our more complicated model has done worse!
#let's try out a conditional inference forest and see if that makes any difference
conforest <- cforest(Hegira.Date ~ Hegira.TPQ + Leader + Region + Weight..in.grams., controls=cforest_unbiased(ntree=2000))
#now let's test it out
Prediction <- predict(conforest, Test, OOB=TRUE)
submit <- data.frame(Hegira.Date = Prediction)
write.csv(submit, file = "conforest.csv", row.names = FALSE)
#and that created an even more wrong model, with an average of 41.8 years off
#looks like our original tree was the best, by far
#sometimes, less complicated is better!