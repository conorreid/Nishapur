# This will be an analysis of TPQ (coin deposit date) and the actual date of the 
# coin. Are coins only in circulation for a short period of time after minting, or
# do they stay around for quite a long time? Is there a relationship between 
# circulation time and the deposit date; ie do only really old coins from the early
# 10th century circulate for a long time, whilst coins from the middle and end of 
# the century get deposited more quickly? We'll examine this question in this study.
# For this analysis, I will use TPQ date of the hoard each coin was found in, the 
# actual date of the coin and a constructed 'difference' variable, which is the 
# difference between the TPQ and the actual date on the coin itself. A larger 
# 'difference' variable means the coin is older. 
# 
# The data for this study is from Professor Roman Kovalev's as of yet unpublished
# catalog of Islamic dirham coin hoards from around Eurasia dating from the 8th to
# 11th centuries. All datees are in the Islamic Hijri calendar.

# First, let's import our data
TPQ <- read.csv("C:/Users/Conor/Dropbox/TCNJ/MUSE/Nishapur Analysis/TPQ.csv")
attach(TPQ)
# With our data entered, let's see if we can spot any patterns in the data, and 
# tease them out. First, we'll start with TPQ vs the actual coin date
plot(Hijri.Date, Hijri.TPQ)
# Looking at the graph, it seems that the older a coin it, the higher chance it 
# will have stayed in circulation and been buried later. Very old coins can have a
# low or high TPQ, yet newer coins have only relatively medium TPQ dates. To 
# examine this relationship, let's run a simple regression
TPQ.and.Date <- lm(Hijri.Date ~ Hijri.TPQ + I(Hijri.TPQ^2))
summary(TPQ.and.Date)
# The numbers here aren't very meaninful in their actual values, but the signs and
# such are interesting, to say the least. With an F-statistic of 76.43, and p-values
# much lower than 0.05, and an R squared of 0.1466 (or 14.66% of the variation in
# the coin date is accounted for by TPQ date), this data does indeed look 
# significant. And what it says is interesting. It states that as TPQ rises, so too
# does the coin date. However, once it reachs a certain point, the squared terms 
# accounts for the fact that it actually then starts to lower the coin date. Once
# TPQ because so large, ie it becomes very old, then it actually predicts the coin
# date will be lower, hence the negative sign. This result holds true with our 
# prediction via the graphs. However, this model doesn't really say much in terms
# of actual predictive power or explanation outside of actual general postive/
# negative signs due to the numbers being sort of non-sensical. Perhaps we should
# look at our constructed variable, the difference between TPQ and actual date, 
# and see if we can create a more powerful model using that variable? To do so, 
# let's first graph the data. We'll start with difference and actual coin date.
plot(Hijri.Date, Difference)
# Looking at this graph, it seems to confirm the results of our previous 
# regression. As can be observed, the coins with early dates can have very small or
# large differences between that and the TPQ, whereas later coins, especially after 
# 330 or so only have medium to low differences, all around or below 50 years. 
# Let's see if, using this data, we can construct a better model
Difference.and.Date <- lm(Difference ~ Hijri.Date)
summary(Difference.and.Date)
# Let's also look at the model on our actual graph
abline(Difference.and.Date)
# This model further confirms our results. Although the R squared is now less than
# before (0.059), the small p-values and large F-statistic mean the model is 
# significant. And what it says is similar to the graph and our last model. As 
# the date on the coin rises, ie as the coin is younger, its TPQ date goes down. 
# Specifically, every 1 year increase in the coin date means a 0.41 year decrease
# in the difference between it and its TPQ. Therefore, older coins, or coins with
# smaller dates, have larger differences on average with TPQ than younger coins.
# This is consistent with the model and graphs we've already seen. However, this
# model still has very small predictive power due to such a low R squared that only
# explains around 6% of the variation in TPQ and coin date difference using the 
# year on the coin. Perhaps we should try our difference variable and the TPQ of 
# coins instead, and hope we can get a better predictive model out of that. Let's 
# first plot our data to see if any pattern exists.
plot(Hijri.TPQ, Difference)
# It seems there's a very clear pattern, clearer than any in of our other data. 
# As TPQ rises, so too does the difference, and very clearly. From around 300 to 
# 350, there are coins deposited at a difference of almost 0, so immediately, to
# up to 50. But after 350, which is rougly 960 AD, the difference then begins to
# increase. It looks like only old coins are sticking around, and newer minted ones
# are deposited immediately. Let's construct a linear model to see this 
# relationship in full.
Difference.and.TPQ <- lm(Difference ~ Hijri.TPQ)
summary(Difference.and.TPQ)
abline(Difference.and.TPQ)
# Wow. This model seems to be very powerful. The p-values and F-statistic are 
# essentially zero, and the R squared is an amazing 0.6905. That means that a 
# startling 69.05% of the variation in the difference is accounted for by the 
# TPQ of the coin hoards. Let's interpret the slope for a second, as well. As TPQ
# rises by 1 year, the difference between the TPQ and the coin date rises by 0.81
# years. That is, as TPQ gets 1 year higher, the likelihood is that the individual
# coins in that hoard are almost a year older than the ones in a hoard deposited
# a year ealier. 