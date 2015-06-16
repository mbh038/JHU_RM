# This time we'll generate the necessary fake data
# within R itself, rather than working in Python and 
# then reading the data into R.  This will demonstrate
# some handy R commands that you may not have known about.


# Let's start with a problem where the predictor variable X
# does not seem to have a linear relationship with Y and 
# needs to be transformed 


rawx = runif(200,min=0,max=10)
X = exp(rawx)
ydev = rnorm(200,0,1)
Y = rawx + ydev

png("plotOfYonX.png")
plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")
graphics.off()

# Now try transforming the X variable using the natural 
# log function

logX = log(X)

png("plotOfYonLogX.png")
plot (logX,Y,col="red",pch=16)
title(main="A plot of Y on log(X)")
graphics.off()

# Note the correlations between Y and each version of X
cor(X,Y)
cor(logX,Y)




# Another problem where the predictor variable X
# does not seem to have a linear relationship with Y and 
# needs to be transformed (x^2 this time)

rawx = runif(200,min=0,max=80)
rawx = sort(rawx)
X = sqrt(rawx)
ydev = rnorm(200,0,2)
Y = rawx + ydev

png("plotOfYonX2.png")
plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")
graphics.off()

expX = exp(X)
X2 = X * X

png("plotOfYonExpX.png")
plot (expX,Y,col="red",pch=16)
title(main="A plot of Y on exp(X)")
graphics.off()

png("plotOfYonXSquared.png")
plot (X2,Y,col="red",pch=16)
title(main="A plot of Y on X-squared")
graphics.off()


# Fit linear models to the raw X and the two transformations
unchanged = lm (Y ~ X)
expTrans = lm ( Y ~ expX )
squareTrans = lm ( Y ~ X2 )

# Use AIC to choose between the three models
AIC (unchanged,expTrans,squareTrans)

# Plot the predictions from the three models all on one plot
png("plotOfModelFits.png")
plot (X,Y,col="red",pch=16)
lines(X,predict(unchanged),col="blue",lw=2)
lines(X,predict(expTrans),col="green",lw=2)
lines(X,predict(squareTrans),col="black",lw=2)
title(main="Various models for Y")
graphics.off()



# Now a problem where there's a polynomial relationship between 
# X and Y.

X = runif(200,min=-10,max=10)
X = sort(X)
ydev = rnorm(200,0,4)
Y = 62.3 + 0.4 * X - 0.2 * X^2 + 0.03 * X^3 + ydev

png("plotOfYonPolyX.png")
plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")
graphics.off()


# We fit the null, the linear, and some polynomial models of
# increasing degree.  In fact the right answer is the 3rd order
# polynomial model (look how we created the data above) and
# the AIC method identifies this model.
m0 = lm ( Y ~ 1 )
m1 = lm ( Y ~ X )
m2 = lm ( Y ~ X + I(X^2) )
m3 = lm ( Y ~ X + I(X^2) + I(X^3) )
m4 = lm ( Y ~ X + I(X^2) + I(X^3) + I(X^4) )


AIC(m0,m1,m2,m3,m4)


# Produce a plot of the different polynomial models 
# fitted to the data
png("plotOfPolyModelFits.png",width=600,height=600)
plot (X,Y,col="red",pch=16)
lines(X,predict(m0),col="grey",lw=2)
lines(X,predict(m1),col="green",lw=2)
lines(X,predict(m2),col="yellow",lw=2)
lines(X,predict(m2),col="blue",lw=2)
lines(X,predict(m3),col="black",lw=2)
title(main="Various polynomial models for Y")
graphics.off()



# Now investigate a situation where there is a significant
# interaction term

X = runif(200,min=0,max=20)
X = sort(X)

GroupChoice = runif(200)
Group = ifelse(GroupChoice < 0.5, "A", "B")
Y = ifelse(GroupChoice < 0.5, 36 + X*4.2, 100 - X*2)
colour = ifelse(GroupChoice < 0.5, "red", "blue")
ydev = rnorm(200,0,12)
Y = Y + ydev


# Plot the relationship between X and Y
png("plotOfYInteractionAllSame.png")
plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")
graphics.off()

# Plot the relationship between X and Y, highlighting 
# the effects of Group
png("plotOfYInteraction.png")
plot (X,Y,col=colour,pch=16)
title(main="A plot of Y on X")
graphics.off()



# Now fit a regression model of Y on X and Group
# Use the drop1 command to see if the interaction term can go
Group = as.factor(Group)
fullModel = lm ( Y ~ X*Group)
summary(fullModel)
drop1(fullModel)


# Plot the relationship between X and Y, highlighting 
# the effects of Group, and showing the fitted model
png("plotOfYInteractionFitted.png",width=600,height=600)
plot (X,Y,col=colour,pch=16)
points(X,predict(fullModel),col="green",pch=15)
title(main="A plot of Y on X, with fitted interaction model")
graphics.off()