#The idea is to calculate the probabilities of at least x successes on y rolls using the binomial distribution and 'dynamic' programming. This is just a fancy way of saying that we save results as we go and refer back to them to decrease the number of computations made. This way we can calculate n probabilities in n calculations. F.x. to calculate the probability of rolling at least 3 successes on 6 dice, you would sum the proabilities of rolling 3, 4, 5, and 6 successes. Here we calculate the marginal cases first, and then fill out the matrix. Successes are defined as a result above 8, rerolling 10s but still counting the 10 as a success. F.x. 4 dice are rolled resulting in: 1, 5, 8, 10. This gives 2 successes and after rerolling the 10 with the result: 9, we have a total of 3 successes. This gives a probability of success of 1/3.

#------------------
#Make a 10x10 Data frame filled with zeros
test = data.frame(matrix(rep(0,100),nrow=10,ncol=10))

#Calculate the first marginal case of having at least one success. This proability p is equal to 1 minus q, the proability of no successes.
for(i in 1:10){test[i,1] = 1 - dbinom(x = 0, size = i, prob = 1/3)}

#Now we fill the second marginal case: 10 successes. This is for all except i = 10
for(i in 1:10){test[i,10] = dbinom(10, i, 1/3)}

#Now we can fill out all cases inbetween by exploiting that the probility of rolling  x or more successes is equal to the proability to roll x successes multiplied by the probability to roll x-1 or more successes.
for(j in 9:2){for(i in 1:10){test[i,j] = dbinom(j, i, 1/3)+test[i,j+1]}}

#-------------------
#Let's see if we can't make a nice plot out of this!
library(dplyr)
library(tidyr)
library(ggplot2)

#Wrangle the data frame. Each row will have an entry for #successes, the probability and the number of dice involved.
names(test) = 1:10
test = gather(test, Successes, Probability)

#Necessary to get the plot to sort successes properly.
test$Successes = as.numeric(test$Successes)
test$Dice = rep(1:10, 10)


#Use ggplot to do a fancy tile plot.

#This part creates the tiles and fills them with color relative to the probability
testPlot = ggplot(test, aes(x=Dice, y=Successes)) + geom_tile(aes(fill = Probability), colour = 'white') + scale_fill_gradient( low = 'white', high = 'red')

#This part handles axes, background, and the text with-in the tiles through geom_text
testPlot = testPlot + theme_grey(base_size = 10) + scale_x_discrete(limit = 1:10, name = 'Dice') + scale_y_discrete(limit = 1:10, name = 'Successes')  + geom_text(aes(label = signif(Probability,2), angle = 315)) + ggtitle('Probability of y or more successes of x dice')

testPlot