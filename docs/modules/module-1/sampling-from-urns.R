#Ball and Urn Problem
# Original Script from Prof. Sindi (UC Merced)
# Modified by Dr. Quijano (University of Portland)

# Goal: To illustrate how to use R to sample with and without
#        replacement and show your theoretical and experimental 
#        results on a plot.

####################
#     Set Seed     #
####################
set.seed(42)

####################
# Create Variables #
####################

numTrials  = 10; #change#
totalBlack = 4; #change#
totalRed   = 4; #change#
totalBalls = totalBlack+totalRed;
ballsToDraw = 4; #change#

#Urn is an "vector" with the words "red" and "black" the correct number of times
urn        = c(replicate(totalRed,"red"),replicate(totalBlack,"black"))

# makes a vector to count the number times we observe a specific outcome. 
# We either draw 0, 1, 2, ..., ballsToDraw 
numBlackDrawnTally = 0*c(0:ballsToDraw)

########################
# Carry out Experiment #
########################
for (j in seq(from=1,to=numTrials,by=1))
{
  #Draw the balls 
  drawnballs = sample(urn,ballsToDraw,replace=FALSE)
  
  #Record the # of Black Balls Drawn
  blackDrawn  = sum(drawnballs=="black"); 
  
  #Add to Count Index:
  # Since arrays start at index 1, we need to shift so that if we draw 0 balls
  # we record this at array value 1.
  numBlackDrawnTally[blackDrawn + 1] = numBlackDrawnTally[blackDrawn + 1]+1
}


#####################
# TrueProbabilities #
#####################

# You will need to code this by hand. 
# You can either change my code below to match the new requirements 
# OR calculate these probabilities by hand and set the numerical values here.
# Note: There are better ways to code this than what I have done!
trueProb    = 0*c(0:ballsToDraw);

##################
#P(Draw 0 Black) #
##################
# Then we draw: {R,R,R,R}
# We are choosing "0" black out of 4.
trueProb[1] = choose(ballsToDraw,0)*(totalRed/totalBalls)*((totalRed-1)/(totalBalls-1))*((totalRed-2)/(totalBalls-2))*((totalRed-3)/(totalBalls-3));
#change#

##################
#P(Draw 1 Black) #
##################
# The we either draw: {B,R,R,R}, {R,B,R,R}, {R,R,B,R}, {R,R,R,B}
# Each of these outcomes has the same probability (since order doesn't matter)
# P(Draw 1 Black) = choose(4,1) P(R,R,R,B) 
trueProb[2] = choose(ballsToDraw,1)*(totalRed/totalBalls)*((totalRed-1)/(totalBalls-1))*((totalRed-2)/(totalBalls-2))*(totalBlack/(totalBalls-3))
#change#

##################
#P(Draw 2 Black) #
##################
# More combinations, let's write them all out
# 1st Black on 1st Pick: {B,B,R,R}, {B,R,B,R}, {B,R,R,B}
# 1st Black on 2nd Pick: {R,B,B,R}, {R,B,R,B}
# 1st Black on 3rd Pick: {R,R,B,B}
# Each of these outcomes has the same probability (since order doesn't matter)
# P(Draw 2 Black) = choose(4,2)*P(R,R,B,B)
trueProb[3] = choose(ballsToDraw,2)*(totalRed/totalBalls)*((totalRed-1)/(totalBalls-1))*(totalBlack/(totalBalls-2))*((totalBlack-1)/(totalBalls-3));
#change#

##################
#P(Draw 3 Black) #
##################
# Let's write them all out as before
# 1st Black on 1st Pick: {B,B,B,R}, {B,R,B,B}
# 1st Black on 2nd Pick: {R,B,B,B}
# Each of these outcomes has the same probability (since order doesn't matter)
# P(Draw 3 Black) = choose(4,3)*P(B,B,B,R)
trueProb[4] = choose(ballsToDraw,3)*(totalBlack/totalBalls)*((totalBlack-1)/(totalBalls-1))*((totalBlack-2)/(totalBalls-2))*(totalRed/(totalBalls-3));
#change#

##################
#P(Draw 4 Black) #
##################
# Then we draw: {B,B,B,B}
# We are choosing all 4 black out of 4.
trueProb[5] = choose(ballsToDraw,4)*(totalBlack/totalBalls)*((totalBlack-1)/(totalBalls-1))*((totalBlack-2)/(totalBalls-2))*((totalBlack-3)/(totalBalls-3));
#change#

############################################################
# Plot and Compare the Experimental and True Probabilities #
############################################################
df <- data.frame(legend = rep(c("sampling proportions","true probabilities"),each=ballsToDraw+1),
                 x = rep(c(0:ballsToDraw),2), 
                 y = c(numBlackDrawnTally/numTrials,trueProb))

library(ggplot2)
p <- ggplot(df, aes(x=x, y=y, group=legend)) +
  geom_line(aes(color=legend)) +
  geom_point(aes(color=legend)) + 
  xlab("Number of Black Balls Drawn") + ylab("Probability") + 
  ggtitle(paste("Probability of Drawing Balls from an Urn:\n \ # Trials = ", numTrials))

print(p)
