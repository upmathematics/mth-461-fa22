#######################################
# Problem 3: Least Squares Regression #
#######################################

#########################
# Step 1: Make Data Set #
#########################

#Linear Model (What you will attempt to learn)
alpha =  5.00; #<- Change this line
beta  = -0.01; #<- Change this line

#Number of Data Points
N     = 30;   #<- Change this line

#Noise Function
mu    = 0;
sigma = 0.075;
U     = rnorm(N, mu, sigma)

#Generate Data
lowTemp  = 100;
highTemp = 400; 
X        = runif(N,lowTemp,highTemp);
Y        = alpha + beta*X + U;

#Store these values in a Data Frame 
reactionTimes = data.frame(temp=X,time=Y)

#########################################
# Step 2: Perform the Linear Regression #
#########################################

#Do the linear regression
linearModel = lm(time ~ temp, data = reactionTimes)

############################
# Step 3: Plot the Results #
############################

#Look at our Model Results
print(summary(linearModel))

#Plot our Results
plot(reactionTimes$temp,reactionTimes$time,
     ylim=c(0.5,4.5),xlim=c(100,400),
     main="Temperature vs Reaction Time",xlab="Temp (F)",ylab="Time (Hr)")
x = seq(lowTemp,highTemp,1);
y = alpha+beta*x;
lines(x,y,type='l',col='red')

