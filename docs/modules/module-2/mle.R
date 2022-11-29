############################################
# Problem 1: Maximum Likelihood Estimation #
############################################

#Likelihood Function (Probability of the Data)
like <- function(a,N) (1/a)^(N); 

#Data
n        = 10;   #<- Change this line
trueA    = 2.5;  #<- Change this line
data     = runif(n, min = 0, max = trueA);

#Maximize the Likelihood
lowerB = max(data); #<-Change this line
upperB = 10;
opt <- optimize(like, lower = lowerB, upper = upperB,
          N = n, maximum=TRUE)

pMax           = opt$maximum;
maxLikelihood  = opt$objective;

#Plot the Resutls
x = seq(lowerB,upperB,0.01);
plot(x,like(x,n),type='l',main="Likelihood of the Uniform Data",
     ylab="L(a)",xlab="a")
points(pMax,maxLikelihood,type='p',col='red')

############################################
# Problem 2: Maximum Likelihood Estimation #
############################################

#Read in the data file
dataFrame = read.csv(file = "testDataExp.csv");
data = dataFrame$x; #<-Only get the data values

like    <- function(lambda,data) lambda #< Change this line #lambda^(length(data))*exp(-lambda*sum(data))

#Maximize the Likelihood
lowerB = 1; #<- Change this value
upperB = 2; #<- Change this value
opt <- optimize(like, lower = lowerB, upper = upperB, data=data, maximum=TRUE)

pMax           = opt$maximum;
maxLikelihood  = opt$objective;

x = seq(lowerB,upperB,0.1);
plot(x,like(x,data),type='l',main="Likelihood of the Exp Data",
     ylab="L(Lambda)",xlab="Lambda")
points(pMax,maxLikelihood,type='p',col='red')
