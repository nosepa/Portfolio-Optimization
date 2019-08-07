# Robin Roddi
# Final Project 2 Stock Prices and Portfolio Optimization
# Ban 7010

#install.packages("quantmod")
library("quantmod")
#install.packages("rmutil")
library("rmutil")
#install.packages("lpSolve")
library("lpSolve")
library("sandwich")
library("lmtest")

# Problem 1: 
# Pick 5 stocks. Stocks that are on the Dow Jones will work best for this project, but are not
# necessary. Ensure its not a "no-name" stock (that is, pick so called "Blue Chips"). For each
# stock that you selected, download then using the getSymbols command. The remainder of
# the problems require you to solve the problem description for each stock you chose.

getSymbols("JNJ")   # Johnson & Johnson
getSymbols("HAL")   # Halliburton
getSymbols("GLW")   # Corning 
getSymbols("KMB")   # Kimberly-Clark
getSymbols("CAT")   # Caterpillar 

# Problem 2:
# Extract each column of the data frame to a vector (use the as.vector() function for each
# column). Store them in the variables open, high, low, close, and volume, respectively.

# Johnson & Johnson
JNJ.open             <- as.vector(JNJ$JNJ.Open)
JNJ.high             <- as.vector(JNJ$JNJ.High)
JNJ.low              <- as.vector(JNJ$JNJ.Low)
JNJ.close            <- as.vector(JNJ$JNJ.Close)
JNJ.vol              <- as.vector(JNJ$JNJ.Volume)

# Halliburton
HAL.open             <- as.vector(HAL$HAL.Open)
HAL.high             <- as.vector(HAL$HAL.High)
HAL.low              <- as.vector(HAL$HAL.Low)
HAL.close            <- as.vector(HAL$HAL.Close)
HAL.vol              <- as.vector(HAL$HAL.Volume)

# Corning
GLW.open             <- as.vector(GLW$GLW.Open)
GLW.high             <- as.vector(GLW$GLW.High)
GLW.low              <- as.vector(GLW$GLW.Low)
GLW.close            <- as.vector(GLW$GLW.Close)
GLW.vol              <- as.vector(GLW$GLW.Volume)


# Kimberly-Clark
KMB.open             <- as.vector(KMB$KMB.Open)
KMB.high             <- as.vector(KMB$KMB.High)
KMB.low              <- as.vector(KMB$KMB.Low)
KMB.close            <- as.vector(KMB$KMB.Close)
KMB.vol              <- as.vector(KMB$KMB.Volume)

# Caterpillar 
CAT.open             <- as.vector(CAT$CAT.Open)
CAT.high             <- as.vector(CAT$CAT.High)
CAT.low              <- as.vector(CAT$CAT.Low)
CAT.close            <- as.vector(CAT$CAT.Close)
CAT.vol              <- as.vector(CAT$CAT.Volume)

# Problem 3
# For the open, high, low, and close, convert each data set to a data set of returns (the current
# day's price - the previous day's price). Store each result in the variable names open_r,
# high_r, low_r, close_r, respectively.

# Neil's function for getting stock return and putting them into vectors.

getReturn = function(v){
  ret = c()
  for(i in 1:(length(v) - 1)){
    ret = c(ret, (v[i + 1] - v[i]))
  }
  return(ret)
}

# Johnson & Johnson
JNJ.open_r         <- getReturn(JNJ.open) 
JNJ.high_r         <- getReturn(JNJ.high)
JNJ.low_r          <- getReturn(JNJ.low)
JNJ.close_r        <- getReturn(JNJ.close)

# Halliburton
HAL.open_r          <- getReturn(HAL.open) 
HAL.high_r          <- getReturn(HAL.high)
HAL.low_r           <- getReturn(HAL.low)
HAL.close_r         <- getReturn(HAL.close)

# Corning      
GLW.open_r           <- getReturn(GLW.open) 
GLW.high_r           <- getReturn(GLW.high)
GLW.low_r            <- getReturn(GLW.low)
GLW.close_r          <- getReturn(GLW.close)

# Kimberly-Clark      
KMB.open_r          <- getReturn(KMB.open) 
KMB.high_r          <- getReturn(KMB.high)
KMB.low_r           <- getReturn(KMB.low)
KMB.close_r         <- getReturn(KMB.close)

# Caterpillar
CAT.open_r           <- getReturn(CAT.open) 
CAT.high_r           <- getReturn(CAT.high)
CAT.low_r            <- getReturn(CAT.low)
CAT.close_r          <- getReturn(CAT.close)

# Problem 4:
# For each vector created above, create two new vectors: one for the "before" returns and
# another for the "current" returns. Ensure they all have the same length. If they do not, then
# remove the data from the beginning of the vector, NOT THE END.

###############################################################################################

# Johnson & Johnson
JNJ.open_r_before  <- JNJ.open_r[-length(JNJ.open_r)]
JNJ.open_r_curr    <- JNJ.open_r[-1]
JNJ.high_r_before  <- JNJ.high_r[-length(JNJ.high_r)]
JNJ.high_r_curr    <- JNJ.high_r[-1]
JNJ.low_r_before   <- JNJ.low_r[-length(JNJ.low_r)]
JNJ.low_r_curr     <- JNJ.low_r[-1]
JNJ.close_r_before <- JNJ.close_r[-length(JNJ.close_r)]
JNJ.close_r_curr   <- JNJ.close_r[-1]

# Halliburton
HAL.open_r_before   <- HAL.open_r[-length(HAL.open_r)]
HAL.open_r_curr     <- HAL.open_r[-1]
HAL.high_r_before   <- HAL.high_r[-length(HAL.high_r)]
HAL.high_r_curr     <- HAL.high_r[-1]
HAL.low_r_before    <- HAL.low_r[-length(HAL.low_r)]
HAL.low_r_curr      <- HAL.low_r[-1]
HAL.close_r_before  <- HAL.close_r[-length(HAL.close_r)]
HAL.close_r_curr    <- HAL.close_r[-1]

# Corning
GLW.open_r_before    <- GLW.open_r[-length(GLW.open_r)]
GLW.open_r_curr      <- GLW.open_r[-1]
GLW.high_r_before    <- GLW.high_r[-length(GLW.high_r)]
GLW.high_r_curr      <- GLW.high_r[-1]
GLW.low_r_before     <- GLW.low_r[-length(GLW.low_r)]
GLW.low_r_curr       <- GLW.low_r[-1]
GLW.close_r_before   <- GLW.close_r[-length(GLW.close_r)]
GLW.close_r_curr     <- GLW.close_r[-1]

# Kimberly-Clark
KMB.open_r_before   <- KMB.open_r[-length(KMB.open_r)]
KMB.open_r_curr     <- KMB.open_r[-1]
KMB.high_r_before   <- KMB.high_r[-length(KMB.high_r)]
KMB.high_r_curr     <- KMB.high_r[-1]
KMB.low_r_before    <- KMB.low_r[-length(KMB.low_r)]
KMB.low_r_curr      <- KMB.low_r[-1]
KMB.close_r_before  <- KMB.close_r[-length(KMB.close_r)]
KMB.close_r_curr    <- KMB.close_r[-1]

# Caterpillar
CAT.open_r_before  <- CAT.open_r[-length(CAT.open_r)]
CAT.open_r_curr    <- CAT.open_r[-1]
CAT.high_r_before  <- CAT.high_r[-length(CAT.high_r)]
CAT.high_r_curr    <- CAT.high_r[-1]
CAT.low_r_before   <- CAT.low_r[-length(CAT.low_r)]
CAT.low_r_curr     <- CAT.low_r[-1]
CAT.close_r_before <- CAT.close_r[-length(CAT.close_r)]
CAT.close_r_curr   <- CAT.close_r[-1]

# Problem 5: 
# Refine each vector so that only the last 400 days are in the data set. (Hint: Each vector is
# by default ordered based on time. Time runs from 1 to the length of the vector. That is, the
# newest stock price is put at the end of the vector).
################################################################################################

# Johnson & Johnson

dfJNJ <- data.frame(open = JNJ.open, high = JNJ.high, low= JNJ.low, close = JNJ.close, vol = JNJ.vol)

dfJNJ2 <- data.frame(open_r_before = JNJ.open_r_before, open_r_curr = JNJ.open_r_curr, high_r_before = JNJ.high_r_before, 
           high_r_curr = JNJ.high_r_curr, low_r_before = JNJ.low_r_before, low_r_curr = JNJ.low_r_curr, 
           close_r_before = JNJ.close_r_before, close_r_curr = JNJ.close_r_curr)

dfJNJ400d <- data.frame (tail(dfJNJ,400), tail(dfJNJ2,400)) 

str(dfJNJ400d)

# Halliburton

dfHAL <- data.frame(open = HAL.open, high = HAL.high, low= HAL.low, close = HAL.close, vol = HAL.vol)

dfHAL2 <- data.frame(open_r_before = HAL.open_r_before, open_r_curr = HAL.open_r_curr, high_r_before = HAL.high_r_before, 
                   high_r_curr = HAL.high_r_curr, low_r_before = HAL.low_r_before, low_r_curr = HAL.low_r_curr, 
                   close_r_before = HAL.close_r_before, close_r_curr = HAL.close_r_curr)

dfHAL400d <- data.frame (tail(dfHAL,400), tail(dfHAL2,400)) 

str(dfHAL400d)

# Corning

dfGLW <- data.frame(open = GLW.open, high = GLW.high, low= GLW.low, close = GLW.close, vol = GLW.vol)

dfGLW2 <- data.frame(open_r_before = GLW.open_r_before, open_r_curr = GLW.open_r_curr, high_r_before = GLW.high_r_before, 
                   high_r_curr = GLW.high_r_curr, low_r_before = GLW.low_r_before, low_r_curr = GLW.low_r_curr, 
                   close_r_before = GLW.close_r_before, close_r_curr = GLW.close_r_curr)

dfGLW400d <- data.frame (tail(dfGLW,400), tail(dfGLW2,400)) 

str(dfGLW400d)

# Kimberly-Clark

dfKMB <- data.frame(open = KMB.open, high = KMB.high, low= KMB.low, close = KMB.close, vol = KMB.vol)

dfKMB2 <- data.frame(open_r_before = KMB.open_r_before, open_r_curr = KMB.open_r_curr, high_r_before = KMB.high_r_before, 
                   high_r_curr = KMB.high_r_curr, low_r_before = KMB.low_r_before, low_r_curr = KMB.low_r_curr, 
                   close_r_before = KMB.close_r_before, close_r_curr = KMB.close_r_curr)

dfKMB400d <- data.frame (tail(dfKMB,400), tail(dfKMB2,400)) 

str(dfKMB400d)

# Caterpillar

dfCAT <- data.frame(open = CAT.open, high = CAT.high, low= CAT.low, close = CAT.close, vol = CAT.vol)

dfCAT2 <- data.frame(open_r_before = CAT.open_r_before, open_r_curr = CAT.open_r_curr, high_r_before = CAT.high_r_before, 
                   high_r_curr = CAT.high_r_curr, low_r_before = CAT.low_r_before, low_r_curr = CAT.low_r_curr, 
                   close_r_before = CAT.close_r_before, close_r_curr = CAT.close_r_curr)

dfCAT400d <- data.frame (tail(dfCAT,400), tail(dfCAT2,400)) 

str(dfCAT400d)

# Problem 6: 
# Construct an empirical model for the closing returns of the "current" days (vector) using
# the other vectors as the independent variable. Take a similar process that we took the other
# day in class to ensure that you have a "good" specification.
############################################################################################
dev.off()
# JNJ
hist(dfJNJ400d$close_r_curr, freq = FALSE, breaks = 20)

par(mar=rep(2,4))

plot(dfJNJ400d)

colnames(dfJNJ400d)

# I've chosen the following model as my best fit 
JNJ_Model <- lm(close_r_curr ~ open_r_curr + high_r_curr + low_r_before + low_r_curr + close_r_before, data = dfJNJ400d)

summary(JNJ_Model)
str(dfJNJ400d)

# Halliburton

hist(dfHAL400d$close_r_curr, freq = FALSE, breaks = 20)

plot(dfHAL400d)


# I've chosen the following model as my best fit
HAL_Model <- lm(close_r_curr ~ low + close + vol + open_r_curr + high_r_before + high_r_curr + low_r_before, data = dfHAL400d)

summary(HAL_Model)



# Corning

hist(dfGLW400d$close_r_curr, freq = FALSE, breaks = 20)

plot(dfGLW400d)

# I've chosen the following model as my best fit
GLW_Model <- lm(close_r_curr ~ open_r_before + high_r_before + high_r_curr + low_r_curr, data = dfGLW400d)

summary(GLW_Model)

#  Kimberly-Clark

hist(dfKMB400d$close_r_curr, freq = FALSE, breaks = 20)

plot(dfKMB400d)

#I've chosen the following model as my best fit
KMB_Model <- lm(close_r_curr ~ open + close + open_r_curr + high_r_before, data = dfKMB400d)

summary(KMB_Model)

# Caterpillar

hist(dfCAT400d$close_r_curr, freq =  FALSE, breaks = 30)

plot(dfCAT400d)

# I've chosen the following model as my best fit
CAT_Model <- lm(close_r_curr ~ open + high + low + open_r_curr, data = dfCAT400d)

summary(CAT_Model)

# Problem 7: 
# Test the model for heteroscdasticity. If it has it, then fix it
# (refer to the Hint Guide to figure out how to do this).
###################################################################################################
# Johnson & Johnson
bptest(JNJ_Model)
# Since p-value < 0.05, we reject the NULL Hypothesis, there is evidence of heteroscedasticity
# fix it as follows
coeftest(JNJ_Model, vcov = vcovHC (JNJ_Model, "HC1"))
summary(JNJ_Model)
##The standard error for low_r_curr  is  0.0276244. After running White's correction, it changed to 0.0276244 which is a significant jump. 
#The same for close_r_before 0.0516853 to  0.07155000, low_r_before and high_r_curr also saw slightly higher jump the estimates stayed the same. 
#
# Halliburton
bptest(HAL_Model)
# Since p-value > 0.05, we reject the NULL Hypothesis, there is evidence of heteroscedasticity 

# fixing it using the coeftest as follow
summary(HAL_Model)
coeftest(HAL_Model, vcov = vcovHC (HAL_Model, "HC1"))
# The standard error for vol is 5.869e-09. After running White's correction, 
# it changed to 8.9706e-09 which is a significant jump.
# the others had slight bumps and the estimates stayed the same.

# Corning
bptest(GLW_Model)
# Since p-value < 0.05, we reject the NULL Hypothesis, there is evidence of heteroscedasticity

# fixing it using the coeftest as follow
summary(GLW_Model)
coeftest(GLW_Model, vcov = vcovHC (GLW_Model, "HC1"))
# The standard error in high_r_curr was 0.0505690  after running the coeftest it changed to 0.11746311 and open_r_before was 0.0475264 
#and after running the coeftest it changed to 0.08250499 all of them had a slight up tick after running White's correction.
# Estimates stayed the same.

# Kimberly-Clark
bptest(KMB_Model)
# Since p-value < 0.05, we reject the NULL Hypothesis, there is evidence of heteroscedasticity 
summary(KMB_Model)
coeftest(KMB_Model, vcov = vcovHC (KMB_Model, "HC1"))
# The standard error in open_r_curr was 0.01823 after running the coeftest it changed to 0.031317 all others had a slight change after running White's correction. 
# Caterpillar
bptest(CAT_Model)
# Since p-value < 0.05, we reject the NULL Hypothesis, there is evidence of heteroscedasticity
# fixing it using the coeftest as follow
summary(CAT_Model)
coeftest(CAT_Model, vcov = vcovHC (CAT_Model, "HC1"))
# The standard error for low is 0.04381. After running White's correction, 
# it changed to 0.075635 a slight but significant jump. 
# for open 0.06424 to 0.089874 also a slight jump. 
# the others had slight bumps and the estimates stayed the same.


# Problem 8: 
# Which variables are significant? Explain which variables have a
# positive or negative effect on the dependent variable

summary(JNJ_Model)
 
# open_r_curr  negative affect on the dependent variable. significant to the model
# high_r_curr  positive affect on the dependent variable. significant to the model
# low_r_before positive affect on the dependent variable. significant to the model
# low_r_curr   positive affect on the dependent variable. significant to the model
# close_r_before negative affect on the dependent variable. significant to the model

summary(HAL_Model)
# low            negative affect on the dependent variable.  significant to the model  
# close          positive affect on the dependent variable.  significant to the model
# vol            negative affect on the dependent variable.  significant to the model
# open_r_curr    negative affect on the dependent variable.  significant to the model
# high_r_before  positive affect on the dependent variable.  significant to the model
# high_r_curr    positive affect on the dependent variable.  significant to the model
# low_r_before   negative affect on the dependent variable.  significant to the model

summary(GLW_Model)

# open_r_before positive affect on the dependent variable.  significant to the model
# high_r_before negative affect on the dependent variable.  significant to the model
# high_r_curr   positive affect on the dependent variable.  significant to the model
# low_r_curr    positive affect on the dependent variable.  significant to the model

summary(KMB_Model)
# open           negative affect on the dependent variable.  significant to the model
# close          positive affect on the dependent variable.  significant to the model
# open_r_curr    positive affect on the dependent variable.  significant to the model
# high_r_before  negative affect on the dependent variable.  least significant to the model 

summary(CAT_Model)
# open           negative affect on the dependent variable.  least significant to the model
# high           positive affect on the dependent variable.  significant to the model
# low            positive affect on the dependent variable.  significant to the model  
# open_r_curr    positive affect on the dependent variable.  significant to the model 

# Problem 9:
# Write a function that will conduct a single iteration of the simulation. 
## 
## testing which distro fits our model

# 9 (b) ks.test for Normal Dist 
#C_Mean <-mean(dfJNJ400d$open_r_curr); C_SD <- sd(dfJNJ400d$open_r_curr)

#C_Norm <- rnorm (100000, C_Mean , C_SD)
#Norm_KStest <- ks.test(dfJNJ400d$open_r_curr, C_Norm)

# ks.test for Laplace

#?rmutil

#C_laplace <- rlaplace (100000, C_Mean, C_SD)
#lap_KStest <- ks.test(dfJNJ400d$open_r_curr,C_laplace)

#sample(rlaplace (100000, C_Mean, C_SD), 1)

# if else statement to determine which distribution the data fits
#{
#if(lap_KStest$p.value < Norm_KStest$p.value)
  #Put sample laplace variable here sample
#  {
#  sample(rlaplace (100000, C_Mean, C_SD), 1)
#} 
#else 
  #Put sample norm variable here
 # {
#  sample(rnorm (100000, C_Mean, C_SD), 1)
#}
#}

# 9 (b) Here i'm using part of Neil's function but modified to work for me. 
N_Sample <- function(df, independ){
  if(regexpr("curr", independ)[1] > 0) # If the Independent Variable is Current data
  {
    # Then test if the data from the independent variable fits the Normal or Laplace
    
    # ks Test for Normal Distribution
    C_Norm <- rnorm(100000, mean(df[, independ]), sd(df[, independ]))
    Norm_KStest = ks.test(df[, independ], C_Norm)
    
    # ks Test for LaPlace Distribution
    C_laplace <- rlaplace(100000, mean(df[, independ]), sd(df[, independ]))
    lap_KStest <- ks.test(df[, independ], C_laplace)
    
    if(Norm_KStest$p.value >= lap_KStest$p.value)
    {
      #print(Norm_KStest) #Print Norm_KStest results
      # If the p-value from the Normal Distribution was larger than the Laplace Distribution
      return(rnorm(1, mean(df[, independ]), sd(df[, independ])))
    }else{
      # print(lap_KStest) #Print lap_KStest results
      # Else the p-value from the LaPlace Distribution was larger than the Normal Distribution
      return(rlaplace(1, mean(df[, independ]), sd(df[, independ])))
    }
    
  }else # Otherwise it's before data, sample from that variable in the data frame
  {
    return(sample(df[,independ], 1))
  }
}

# problem 9 (a) Loop Function 30 day
runSimulation = function(df, dep, independ = c()){
  samp = df[0, independ] #Create an empty dataframe, copying from the original dataframe
  # Loop 30 Iterations
  for(i in 1:30){
    # Loop through our Independent Variables and get samples
    for(v in independ){
      samp[i,v] = N_Sample(df, v)
    }
  }
  # Rebuild our Models Formula
  formula = ""
  for(v in independ){
    if(nchar(formula) > 0)# If the formula has been started, use the paste function, and '+' separator
    {
      formula = paste(formula, v, sep = " + ")
    }else{ # Otherwise just add the Column name to the formula
      formula = v
    }
  }
  formula = paste(dep, formula, sep = " ~ ") # New Linear Model Formula using the variables
  print(formula) # Print Formula
  lm = lm(formula = formula, data = df) # Run the Linear Regression Model
  prediction = predict(lm, samp)
  #print(prediction)
  samp = data.frame(samp, "prediction" = prediction)
  # sigma(lm) will return the Residual Standard Error from our Model
  sError = rnorm(1, mean = 0, sd = sigma(lm))
  #print(sError) Print Sample Error
  prediction = prediction + sError
  samp = data.frame(samp, dep = prediction)
  names(samp)[names(samp) == "dep"] = dep # I'm naming the
  #prediction + error the same as the dependent variable
  #print(samp)
  newlm = lm(formula = formula, data = samp)
  #print(summary(newlm)) # Print Summary of Regression using new data
  return(samp[,dep]) # Return our Predicted dependent Variable
}

# 10) Now write the core of the simulation:

################################################################################################
# N = Number of times I choose to run the simulation, default = 100
coreSimulation = function (df, dep, independ = c(), N = 100){
  results = c() # Created an empty vector called results
  for(j in 1:N){
    # Run the Simulation Function, and combine the results from prior runs
    results = c(results,
                runSimulation(df, dep, independ))
  }
  # The final vector size should be 30 * N
  return(results)
}
# 10 Simulation 

# Johnson & Johnson Simulation
simJNJ <- coreSimulation (dfJNJ400d, "close_r_curr", c("open_r_curr" , "high_r_curr" , "low_r_before" , "low_r_curr" , "close_r_before"), N = 10)

# Halliburton Simulation
simHAL <- coreSimulation(dfHAL400d, "close_r_curr", c("low", "close" , "vol" , "open_r_curr" , "high_r_before" , "high_r_curr", "low_r_before"), N = 10)

# Kimberly-Clark Simulation
simKMB  <- coreSimulation(dfKMB400d, "close_r_curr", c("open" , "close" , "open_r_curr" , "high_r_before" ), N = 10)

# Caterpiller Simulation
simCAT   <- coreSimulation(dfCAT400d, "close_r_curr", c("open" , "high" , "low" , "open_r_curr"), N = 10)

# Corning Simulation
simGLW   <- coreSimulation(dfGLW400d, "close_r_curr", c("open_r_before" , "high_r_before" , "high_r_curr" , "low_r_curr"), N = 10)

# problme 11:
# When the simulation is complete, find the average return and store it.
############################################################################################
# Average for Johnson & Johnson Simulation Returns
meanSimJNJ = mean(simJNJ)
meanSimJNJ

# Average for Halliburtion Simulation Returns
meanSimHAL = mean(simHAL)
meanSimHAL

# Average for Kimberly-Clark Simulation Returns
meanSimKMB = mean(simKMB)
meanSimKMB

# Average for Corning Simulation Returns
meanSimGLW <- mean(simGLW)
meanSimGLW

# Average for Caterpiller Simulation Returns
meanSimCAT <- mean(simCAT)
meanSimCAT

# Problem 12:
# When the simulation is complete for ALL stocks, take each vector, store it in a data frame.
# Then, run the cov function to compute the variance-covariance matrix. Store this matrix in
# a variable.
#################################################################################################
simdf <- data.frame(simJNJ, simHAL, simKMB, simGLW, simCAT)
simdfCov <- cov(simdf)  

# Problem 13:
# Plug this model into R using the lpSolve package. Solve it using this package and find the
# optimal allocation of investments.
simMeans <- c(meanSimJNJ, meanSimHAL, meanSimKMB, meanSimGLW, meanSimCAT)
simdfCov

# coefficients objective function Neil helped this function 
obj = c()
for(i in 1:nrow(simdfCov)){
  total = 0
  for(j in 1:ncol(simdfCov)){
    total = total + (simMeans[i] * simMeans[j] * simdfCov[i,j])
  }
  obj = c(obj, total)
}
# Constraint Matrix
A <- matrix(
  c(meanSimJNJ, meanSimHAL, meanSimKMB, meanSimGLW, meanSimCAT,
    1, 1, 1, 1, 1),
  nrow = 2, byrow = TRUE
)

# Constraints RHS (Right Hand Side)
constraints <- c(2000, 20000)

# Constraint Directions
constraintDirs <- c(">=", "<=")

solution = lp(
  direction = "min",
  objective.in = obj,
  const.mat = A,
  const.dir = constraintDirs,
  const.rhs = constraints,
  all.int = TRUE,
  compute.sens = TRUE
)

solution

solution$solution


# According to the linear programming solution,
# My initial investment should be as follows 
# Johnson & Johnson = $0 
# Halliburton  =  $16,193
# Kimberly-Clark  =  $0
# Corning  =  $3,806
# Caterpiller =  $0
# 

# solution
#Success: the objective function is 2071.712 
# solution$solution
#[1]     0 16193     0  3806     0
 
