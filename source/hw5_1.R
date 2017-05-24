#########################################################################################
###########  ISYE6783         ######
###########  DL  Ajay DSouza   #####
###########   HW 5            ######
#########################################################################################

library(leaps)
library(faraway)
library(MASS)
library(car)
library(reshape2)
library(ggplot2)

#--------------------------------------------
# Setup
#
#--------------------------------------------

#cleanup
rm(list = ls())

# set rounding digits globally
options(digits = 6)

set.seed(100)

setwd(
  "C:/wk/odrive/Amazon Cloud Drive/ajays_stuff/georgia_tech_ms/isye_math6783_financial_data_analysis/hw5"
)

sink("hw5_script_output.txt", append = FALSE, split = TRUE)
sink()


sink("hw5_script_output.txt", append = TRUE, split = TRUE)
cat('\nHW5\n')
sink()

# Read the ascii data file
data <-
  read.csv(file = "w_logret_3automanu.csv", header = FALSE, sep =
             ",")


# N data size
n <- nrow(data)

# set column names
colnames(data) <- c('Toyota', 'Ford', 'GM')

#attach data
detach(data)
attach(data)

# ------------------------------------------
# Data Analysis
# ------------------------------------------
# 1 scatter plot
par(mfrow = c(1, 1))
scatterplotMatrix( ~ GM + Ford + Toyota, main = "")
dev.copy(png,
         filename = "hw5_g_pairs.png",
         width = 1000,
         height = 700)
dev.off()

# correlation
sink("hw5_script_output.txt", append = TRUE, split = TRUE)
cat(paste("\n\n\nCorrelation:\n"))
print(cor(data))
sink()


sink("hw5_script_output.txt", append = TRUE, split = TRUE)
cat(paste("\n\n\nCorrelation With poly(Toyota,2):\n"))
print( cor(cbind(poly(Toyota,2),Ford,GM)))
sink()

# 2 Histogram, KDE
par(mfrow = c(3, 1), oma = c(0, 0, 2, 0))
den <- density(data$Toyota)
plot(den$x, den$y, xlab = "Toyota", ylab = "Density")

den <- density(data$Ford)
plot(den$x, den$y, xlab = "Ford", ylab = "Density")

den <- density(data$GM)
plot(den$x, den$y, xlab = "GM", ylab = "Density")

#title("Density(KDE) Plot - Daily Log Returns", outer = TRUE)

dev.copy(png,
         filename = "hw5_g_kde.png",
         width = 1000,
         height = 700)
dev.off()



# 3 QQplot with normal distribution
# qqplot with T distribution
tqd <- pt((seq(n) - .5) / n, df = 2.4)

par(mfrow = c(3, 2), oma = c(0, 0, 2, 0))
qqnorm(data$Toyota,
       datax = TRUE,
       ylab = "Toyota",
       main = "QQNorm")
qqplot(data$Toyota, tqd, xlab = "Toyota", main = "QQ-TDist")

qqnorm(data$Ford,
       datax = TRUE,
       ylab = "Ford",
       main = "QQNorm")
qqplot(data$Ford, tqd, xlab = "Ford", main = "QQ-TDist")

qqnorm(data$GM,
       datax = TRUE,
       ylab = "GM",
       main = "QQNorm")
qqplot(data$GM, tqd, xlab = "GM", main = "QQ-TDist")

#title("QQPlot\n Normal , T-Distribution \n Daily Log Returns", outer = TRUE)

dev.copy(png,
         filename = "hw5_g_qq.png",
         width = 1000,
         height = 700)
dev.off()


#------------------------------------------------------
# Linear Regression - Model 1 - Analysis
# lm (GM~Ford+Toyota)
#
#------------------------------------------------------

for (mdel in c(1, 2)) {
  #annova
  if (mdel == 1) {
    llfit <- lm(data = data, GM ~ Ford + Toyota)
  }
  else if (mdel == 2) {
    lofit <- llfit
    llfit <- lm(GM ~ Ford + poly(Toyota, 2))
  }
  
  anlfit <- anova(llfit)
  
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(paste(
    "\n\n\nANOVA for the linear regression model :",
    llfit$call[2],
    ":\n"
  ))
  print(anlfit)
  sink()
  
  #summary
  smlfit <- summary(llfit)
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(paste(
    "\n\n\nSummary for the linear regression model :",
    llfit$call[2],
    ":\n"
  ))
  print(smlfit)
  sink()
  

  
  # Anova comparison of two models and F
  if (mdel == 1) {
    llfit2 <- lm(data = data, GM ~ Ford)
    acomp <- anova(llfit2, llfit)
    
  } else if (mdel == 2) {
    acomp <- anova(lofit,llfit)
  }
  
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(paste("\n\n\nComparing Models by ANOVA:\n"))
  print(acomp)
  sink()
  
  

  # Model Selection
  if (mdel == 1) {
    subsets = regsubsets(data$GM ~ .,
                         data = as.data.frame(cbind(data$Ford, data$Toyota)), nbest =
                           1)
  } else if (mdel == 2) {
    subsets = regsubsets(data$GM ~ .,
                         data = as.data.frame(cbind(data$Ford, poly(data$Toyota, 2))), nbest =
                           1)
  }
  
  b = summary(subsets)
  
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(
    paste(
      "\n\n\nRegression Subsets - Choosing the Best set of Predictors by BIC/Cp:\n"
    )
  )
  print(b)
  sink()
  
  
  
  nms <- length(coef(llfit)) - 1
  
  par(
    mfrow = c(2, 2),
    oma = c(0, 0, 2, 0),
    lab = c(2, 5, 3),
    pch = 19
  )
  plot(
    1:nms,
    b$bic,
    type = "b",
    xlab = "number of variables",
    ylab = "BIC",
    cex = 2.5
  )
  plot(
    1:nms,
    b$cp,
    type = "b",
    xlab = "number of variables",
    ylab = "Cp",
    cex = 2.5
  )
  plot(1:nms,
       b$adjr2,
       type = "b",
       xlab = "number of variables",
       ylab = "adjusted R2")
  #title(paste("Model Selection - \nDaily Log Returns ", llfit$call[2]),
  #      outer = TRUE)
  dev.copy(
    png,
    filename = paste("hw5_m", mdel, "_g_complexity.png", sep = ""),
    width = 1000,
    height = 700
  )
  dev.off()
  
  
  #vif for collenarity
  vf <- vif(lm(llfit))
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(paste("\n\n\nVariance Inflation Factor:\n"))
  print(vf)
  sink()
  
  
 
  #StepAIC
  
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(paste("\n\n\nStep AIC ", llfit$call[2], ":\n"))
  step_lm = stepAIC(llfit)
  cat("\n\n")
  smlm <- summary(lm(step_lm))
  print(smlm)
  sink()
  

  #leaps
  if (mdel == 1) {
    x1 = as.matrix(cbind(Ford, Toyota))
    names_x1 = c("Ford", "Toyota")
  } else if (mdel == 2) {
    x1 = as.matrix(cbind(Ford, poly(Toyota, 2)))
    names_x1 = c("Ford", "Toyota1", "Toyota2")
  }
  
  leaps.fit = leaps(
    y = GM,
    x = x1,
    names = names_x1,
    nbest = 1
  )
  options(digits = 2)
  
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(paste("\n\n\nLeaps - Best Model for each K Predictor by Cp:\n"))
  print(cbind(leaps.fit$which, leaps.fit$Cp))
  sink()
  

  
  # Partial Residual Plots
  par(
    mfrow = c(2, 2),
    oma = c(0, 0, 2, 0),
    lab = c(2, 5, 3),
    pch = 19
  )
  lfit_ford = lm(GM ~ Ford)
  lfit_toyota = lm(GM ~ Toyota)
  crPlot(
    llfit,
    var = "Ford",
    main = "Partial - Ford",
    smooth = F,
    lty = 1,
    lwd = 2,
    col = "black"
  )
  
  if (mdel == 1) {
    crTVar <- 'Toyota'
  } else if (mdel == 2) {
    crTVar <- 'poly(Toyota, 2)'
  }
  crPlot(
    llfit,
    var = crTVar,
    main = paste("Partial-", crTVar),
    smooth = F,
    lty = 1,
    lwd = 2,
    col = "black"
  )
  plot(Ford, GM, main = "Ford Vs GM")
  regLine(lfit_ford,
          col = "red",
          lwd = 2,
          lty = "dashed")
  plot(Toyota, GM, main = "Toyota Vs GM")
  regLine(lfit_toyota,
          col = "red",
          lwd = 2,
          lty = "dashed")
  #title(paste("Partial Residual Plots- \nDaily Log Returns ", llfit$call[2]),
  #      outer = TRUE)
  dev.copy(
    png,
    filename = paste("hw5_m", mdel, "_g_partial_residual.png", sep =
                       ""),
    width = 1000,
    height = 700
  )
  dev.off()
  
  # High Leverage Points
  hvals <- hatvalues(llfit)
  # High leverage points > 2(p+1)/n more than 2wice the average ( 59 of them)
  hLevAv <- (length(coef(llfit)) - 1 + 1) / length(llfit$residuals)
  hValHAvg <- sum(hvals > 2 * hLevAv)
  sIdx <- sort(hvals, decreasing = TRUE, index.return = TRUE)
  
  # Top 5 high leverage points
  tp <- 10
  sink("hw5_script_output.txt",
       append = TRUE,
       split = TRUE)
  cat(paste("\n\n\nTop ", tp, " Data Points with the Highest Leverage:\n"))
  print(cbind(data[head(unique(sIdx$ix), tp), ], leverage = head(unique(sIdx$x), tp)))
  
  cat(paste(
    "\n\n\nThe Correlation between the ",
    hValHAvg,
    " High Leverage Data Points:\n"
  ))
  print(cor(cbind(data[head(unique(sIdx$ix), hValHAvg), ])))
  sink()
  
  
  #plot
  par(
    mfrow = c(2, 1),
    oma = c(0, 0, 2, 0),
    lab = c(2, 5, 3),
    lwd = 1,
    pch = 19
  )
  
  plot(
    hvals,
    ylab = "leverage",
    main = "Leverage of All Points",
    ylim = c(0, .1),
    xlab = "Index"
  )
  abline(h = 2 * (length(coef(llfit)) - 1 + 1) / length(llfit$residuals),
         col = 'red')
  
  gpts <-
    as.data.frame(cbind(head(unique(sIdx$ix), 3), hvals[head(unique(sIdx$ix), 3)]))
  colnames(gpts) <- c('Index', 'Leverage')
  text(gpts$Index, gpts$Leverage, gpts$Index, pos = 2)
  
  plot(
    which(hvals > 2 * (length(coef(
      llfit
    )) - 1 + 1) / length(llfit$residuals)),
    hvals[hvals > 2 * (length(coef(llfit)) - 1 + 1) / length(llfit$residuals)],
    main = paste("Points with Leverage > 2(p+1)/n"),
    ylab = "leverage",
    xlab = "Index"
  )
  abline(h = 2 * hLevAv, col = 'red')
  
  # Label the top 5 Leverage values
  gpts <-
    as.data.frame(cbind(head(unique(sIdx$ix), 5), hvals[head(unique(sIdx$ix), 5)]))
  colnames(gpts) <- c('Index', 'Leverage')
  text(gpts$Index, gpts$Leverage, gpts$Index, pos = 2)
  
  #title(paste("Leverage (Hat Values)- \nDaily Log Returns", llfit$call[2]),
  #      outer = TRUE)
  dev.copy(
    png,
    filename = paste("hw5_m", mdel, "_g_leverage.png", sep = ""),
    width = 1000,
    height = 700
  )
  dev.off()
  

  # RStudent Residuals
  
  # Studentized Residuals
  rst <- rstudent(llfit)
  
  # Label the top 5 Leverage values
  rlpts <-
    as.data.frame(cbind(head(unique(sIdx$ix), 5), hvals[head(unique(sIdx$ix), 5)], rst[head(unique(sIdx$ix), 5)]))
  colnames(rlpts) <- c('Index', 'Leverage', 'rst')
  
  # Cook's Distance
  sqcdis <- sqrt(cooks.distance(llfit))
  
  # Cook's Distance Plots
  sqcdis <- sqrt(cooks.distance(llfit))
  
  # Label the top 5 Leverage values
  cIdx <- sort(sqcdis, decreasing = TRUE, index.return = TRUE)
  
  clpts <-
    as.data.frame(cbind(head(unique(cIdx$ix), 5), hvals[head(unique(cIdx$ix), 5)], rst[head(unique(cIdx$ix), 5)], sqcdis[head(unique(cIdx$ix), 5)]))
  colnames(clpts) <- c('Index', 'Leverage', 'rst', 'Cooks')
  
    
  par(
    mfrow = c(2, 1),
    oma = c(0, 0, 2, 0),
    lab = c(2, 5, 3),
    lwd = 1,
    pch = 19
  )
  
  #plot(rst, ylab = "Studentized residual", main = "Externally Studentized Residuals")
  #plot(residuals(llfit), ylab = "residual", main = "Raw Residuals")
  plot(hvals,
       rst,
       ylab = "Studentized residual",
       xlab = "Leverage",
       main = "Student Residuals Vs Leverage")

  text(rlpts$Leverage, rlpts$rst, rlpts$Index, pos = 3)
  
  plot(sqcdis,
       rst,
       ylab = "Studentized residual",
       xlab = "Square Root Cooks Distance",
       main = "Student Residuals Vs Cooks Distance")
  
  text(clpts$Cooks, clpts$rst, clpts$Index, pos = 3)
  
  
 # title(paste("Studentized Residuals- \nDaily Log Returns ", llfit$call[2]),
 #        outer = TRUE)
  
  dev.copy(
    png,
    filename = paste("hw5_m", mdel, "_g_rstudent_residuals.png", sep =
                       ""),
    width = 1000,
    height = 700
  )
  dev.off()
  
  

  
  
  
  
  # Cooks Distance Plots  
  par(
    mfrow = c(2, 1),
    oma = c(0, 0, 2, 0),
    cex.axis = 1,
    cex.lab = 1,
    lwd = 1,
    pch = 19
  )
  
  plot(
    sqcdis,
    ylab = ("square root Cook's D"),
    cex = 1,
    main = "Sqrt of Cooks Distance",
    ylim = c(0, .6)
  )
  text(clpts$Index,
       clpts$Cooks,
       clpts$Index,
       pos = 2,
       cex = 0.7)
  
  halfnorm(
    sqcdis,
    ylab = ("square root Cook's D"),
    cex = 1,
    main = "Half Normal Plots"
  )
  
  #title(paste(
  #  "Cook's Distance- Daily Log Returns GM~Ford,Toyota",
  #  llfit$call[2]
  #),
  #outer = TRUE)
  dev.copy(
    png,
    filename = paste("hw5_m", mdel, "_g_cooks.png", sep = ""),
    width = 1000,
    height = 700
  )
  dev.off()
  
  
  
  # Check for non constant variance or heteroskedacity
  #
  # Plot of absolute Residuals versus predictors and response
  #
  
  par(
    mfrow = c(3, 2),
    oma = c(0, 0, 2, 0),
    lab = c(2, 5, 3),
    lwd = 1,
    pch = 19
  )
  arst <- abs(rst)
  
  qqnorm(rst, datax = T, main = "Normal QQ Plot - Rst")
  
  den <- density(rst)
  plot(den$x,
       den$y,
       xlab = "RStudent",
       ylab = "Density",
       main = "KDE of RStudent")
  
  plot(Ford,
       rst,
       main = "Ford",
       ylab = "RStudent",
       xlab = "Ford")
  fit2 = loess(rst ~ Ford)
  ordx2 = order(Ford)
  lines(Ford[ordx2], fit2$fitted[ordx2], col = "red", lwd = 2)
  
  plot(Toyota,
       rst,
       main = "Toyota",
       ylab = "RStudent",
       xlab = "Toyota")
  fit3 = loess(rst ~ Toyota)
  ordx2 = order(Toyota)
  lines(Toyota[ordx2], fit3$fitted[ordx2], col = "red", lwd = 2)
  
  plot(llfit$fitted,
       arst,
       xlab = "fitted values",
       ylab = "abs(rstudent)",
       main = "Fitted Values")
  fit4 = loess(arst ~ llfit$fitted)
  ord = order(llfit$fitted)
  lines(llfit$fitted[ord], fit4$fitted[ord], col = "red", lwd = 2)
  

  
 # title(
 #    paste(
 #    "Check for Variance \nResiduals Vs Predictors/Fitted Values \nDaily Log Returns ",
 #    llfit$call[2]
 #   ),
 #  outer = TRUE
 # )
  dev.copy(
    png,
    filename = paste("hw5_m", mdel, "_g_heteroskedacity.png", sep =
                       ""),
    width = 1000,
    height = 700
  )
  dev.off()
  

  # Plot the final fit
  par(
    mfrow = c(2, 2),
    oma = c(0, 0, 2, 0),
    lab = c(2, 5, 3),
    lwd = 1,
    pch = 19
  )
  
  plot(llfit)
  
  dev.copy(
    png,
    filename = paste("hw5_m", mdel, "_g_fit.png", sep = ""),
    width = 1000,
    height = 700
  )
  dev.off()
  
}

#--------------------------------------------
#
# Model Comparison
#
# 10 fold cross validation of the chosen models
#
#--------------------------------------------
# Split data into train and hold out test
#

# Number of folds
nFolds <- 10

#Randomly shuffle the array index
smpIdx <- sample(n)

#Create 10 equally size folds
folds <- cut(seq(1, n), breaks = nFolds, labels = FALSE)

# Store the fold results
fResults <- data.frame(
  model = integer(),
  k = integer(),
  trainN = integer(),
  testN = integer(),
  SSR = double(),
  SSE = double(),
  SST = double(),
  MSE = double(),
  MST = double(),
  R2 = double(),
  R2A = double(),
  MSET = double(),
  corFitTrain = double(),
  corTestFit = double(),
  stringsAsFactors = FALSE
)

for (mdel in c(1:2)) {
  #Perform 10 fold cross validation
  for (k in 1:10) {
    testIndices <- smpIdx[which(folds == k, arr.ind = TRUE)]
    ## pick a random train and test rows
    testData <- data[testIndices,]
    trainData <-  data[-testIndices,]
    
    # perform linear regression
    if (mdel == 1) {
      lfit <- lm(data = trainData, GM ~ Ford + Toyota)
    }
    else if (mdel == 2) {
      # perform linear regression
      lfit <- lm(data = trainData, GM ~ Ford + poly(Toyota, 2))
    }
    
    
    # residual - mean square error on the fitted data
    yTrainBar <- mean(trainData$GM)
    nTrain <- length(trainData$GM)
    
    #SSR
    SSR <- sum((predict(lfit) - yTrainBar) ^ 2)
    
    #SSE and MSE
    SSE <-  sum(lfit$residuals ^ 2)
    MSE <- SSE / df.residual(lfit)
    
    # SST and MST
    SST <- sum((trainData$GM - yTrainBar) ^ 2)
    MST <- SST / (nTrain - 1)
    
    # MSE on the training data
    nTest <- length(testData$GM)
    testPred <- predict.lm(lfit, testData[, c('Ford', 'Toyota')])
    MSET <- sum((testPred - testData$GM) ^ 2) / nTest
    
    
    # Save the results
    fResults[nrow(fResults) + 1,] <- c(
      mdel,
      k,
      nTrain,
      nTest,
      SSR,
      SSE,
      SST,
      MSE,
      MST,
      SSR / SST,
      1 - (MSE / MST),
      MSET,
      cor(lfit$fitted.values, trainData$GM) **
        2,
      cor(testPred, testData$GM) ** 2
    )
  }
}

CV_MSE_1 <-
  sum(fResults[fResults$model == 1, ]$MSET * fResults[fResults$model == 1, ]$testN) /
  n
CV_MSE_2 <-
  sum(fResults[fResults$model == 2, ]$MSET * fResults[fResults$model == 2, ]$testN) /
  n


sink("hw5_script_output.txt", append = TRUE, split = TRUE)
cat(
  paste(
    "\n\n\n\nResults of 10 Fold Cross Valiation: MSE on the Validation Set for Each Model:\n"
  )
)
cat(paste("\nModel :", lofit$call[2]))
cat(paste("\nMSE=", round(CV_MSE_1, 6)))
cat(paste("\n"))
cat(paste("\nModel :", llfit$call[2]))
cat(paste("\nMSE=", round(CV_MSE_2, 6)))
cat(paste("\n"))
sink()

fResults$model <- as.factor(fResults$model)

#melt with method name as pivot
molten <-
  melt(
    fResults[, c('k', 'model', 'MSET')],
    id.vars = c('model', 'k'),
    measure.vars = c('MSET'),
    variable_name = 'series'
  )


klabs <- c(paste(unlist(seq(1:10))))

par(
  mfrow = c(1, 1),
  oma = c(0, 0, 2, 0),
  lab = c(2, 5, 3),
  lwd = 1,
  pch = 19
)

ggplot(molten, aes(x = k, y = value, colour = model)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10, labels = klabs) +
  xlab("K-Fold") +
  ylab("MSE") +
  scale_colour_discrete(
    name  = "Model",
    breaks = c("1", "2"),
    labels = c(lofit$call[2], llfit$call[2])
  ) +
  scale_shape_discrete(
    name  = "Model",
    breaks = c("1", "2"),
    labels = c(lofit$call[2], llfit$call[2])
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

dev.copy(png,
         filename = "hw5_cv_plot.png",
         width = 1000,
         height = 700)
dev.off()


#----------------------------------------------
# END
#----------------------------------------------
