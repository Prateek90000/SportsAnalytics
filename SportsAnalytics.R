rm(list = ls())

library("dplyr")
library("magrittr")
library("ggplot2")

# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("ggplot2")

SportsData = read.csv("baseball.csv")
trainsubset =subset(SportsData,Year<=2007)
testsubset =subset(SportsData,Year>2007)
Sports96to2001 = subset(SportsData,Year>=1996 & Year<=2001)

ggplot(data = Sports96to2001, aes(x = W, y = Team)) + theme_bw() +
  scale_color_manual(values = c("grey", "red3")) +
  geom_vline(xintercept = c(85.0,95.0), col="green2",linetype = "longdash") +
  geom_point(aes(color = factor(Playoffs)), pch = 16, size = 3.0)

ggplot(data = Sports96to2001)

ggplot(data = Sports96to2001, aes(x = RA, y = W)) + theme_bw() +  
  scale_color_manual(values = c("grey", "red3")) + 
  geom_hline(yintercept= c(85.0, 95.0), col = "green2", linetype = "longdash") +
  geom_point(aes(color = factor(Playoffs)), alpha = 0.5,pch = 16, size = 3.0)

wins_Regression <- lm(W ~ RA, data = trainsubset)
summary(wins_Regression)

ggplot(data = Sports96to2001, aes(x = RS, y = W)) + theme_bw() +  
  scale_color_manual(values = c("grey", "red3")) + 
  geom_hline(yintercept= c(85.0, 95.0), col = "green2", linetype = "longdash") +
  geom_point(aes(color = factor(Playoffs)), alpha = 0.5,pch = 16, size = 3.0)

Sports96to2001$difference = Sports96to2001$RS - Sports96to2001$RA
ggplot(data = Sports96to2001, aes(x = difference, y = W)) + theme_bw() +  
  scale_color_manual(values = c("grey", "red3")) + 
  geom_hline(yintercept= c(85.0, 95.0), col = "green2", linetype = "longdash") +
  geom_point(aes(color = factor(Playoffs)), alpha = 0.5,pch = 16, size = 3.0)

wins_RegressionOld <- lm(W ~ RA, data = trainsubset)
summary(wins_RegressionOld)

wins_Regression <- lm(W ~ differnce, data = trainsubset)
summary(wins_Regression)

View(Sports96to2001)

RunScoreModel =lm(RS ~ OBP+SLG+BA, data = trainsubset)
summary(RunScoreModel)


RunScoreModel2 =lm(RS ~ OBP+SLG, data = trainsubset)
summary(RunScoreModel2)

trainsubset$PredictionOfRunScored = -811.80 + 2816.77 *trainsubset$OBP + 1532.58*trainsubset$SLG

printStats <- function (m, with.cx=TRUE) {
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(m)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
  attributes(p) <- NULL
  
  fml <- as.character(formula(m))
  cx <- summary(m)$coeff
  stars <- rep(" ", nrow(cx))
  stars[cx[,4] <= 0.1] <- "."
  stars[cx[,4] <= 0.05] <- "*"
  stars[cx[,4] <= 0.01] <- "**"
  stars[cx[,4] <= 0.001] <- "***"
  cat("MODEL        : ", sprintf("%s", paste(fml[c(2,1,3)], sep=" ", collapse=" ")), "\n", sep="")
  cat("SUMMARY STATS: ")
  cat("R^2 = ",sprintf("%6.4f",summary(m)$r.squared), 
      "  (adj. = ", sprintf("%6.4f",summary(m)$adj.r.squared), ")", sep="")
  cat("\n")
  cat("               ")
  cat("F-stats: ", sprintf("%.3f",f[1]), " on ", f[2], " and ", f[3], " DF,  p-value: ", p, "\n", sep="")
  if( with.cx ) {
    cat("\n")
    print(cbind(format(cx[,c(1,2,4)], scientific=TRUE, justify="right", digits=5), Signif=stars), 
          quote=FALSE, print.gap=3)
  }
}

printStats.cpt <- function (m, with.cx=TRUE) {
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(m)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
  attributes(p) <- NULL
  
  fml <- as.character(formula(m))
  cx <- summary(m)$coeff
  stars <- rep(" ", nrow(cx))
  stars[cx[,4] <= 0.1] <- "."
  stars[cx[,4] <= 0.05] <- "*"
  stars[cx[,4] <= 0.01] <- "**"
  stars[cx[,4] <= 0.001] <- "***"
  cat("MODEL : ", sprintf("%s", paste(fml[c(2,1,3)], sep=" ", collapse=" ")), "\n", sep="")
  cat("      : ")
  cat("adj. R^2 = ", sprintf("%6.4f",summary(m)$adj.r.squared), 
      " /  F-stats: ", sprintf("%.3f",f[1]), " on ", f[2],",", f[3], " Df,  p-value: ", p, "\n", sep="")
  if( with.cx ) {
    print(cbind(format(cx[,c(1,2,4)], scientific=TRUE, justify="right", digits=5), Signif=stars), 
          quote=FALSE, print.gap=3)
    cat("\n")
  }
}
RunScoreModel2 =lm(RS ~ OBP+SLG, data = trainsubset)
printStats (RunScoreModel2)

trainsubset$PredictionofRunsScored = -811.70 + 2816.77 *trainsubset$OBP + 1532.58 *trainsubset$SLG
trainsubset$error = abs(trainsubset$PredictionofRunsScored - trainsubset$RS)
View(trainsubset)

testsubset$PredictionofRunsScored = -811.70 + 2816.77 *testsubset$OBP + 1532.58 *testsubset$SLG
testsubset$error = abs(testsubset$PredictionofRunsScored - testsubset$RS)

trainsubset$difference = trainsubset$RS - trainsubset$RA
Wins_Regression <- lm(W ~ difference, data = trainsubset)
printStats(Wins_Regression)

trainsubset$WinsPrediction =  8.0894e+01 + 1.2128e-01*trainsubset$difference
trainsubset$Winserror = abs(trainsubset$W - trainsubset$WinsPrediction)
View(trainsubset)

testsubset$difference = testsubset$RS - testsubset$RA
Wins_Regression <- lm(W ~ difference, data = testsubset)
printStats(Wins_Regression)

testsubset$WinsPrediction =  8.0894e+01 + 1.2128e-01*testsubset$difference
testsubset$Winserror = abs(testsubset$W - testsubset$WinsPrediction)
View(testsubset)
