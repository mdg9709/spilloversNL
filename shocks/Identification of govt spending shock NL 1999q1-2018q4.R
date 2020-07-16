#################################################################
# Identify Dutch government spending shock through VAR(4) model #
#################################################################

# Netherlands, 1999q1-2018q4

# Import Dutch fiscal data
library(readxl)
fNL <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/Fiscal data NL 1980-2019Q4 v3.xlsx", 
                    sheet = "1999q1-2018q4")
View(fNL)

# Create variables for real government spending & net tax revenues, and take logs
library(dplyr)
fNL1 <- fNL %>%
  mutate(trNL = tor - (thn + sin), gNL = gcn + gin) %>%
  mutate(rtrNL = trNL/(P/100), rgNL = gNL/(P/100), ryNL = Y/(P/100)) %>%
  mutate(lrtrNL = log(rtrNL), lrgNL = log(rgNL), lryNL = log(ryNL), lPNL = log(P))
names(fNL1)[17] <- "intNL"

# Create time series
library(zoo)
names(fNL1)[1] <- "quarterNL"
quarterNL <- fNL1$quarterNL
fNL2 <- fNL1 %>%
  mutate(quartersNL = as.yearqtr(quarterNL))   ## declare that the "quarter" column contains dates.
fNL3 <- subset(fNL2, select = -c(quarterNL))
fNL4 <- fNL3[, c(26, 1:25)]
fNLts <- ts(fNL4, start=c(1999, 1), end=c(2018, 4), frequency=4)   ## create time series.
View(fNLts)

# Create log variables and the interest rate variable
lrtrNL <- fNLts[, "lrtrNL"]
lrgNL <- fNLts[, "lrgNL"]
lryNL <- fNLts[, "lryNL"]
lPNL <- fNLts[, "lPNL"]
intNL <- fNLts[, "intNL"]

# KPSS tests for level and trend stationarity
library(tseries)
kpss.test(lrtrNL)
kpss.test(lrtrNL, null = "T")
kpss.test(lrgNL)
kpss.test(lrgNL, null = "T")
kpss.test(lryNL)
kpss.test(lryNL, null = "T")
kpss.test(lPNL)
kpss.test(lPNL, null = "T")
kpss.test(intNL)
kpss.test(intNL, null = "T")

# Create dataframe and time series for VAR model
df.NL <- cbind(lrtrNL, lrgNL, lryNL, lPNL, intNL)
df.NL1 <- data.frame(df.NL)
ts.NL <- ts(df.NL1, start=c(1999, 1), end=c(2018, 4), frequency=4)
q <- time(ts.NL)
ex <- I(q)^2

# Optimal lag length
library(vars)
VARselect(ts.NL, lag.max = 6, type = "both", exogen = ex)   

# VAR model, which contains four lags, a constant, a trend and a dummy (see Alloza et al.)
varNL1 <- VAR(ts.NL, p = 4, type = "both", exogen = ex)
summary(varNL1)

# Eigenvalues: stability of VAR process (stable if values < 1)
roots(varNL1, modulus = TRUE)   
# Test for serially correlated errors
serial.test(varNL1, lags.pt = 16)   
# Normality, multivariate skewness and kurtosis test
normality.test(varNL1)

# Identify the structural government spending (d.lrg) shock
resNL1 <- residuals(varNL1)
resNL2 <- ts(resNL1, start=c(1999, 1), end=c(2018, 4), frequency=4) 
# Obtain the residuals of d.lrg and d.lP - see Alloza et al. (2019)
res.lrgNL <- subset(resNL2, TRUE, lrgNL, drop = FALSE)
res.lPNL <- subset(resNL2, TRUE, lPNL, drop = FALSE)
# Compute structural d.lrg shock - see Alloza et al. (2019), page 4-5, Eq. 3 and footnote 11
shock.lrgNL <- res.lrgNL + 0.5*res.lPNL
shock.lrgNL

# Add structural shock vector to time series
fNLts1 <- cbind(fNLts, shock.lrgNL)
colnames(fNLts1) <- c("quartersNL", "toe", "coe", "inp", "thn", "sin", "gin", "gcn", "tor", 
                      "tin", "dtx", "sct", "gcf", "acq", "P", "Y", "r", "trNL", "gNL", "rtrNL", 
                      "rgNL", "ryNL", "lrtrNL", "lrgNL", "lryNL", "lPNL", "shockNL")
