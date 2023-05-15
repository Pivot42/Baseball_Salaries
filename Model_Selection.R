
####################################################
# MODEL SELECTION WITH REAL ESTATE DATA
####################################################

# ====================
# BRING IN THE DATA
# ===================

# PREPARE THE DATA
real <- read.csv(file="real.csv", header=T); real

# THE NULL MODEL WITH INTERCEPT ONLY
fit.null <- lm(sale.price ~ 1, data=real); summary(fit.null)

# MODEL WITH ALL FIRST-ORDER INTERACTION
fit.whole <- lm(sale.price ~ land.value + improvement + area, 
	data=real,  x=TRUE, y=TRUE)
summary(fit.whole)
n <- NROW(real); n
cbind(AIC=AIC(fit.whole, k=2), BIC=AIC(fit.whole, k=log(n)))


# =================================
# STEPWISE PROCEDURES
# =================================

# BACKWARD DELETION
# ------------------
fit.backward <-  step(fit.whole, direction="backward", k = 2)  
# AIC IS USED HERE BY DEFAULT; FOR BIC, SET k=log(n).
fit.backward$anova # DISPLAY DETAILED STEPS 
summary(fit.backward) # THE FINAL MODEL FOUND WITH BACKWARD DELETION

# DO IT STEP-BY-STEP
fit <- fit.whole; summary(fit)
fit <- update(fit, .~.-land.value); summary(fit)


# FORWARD ADDITION
# ------------------
# TO USE FORWARD ADDITION OR STEPWISE, IT IS IMPORTANT TO SPECIFY THE MODEL SEARCH RANGE CORRECTLY. 
fit.forward <-  step(fit.null, scope=list(lower=fit.null, upper=fit.whole), 
	direction="forward", k = 2)  
fit.forward$anova  
summary(fit.forward) # THE FINAL MODEL FOUND WITH FORWARD ADDITION

# STEPWISE SELECTION
# -------------------
fit.stepwise <-  step(fit.null, scope=list(lower=fit.null, upper=fit.whole), direction="both", k = 2)  
fit.stepwise$anova  
summary(fit.stepwise)



# ======================================
# ALL SUBSETS REGRESSION - SELECTION
# ======================================


# USING PACKAGE {bestglm}  - THE BEST SO FAR
# -------------------------------------------
# THIS PACKAGE ACTUALLY BUILDS ON THE {leaps} PACKAGE 
# install.packages("bestglm")
require(bestglm)
# formula0 <- sale.price ~ land.value + improvement + area -1		# NO QUOTATION MARKS & NO INTERCEPT
formula0 <- sale.price ~ . -property -1   					# EQUIVALENT TO THE ABOVE FORMULA. 
# NOTE THAT THE DOT . IN FORMULA IS A SHORT WAY FOR INCLUDING ALL VARIABLES
dat <- real											# TO MAKE CODES GENERIC

y <- dat[,names(dat)==as.character(formula0)[2]]
X <- model.matrix(as.formula(formula0),dat)
dat.tmp <- as.data.frame(cbind(X, y))
result.bestBIC <- bestglm(Xy=dat.tmp, IC="BIC", intercept=TRUE); 		# THE intercept=TRUE MAKES SURE THAT AN INTERCEPT IS ALWAYS INCLUDED. 
# names(result.bestBIC)									# SEE THE DETAILS
fit.bestBIC <- result.bestBIC$BestModel
summary(fit.bestBIC)



# USING PACKAGE {leaps}
# -----------------------
# install.packages("leaps")
library(leaps)
fits.B1 <- regsubsets(sale.price ~ land.value + improvement + area, data=real, nbest=1, nvmax=3, method="exhaustive")
summary(fits.B1, all.best=TRUE)
summary(fits.B1, all.best=FALSE)
plot(fits.B1, scale="bic")  # BLACK(OR GRAY)=SELECTED; WHITE=NOT SELECTED





# #########################################################
# APPENDIX: A SELF-WRITTEN BEST SUBSET SELECTION FUNCTION
# #########################################################

# FOR ILLUSTRATIVE PURPOSES, I WROTE A FUNCTION FOR ALL POSSIBLE REGRESSIONS,
# WHICH IS ONLY APPLICABLE FOR SMALL p

all.possible.regressions <- function(fit.whole) {
if (NROW(fit.whole$x) == 0) {
dat0 <- fit.whole$model
fit.whole <- lm(formula=formula(fit.whole), data=dat0,  x=TRUE, y=TRUE)
}
yname <- names(fit.whole$model)[1] 
dat <- data.frame(fit.whole$y, fit.whole$x[, -1]) 
colnames(dat)[1] <- yname 
n <- NROW(dat); p <- NCOL(dat)-1 	
regressors <- colnames(dat)[-1]; 
lst <- rep(list(c(TRUE, FALSE)), p)
regMat <- expand.grid(lst);  
names(regMat) <- regressors
form <- apply(regMat, 1, function(x)
as.character(paste(c(paste(yname," ~ 1", sep=""), regressors[x]), collapse=" + ")))
allModelsList <- apply(regMat, 1, function(x) 
as.formula(paste(c(paste(yname," ~ 1", sep = ""), regressors[x]), collapse = " + ")))
allModelsResults <- lapply(allModelsList, function(x, data) lm(formula = x, data = data), data = dat)
n.models <- length(allModelsResults)

extract <- function(fit) {
df.sse <- fit$df.residual
p <- n - df.sse - 1
sigma <- summary(fit)$sigma
MSE <- sigma^2
R2 <- summary(fit)$r.squared
#R2.adj <- summary(fit)$adj.r.squared
sse <- MSE * df.sse
aic <- n * log(sse) + 2 * p			# IGNORING IRRELEVANT CONSTANTS
bic <- n * log(sse) + log(n) * p
out <- data.frame(df.sse = df.sse, p = p, SSE = sse, MSE = MSE, R2 = R2, AIC = aic, BIC = bic)
return(out)
}

result <- lapply(allModelsResults, extract)
result <- as.data.frame(matrix(unlist(result), nrow = n.models, byrow = TRUE))
result <- cbind(form, result)
rownames(result) <- NULL
colnames(result) <- c("model", "df.error", "p", "SSE", "MSE", "R2", "AIC", "BIC")
result <- as.data.frame(result)
return(result)
}

fit.whole <- lm(sale.price ~ land.value + improvement + area, data=real)
#fit.whole <- lm(sale.price ~ . - property, data=real)
fit.all.subset <- all.possible.regressions(fit.whole)
fit.all.subset
write.csv(fit.all.subset, "all-subsets.csv", row.names=FALSE)




####################################
# ANOTHER EXAMPLE - THE state DATA
####################################
# BACKWARD DELETION WITH SIGNIFICANCE TESTING

data(state)
# ?state
dat <- data.frame(state.x77,row.names=state.abb,check.names=T)
dat
fit <- lm(Life.Exp~., data=dat); summary(fit)
fit <- update(fit, .~. - Area); summary(fit)
fit <- update(fit, .~. - Illiteracy); summary(fit)





#
