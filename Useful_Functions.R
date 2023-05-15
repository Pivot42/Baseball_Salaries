# nolint start

# THE ANOVA() FUNCTION EXTRACTS THE SAME ANVOA TABLE AS PRESENTED IN CLASS
simple_ANOVA <- function(fit, filename=NULL, digits=4){
form.null <- formula(fit); form.null[[3]] <- 1
fit.null <- lm(form.null, data=fit$model)
aov0 <- anova(fit.null, fit,  test="F")
DF <- c(na.omit(aov0$"Df"), sort(aov0$Res.Df))
SS <- round(c(na.omit(aov0$"Sum of Sq"), sort(aov0$RSS)), digits=digits)
MS <- round(SS/DF, digits=digits); MS <- MS[-3]
F <- round(na.omit(aov0$"F"), digits=digits)
P.Value <- na.omit(aov0$"Pr(>F)")
OUT <- matrix(" ", nrow=3, ncol=5)
OUT[,1:2] <- cbind(DF, SS); OUT[1:2,3] <- MS
OUT[1, 4] <- F; OUT[1, 5] <- P.Value 
OUT <- as.data.frame(OUT)
row.names(OUT) <- c("Model", "Error", "Total")
colnames(OUT) <- c("df", "SS", "MS", "F", "P-Value")
if (!is.null(filename)) write.csv(OUT, file=filename)
return(OUT)
}

# FOR ILLUSTRATIVE PURPOSES, I WROTE A FUNCTION FOR ALL POSSIBLE REGRESSIONS,
# WHICH IS ONLY APPLICABLE FOR SMALL p
all_possible_regressions <- function(fit.whole) {
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
form <- apply(regMat, 1, function(x) as.character(paste(c(paste(yname," ~ 1", sep = ""), regressors[x]), collapse = " + ")))
allModelsList <- apply(regMat, 1, function(x) as.formula(paste(c(paste(yname," ~ 1", sep = ""), regressors[x]), collapse = " + ")))
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

# nolint end