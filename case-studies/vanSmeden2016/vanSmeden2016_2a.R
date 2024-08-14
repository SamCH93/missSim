## Simulation study 2a by van Smeeden et al. (2016)
## TODO this doesn't work yet!!!
## -----------------------------------------------------------------------------
library(SimDesign)
library(detectseparation)
Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    ## prevalence is not clear from description either
    logitprevalence <- qlogis(p = 0.5)
    ## the range is not clear from the description, I take the same as in 1a
    nrange <- c(60, 600)
    x <- rbinom(n = 1000, size = 1, prob = 0.5)
    linPred <- logitprevalence + x*logOR
    y <- rbinom(n = 1000, size = 1, prob = plogis(linPred))

    ## "keeping the first events and non-events generated up to the required
    ## number of each" (but how many nonevents are required? that's not
    ## clearly specified)
    yevents <- rep(1, EPV)
    ynonevents <- rep(0, EPV)
    xevents <- x[y == 1][1:EPV]
    xnonevents <- x[y == 0][1:EPV]
    dat <- data.frame(x = c(xevents, xnonevents), y = c(yevents, ynonevents))
    return(dat)
}



Analyse <- function(condition, dat, fixed_objects = NULL) {
    ## fit glm to get coefs, CIs, SEs
    glmfit <- glm(y ~ x, data = dat, family = "binomial")
    glmsummary <- summary(glmfit)

    ## ## check complete separation as defined in the Appendix of van Smeden (2016)
    ## refits <- t(sapply(X = seq(1, 30), FUN = function(i) {
    ##     summary(glm(y ~ x, data = dat, family = "binomial",
    ##                 control = list(maxit = i)))$coefficients[2,1:2]
    ## }))
    ## sescaled <- refits[,2]/refits[1,2]
    ## separation <- var(sescaled) > 20

    ## check separation with detectseparation package (faster)
    glmcheck <- glm(y ~ x, data = dat, family = "binomial",
                    method = "detect_separation")
    separation <- glmcheck$outcome

    ## Wald CI
    CI90 <- suppressMessages(unname(confint.default(glmfit, level = 0.9)[2,]))
    ret <- data.frame(coef = glmsummary$coefficients[2,1],
                      se = glmsummary$coefficients[2,2],
                      CI90lower = CI90[1],
                      CI90upper = CI90[2],
                      separation = separation)
    return(ret)
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    Attach(condition)
    Attach(results)
    nsim <- length(separation)
    notsep <- which(separation == FALSE)
    ret <- data.frame(biasNaive = bias(estimate = coef, parameter = logOR),
                      biasComplete = bias(estimate = coef[notsep],
                                          parameter = logOR),
                      coverageNaive = ECR(CIs = cbind(CI90lower, CI90upper),
                                          parameter = logOR),
                      coverageComplete = ECR(CIs = cbind(CI90lower, CI90upper)[notsep,],
                                             parameter = logOR),
                      mseNaive = RMSE(estimate = coef, parameter = logOR,
                                      MSE = TRUE),
                      mseComplete = RMSE(estimate = coef[notsep],
                                         parameter = logOR, MSE = TRUE),
                      separation = mean(separation == TRUE))
    ret$separationMCSE <- sqrt(ret$separation*(1 - ret$separation)/nsim)
    return(ret)
}

set.seed(232)
nsim <- 1000
## these EPVs are reported on p. 4 but differ from the ones shown in Table 3
Design <- createDesign(EPV = seq(6, 30, 2),
                       logOR = log(c(1, 2, 4)))
## res <- runSimulation(design = Design,
##                      replications = nsim,
##                      generate = Generate,
##                      analyse = Analyse,
##                      summarise = Summarise,
##                      store_results = TRUE,
##                      save_results = TRUE,
##                      parallel = TRUE,
##                      ncores = 11)
results <- do.call("rbind", lapply(X = list.files(path = "study2a/"), FUN = function(file) {
    dat <- readRDS(file = paste0("study2a/", file))
    data.frame(dat$condition, dat$results)
}))
res <- reSummarise(summarise = Summarise, dir = "study2a/")


library(ggplot2)
ggplot(data = res, aes(x = EPV, y = separation,
                       color = factor(exp(logOR), ordered = TRUE))) +
    geom_line(alpha = 0.5) +
    geom_pointrange(aes(ymin = separation - separationMCSE,
                        ymax = separation + separationMCSE)) +
    labs(x = "EPV", y = "Separated data sets", color = bquote(exp(beta[1]))) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.2)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

## ggplot(data = res, aes(x = EPV, y = biasNaive,
##                        color = factor(exp(logOR), ordered = TRUE))) +
##     geom_line(alpha = 0.5) +
##     geom_point() +
##     labs(x = "EPV", y = "Bias", color = bquote(exp(beta[1]))) +
##     theme_bw() +
##     theme(panel.grid.minor = element_blank())
