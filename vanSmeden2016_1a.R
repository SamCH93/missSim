## Simulation study 1a by van Smeeden et al. (2016)
## -----------------------------------------------------------------------------
## this replication is only done to verify that the DGM is correctly implemented
library(SimDesign)
library(logistf) # Firth logistic regressions
library(detectseparation) # check separation
Generate <- function(condition, fixed_objects = NULL) {
    Attach(condition)
    attempts <- 0
    logitprevalence <- qlogis(p = 0.5)
    nrange <- c(30, 300)
    ## generate data until specified EPV is obtained
    while (TRUE) {
        attempts <- attempts + 1
        x <- rnorm(n = nrange[2], mean = 0, sd = 1)
        linPred <- logitprevalence + x*logOR
        y <- rbinom(n = nrange[2], size = 1, prob = plogis(linPred))
        ncases <- cumsum(y)
        inds <- seq_along(ncases)
        if (any(ncases == EPV & inds > nrange[1])) {
            ind <- seq(1, which(ncases == EPV & inds >= nrange[1])[1])
            dat <- data.frame(x = x[ind], y = y[ind])
            return(dat)
        } else {
            if (attempts > 1000) {
                stop("no data set could be generated")
            }
        }
    }
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
    glmfit <- glm(y ~ x, data = dat, family = "binomial")
    CI90GLM <- unname(confint.default(glmfit, level = 0.9)[2,])
    firthfit <- logistf(y ~ x, data = dat, alpha = 0.1)
    CI90Firth <- unname(c(firthfit$ci.lower[2], firthfit$ci.upper[2]))

    ## ## check complete separation as defined in the Appendix of van Smeden (2016)
    ## ## this takes really long
    ## refits <- t(sapply(X = seq(1, 30), FUN = function(i) {
    ##     summary(glm(y ~ x, data = dat, family = "binomial",
    ##                 control = list(maxit = i)))$coefficients[2,1:2]
    ## }))
    ## sescaled <- refits[,2]/refits[1,2]
    ## separation <- var(sescaled) > 20

    ## check separation with detectseparation package
    ## (faster but not exactly the same)
    glmcheck <- glm(y ~ x, data = dat, family = "binomial",
                    method = "detect_separation")
    separation <- glmcheck$outcome

    ret <- data.frame(estGLM = unname(glmfit$coefficients[2]),
                      estFirth = unname(firthfit$coefficients[2]),
                      CI90lowerGLM = CI90GLM[1],
                      CI90lowerFirth = CI90Firth[1],
                      CI90upperGLM = CI90GLM[2],
                      CI90upperFirth = CI90Firth[2],
                      separation = separation)
    return(ret)
}

Summarise <- function(condition, results, fixed_objects = NULL) {
    Attach(condition)
    Attach(results)
    ## no MCSE for the moment
    indnosep <- which(separation == FALSE)
    ret <- data.frame(biasGLM = bias(estimate = estGLM[indnosep],
                                     parameter = logOR),
                      biasFirth = bias(estimate = estFirth, parameter = logOR),
                      coverageGLM = ECR(CIs = cbind(CI90lowerGLM, CI90upperGLM)[indnosep,],
                                        parameter = logOR),
                      coverageFirth = ECR(CIs = cbind(CI90lowerFirth, CI90upperFirth),
                                          parameter = logOR),
                      mseGLM = RMSE(estimate = estGLM[indnosep],
                                    parameter = logOR, MSE = TRUE),
                      mseFirth = RMSE(estimate = estFirth, parameter = logOR,
                                      MSE = TRUE),
                      separation = mean(separation))
    return(ret)
}

## run simulation
set.seed(1423)
nsim <- 10000
Design <- createDesign(EPV = seq(15, 150, 15),
                       logOR = log(c(0.25, 0.5, 1, 2, 4)))
## res <- runSimulation(design = Design,
##                      replications = nsim,
##                      generate = Generate,
##                      analyse = Analyse,
##                      summarise = Summarise,
##                      parallel = TRUE,
##                      save_results = TRUE,
##                      store_results = TRUE,
##                      ncores = 11)
## results <- do.call("rbind", lapply(X = list.files(path = "study1a/"), FUN = function(file) {
##     dat <- readRDS(file = paste0("study1a/", file))
##     data.frame(dat$condition, dat$results)
## }))
res <- reSummarise(summarise = Summarise, dir = "study1a/")

## look at separation (almost no separation) aligns with reported result:
## "maximum percentage separated data sets in a single simulation scenario of 0.3%"
print(res[,c("EPV", "logOR", "separation")], n = 50)

## recreate Figure 2
library(ggplot2)
library(ggpubr)
res$OR <- factor(exp(res$logOR), ordered = TRUE)
biasGLM <- ggplot(data = res,
                  aes(x = EPV, y = biasGLM, color = OR, shape = OR)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
    geom_line(alpha = 0.5) +
    geom_point() +
    coord_cartesian(ylim = c(-0.3, 0.3)) +
    scale_x_continuous(breaks = seq(15, 150, 15)) +
    scale_y_continuous(breaks = round(seq(-0.3, 0.3, 0.1), 1)) +
    labs(x = "Events per variable", y = bquote("Bias" (beta[1]^"ML")),
         color = bquote(exp(beta[1])), shape = bquote(exp(beta[1]))) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
biasFirth <- ggplot(data = res, aes(x = EPV, y = biasFirth, color = OR, shape = OR)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
    geom_line(alpha = 0.5) +
    geom_point() +
    coord_cartesian(ylim = c(-0.3, 0.3)) +
    scale_x_continuous(breaks = seq(15, 150, 15)) +
    scale_y_continuous(breaks = round(seq(-0.3, 0.3, 0.1), 1)) +
    labs(x = "Events per variable", y = bquote("Bias" (beta[1]^"F")),
         color = bquote(exp(beta[1])), shape = bquote(exp(beta[1]))) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
biasplots <- ggarrange(biasGLM, biasFirth, ncol = 2, align = "hv", common.legend = TRUE)
covGLM <- ggplot(data = res,
                 aes(x = EPV, y = coverageGLM, color = OR, shape = OR)) +
    geom_hline(yintercept = 0.9, lty = 2, alpha = 0.3) +
    geom_line(alpha = 0.5, show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    coord_cartesian(ylim = c(0.85, 0.95)) +
    scale_x_continuous(breaks = seq(15, 150, 15)) +
    scale_y_continuous(breaks = seq(0.85, 0.95, 0.02), labels = scales::percent) +
    labs(x = "Events per variable", y = "Coverage") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
covFirth <- ggplot(data = res, aes(x = EPV, y = coverageFirth, color = OR, shape = OR)) +
    geom_hline(yintercept = 0.9, lty = 2, alpha = 0.3) +
    geom_line(alpha = 0.5, show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    coord_cartesian(ylim = c(0.85, 0.95)) +
    scale_x_continuous(breaks = seq(15, 150, 15)) +
    scale_y_continuous(breaks = seq(0.85, 0.95, 0.02), labels = scales::percent) +
    labs(x = "Events per variable", y = "Coverage") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
covplots <- ggarrange(covGLM, covFirth, ncol = 2, align = "hv", common.legend = TRUE)
mseGLM <- ggplot(data = res,
                 aes(x = EPV, y = mseGLM, color = OR, shape = OR)) +
    geom_line(alpha = 0.5, show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    coord_cartesian(ylim = c(0, 0.8)) +
    scale_x_continuous(breaks = seq(15, 150, 15)) +
    scale_y_continuous(breaks = seq(0, 0.8, 0.2)) +
    labs(x = "Events per variable", y = bquote("MSE" (beta[1]^"ML"))) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
mseFirth <- ggplot(data = res, aes(x = EPV, y = mseFirth, color = OR, shape = OR)) +
    geom_line(alpha = 0.5, show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    coord_cartesian(ylim = c(0, 0.8)) +
    scale_x_continuous(breaks = seq(15, 150, 15)) +
    scale_y_continuous(breaks = seq(0, 0.8, 0.2)) +
    labs(x = "Events per variable", y = bquote("MSE" (beta[1]^"F"))) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
mseplots <- ggarrange(mseGLM, mseFirth, ncol = 2, align = "hv", common.legend = TRUE)
ggarrange(biasplots, covplots, mseplots,
          ncol = 1, align = "hv", common.legend = TRUE)

