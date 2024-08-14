library(dplyr)

## load intermediate data from Carter et al. (2019), data is too big too big for
## GitHub, run R script "3-resultsFramework.R" from
## https://github.com/nicebread/meta-showdown to produce it
## setwd("case-studies/Carter2019/")
load("res.wide.red.RData")
str(res.wide.red)

## need to merge "H0.reject.pos" colum of the "pcurve.evidence" method with the
## "pcurve" method as "pcurve.evidence" is used for assessment of hypothesis
## testing performance of method "pcurve" but for nothing else
pcurveevidencedat <- res.wide.red |>
    filter(method == "pcurve.evidence") |>
    rename(H0.reject.pos.pcurve.evidence = H0.reject.pos) |>
    select(id, k, delta, qrpEnv, censor, tau, H0.reject.pos.pcurve.evidence)

dat <- left_join(res.wide.red , pcurveevidencedat) |>
    mutate(H0.reject.pos = ifelse(method == "pcurve",
                                  H0.reject.pos.pcurve.evidence,
                                  H0.reject.pos)) |>
    ## select relevant columns and methods, recode them as in paper
    select(id, method, k, delta, qrpEnv, censor, tau, b0_estimate, b0_conf.low,
           b0_conf.high, H0.reject, H0.reject.pos, H0.reject.wrongSign) |>
    filter(method %in% c("reMA", "TF", "PETPEESE", "pcurve", "puniform", "3PSM",
                         "WAAP-WLS")) |>
	mutate(method = factor(method,
                           levels = c("reMA", "TF", "WAAP-WLS", "pcurve",
                                      "puniform", "PETPEESE", "3PSM"),
                           labels = c("RE", "TF", "WAAP-WLS", "p-curve",
                                      "p-uniform", "PET-PEESE", "3PSM")),
           qrpEnv = factor(qrpEnv, levels = c("none", "med", "high"),
                           ordered = TRUE),
           censor = factor(censor, levels = c("none", "med", "high"),
                           ordered = TRUE))

## for some reason ,data contains non-convergent repetitions from p-curve and
## p-uniform as NAs, but not from TF and 3PSM methods --> manually add missing
## repetitions for TF and 3PSM
mtds <- c("TF", "3PSM")
unique(dat$method)
ids <- unique(dat[dat$method == "WAAP-WLS",]$id) # WAAP-WLS always converged
dat2 <- do.call("rbind", lapply(X = mtds, FUN = function(mtd) {
    tmp <- dat[dat$method == mtd,]
    missingid <- setdiff(ids, tmp$id)
    tmpdf <- subset(dat, method == "WAAP-WLS" & id %in% missingid)
    tmpdf$method <- mtd
    tmpdf[,8:13] <- NA # set estimates and H0 reject to NA
    return(tmpdf)
}))
datClean <- rbind(dat, dat2)


## check that the same overall convergence rates as Carter et al. (2019)
datClean |>
    mutate(validEstimate = !is.na(b0_estimate)) |>
    group_by(method) |>
    summarise(nConverged = mean(validEstimate)*1000)
## overall convergence rates from code of Carter et al. (2019)
## # A tibble: 1 × 7
##      RE    TF `WAAP-WLS` `p-curve` `p-uniform` `PET-PEESE` `3PSM`
##   <dbl> <dbl>      <dbl>     <dbl>       <dbl>       <dbl>  <dbl>
## 1 1000.  949.       1000      992.        992.        1000   915.

## check that same computed as missingness as in Table 2 in Carter et al. (2019)
## supplementary material (https://osf.io/vmsxh)
datClean |>
    mutate(validEstimate = !is.na(b0_estimate)) |>
    ## first row in table
    group_by(method) |>
    filter(k == 10, delta == 0, qrpEnv == "none", censor == "none", tau == 0) |>
    summarise(percConverged = mean(validEstimate)*100)
## k    δ       QRP     PB      τ       TF      p-curve     p-uniform   3PSM
## 10   0.0     none    none    0.0     99%     23%         23%         74%


## save data
carter2019 <- datClean
save(object = carter2019, file = "carter2019.rda")
