## R script to extract relevant summary data from simulation study on
## publication bias adjustment methods for meta-analysis by Carter et al. (2019)

## packages
library(dplyr)
library(ggplot2)

## load intermediate data "res.wide.red.RData" from Carter et al. (2019), data
## is too big too big for GitHub, run R script "3-resultsFramework.R" from
## https://github.com/nicebread/meta-showdown to produce it
## setwd("case-study/")
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


## reproduce some figures from paper
fig3 <- function(data, pubbias = "none") {
    fig3dat <- data |>
        filter(delta %in% c(0, 0.5)) |>
        group_by(method, k, delta, tau, qrpEnv, censor) |>
        summarise(RR = mean(H0.reject.pos, na.rm = TRUE),
                  mcse = sqrt(RR*(1 - RR)/sum(!is.na(H0.reject.pos))),
                  missrate = mean(is.na(H0.reject.pos)),
                  missperc = paste0(round(missrate*100, 0), "%"),
                  misswarn = case_when(missrate <= 0.25 ~ " ",
                                       missrate > 0.25 & missrate <= 0.5 ~ "*",
                                       missrate > 0.5 & missrate <= 0.75 ~ "#",
                                       TRUE ~ "!")) |>
        mutate(RRtype = ifelse(delta == 0, "T1E", "Power"),
               RRtype = factor(RRtype, levels = c("T1E", "Power")))

    ggplot(data = filter(fig3dat, censor == pubbias, RRtype == "T1E"),
           aes(x = factor(k), y = RR, ymin = RR - mcse, ymax = RR + mcse,
               color = RRtype, fill = RRtype, shape = qrpEnv)) +
        facet_grid(tau ~ method) +
        geom_pointrange(position = position_dodge(width = 0.75), size = 2,
                        fatten = 1) +
        geom_pointrange(data = filter(fig3dat, censor == pubbias, RRtype == "Power"),
                        position = position_dodge(width = 0.75), size = 2,
                        alpha = 0.9, fatten = 1) +
        ## missingness warning labels
        geom_text(aes(y = RR - mcse - 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
        geom_text(data = filter(fig3dat, censor == pubbias, delta == 0.5),
                  aes(y = RR + mcse + 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
         ## missingness rates when larger than 0%
        geom_text(data = filter(fig3dat, censor == pubbias, missperc != "0%",
                                delta == 0),
                  aes(y = - 0.1, label = missperc), size = 2.5,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
        geom_text(data = filter(fig3dat, censor == pubbias, missperc != "0%",
                                delta == 0.5),
                  aes(y = 1.125, label = missperc), size = 2.5,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
        scale_y_continuous(limits = c(-0.125, 1.175),
                           breaks = c(0.05, 0.5, 0.8, 1),
                           labels = scales::percent) +
        labs(x = "Studies k", y = "Rejection rate", color = "True effect",
             shape = "QRPs", fill = "True effect",
             title = paste("Publication bias =", pubbias)) +
        scale_shape_manual(values = c(21, 22, 24)) +
        scale_fill_manual(values = c("skyblue", "black")) +
        scale_color_manual(values = c("skyblue", "black")) +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
}
fig3(data = carter2019, pubbias = "none") # Figure 3a
fig3(data = carter2019, pubbias = "med") # Figure 3b
fig3(data = carter2019, pubbias = "high") # Figure 3c

fig4 <- function(data, pubbias = "none") {
    fig4dat <- data |>
        filter(delta %in% c(0, 0.5)) |>
        group_by(method, k, delta, tau, qrpEnv, censor) |>
        summarise(lower = quantile(b0_estimate, probs = 0.025, na.rm = TRUE),
                  est = mean(b0_estimate, na.rm = TRUE),
                  upper = quantile(b0_estimate, probs = 0.975, na.rm = TRUE),
                  missrate = mean(is.na(b0_estimate)),
                  missperc = paste0(round(missrate*100, 0), "%"),
                  misswarn = case_when(missrate <= 0.25 ~ " ",
                                       missrate > 0.25 & missrate <= 0.5 ~ "*",
                                       missrate > 0.5 & missrate <= 0.75 ~ "#",
                                       TRUE ~ "!"))
    plt <- ggplot(data = filter(fig4dat, censor == pubbias, delta == 0),
                  aes(x = factor(k), y = est, ymin = lower, ymax = upper,
                      fill = factor(delta), color = factor(delta),
                      shape = qrpEnv)) +
        facet_grid(tau ~ method, labeller = label_bquote(rows = tau == .(tau))) +
        geom_hline(yintercept = 0, color = "skyblue", alpha = 0.5) +
        geom_hline(yintercept = 0.5, color = "black", alpha = 0.5) +
        geom_pointrange(position = position_dodge(width = 0.5), size = 2,
                        fatten = 1, alpha = 0.95) +
        geom_pointrange(data =  filter(fig4dat, censor == pubbias, delta == 0.5),
                        position = position_dodge(width = 0.6), size = 2,
                        fatten = 1, alpha = 0.65) +
        geom_text(aes(y = lower - 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        geom_text(data = filter(fig4dat, censor == pubbias, delta == 0.5),
                  aes(y = upper + 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        ## report missingness rates when larger than 0%
        geom_text(data = filter(fig4dat, censor == pubbias, missperc != "0%",
                                delta == 0),
                  aes(y = - 0.45, label = missperc), size = 3,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        geom_text(data = filter(fig4dat, censor == pubbias, missperc != "0%",
                                delta == 0.5), aes(y = 1.4, label = missperc),
                  size = 3, position = position_dodge(width = 0.6),
                  show.legend = FALSE) +
        scale_y_continuous(breaks = c(-0.6, 0, 0.5, 1)) +
        scale_shape_manual(values = c(21, 22, 24)) +
        scale_fill_manual(values = c("skyblue", "black")) +
        scale_color_manual(values = c("skyblue", "black")) +
        labs(x = "Studies k", y = "Effect estimate", color = "True effect",
             shape = "QRPs", fill = "True effect",
             title = paste("Publication bias =", pubbias)) +
        coord_flip(ylim = c(-0.5, 1.5)) +
        theme_bw() +
        theme(legend.position = "bottom", panel.grid = element_blank())
    return(plt)
}
fig4(data = carter2019, pubbias = "none") # Figure 4a
fig4(data = carter2019, pubbias = "med") # Figure 4b
fig4(data = carter2019, pubbias = "high") # Figure 4c
