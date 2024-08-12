library(dplyr)

## load intermediate data from Carter et al. (2019), data is too big too big for
## GitHub, run R script "3-resultsFramework.R" from
## https://github.com/nicebread/meta-showdown to produce it
## setwd("Carter2019/")
load("res.wide.red.RData")
str(res.wide.red)
dat <- res.wide.red |>
    ## select relevant columns and methods, recode them as in paper
    select(id, method, k, delta, qrpEnv, censor,  tau, b0_estimate, b0_conf.low,
           b0_conf.high, H0.reject, H0.reject.pos, H0.reject.wrongSign,
           consisZero) |>
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
    tmpdf[,8:14] <- NA # set estimates and H0 reject to NA
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
saveRDS(object = carter2019, file = "carter2019.rda", version = 2)

## replicate some figures from paper
library(ggplot2)

## figure 4a
fig4dat <- carter2019 |>
    filter(delta %in% c(0, 0.5)) |>
    group_by(method, k, delta, tau, qrpEnv, censor) |>
    summarise(lower = quantile(b0_estimate, probs = 0.025, na.rm = TRUE),
              est = mean(b0_estimate, na.rm = TRUE),
              upper = quantile(b0_estimate, probs = 0.975, na.rm = TRUE),
              missrate = mean(is.na(b0_estimate)),
              misswarn = case_when(missrate <= 0.25 ~ " ",
                                   missrate > 0.25 & missrate <= 0.5 ~ "*",
                                   missrate > 0.5 & missrate <= 0.75 ~ "#",
                                   TRUE ~ "!"))

ggplot(data = filter(fig4dat, censor == "none", delta == 0),
       aes(x = factor(k), y = est, ymin = lower, ymax = upper,
           fill = factor(delta), color = factor(delta), shape = qrpEnv)) +
    facet_grid(tau ~ method, labeller = label_bquote(rows = tau == .(tau))) +
    geom_hline(yintercept = 0, color = "skyblue", alpha = 0.5) +
    geom_hline(yintercept = 0.5, color = "black", alpha = 0.5) +
    geom_pointrange(position = position_dodge(width = 0.5), size = 2,
                    fatten = 1, alpha = 0.95) +
    geom_pointrange(data =  filter(fig4dat, censor == "none", delta == 0.5),
                    position = position_dodge(width = 0.6), size = 2,
                    fatten = 1, alpha = 0.65) +
    geom_text(aes(y = lower - 0.05, label = misswarn), size = 7,
              position = position_dodge(width = 0.6)) +
    scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1)) +
    scale_shape_manual(values = c(21, 22, 24)) +
    scale_fill_manual(values = c("skyblue", "black")) +
    scale_color_manual(values = c("skyblue", "black")) +
    labs(x = "Studies k", y = "Effect estimate", color = "True effect",
         shape = "QRPs", fill = "True effect", title = "No publication bias") +
    coord_flip(ylim = c(-0.5, 1.3)) +
    theme_bw() +
    theme(legend.position = "bottom", panel.grid = element_blank())

## ## figure 3a
## ## TODO fix issues with p-curve having no rejection rates
## fig3dat <- carter2019 |>
##     filter(delta %in% c(0, 0.5)) |>
##     group_by(method, k, delta, tau, qrpEnv, censor) |>
##     summarise(RR = mean(H0.reject.pos, na.rm = TRUE),
##               missrate = mean(is.na(b0_estimate))) |>
##     mutate(RRtype = ifelse(delta == 0, "T1E", "Power"))

## ggplot(data = filter(fig3dat, censor == "none", RRtype == "T1E"),
##        aes(y = RR, x = factor(k), color = RRtype, shape = qrpEnv)) +
##     facet_grid(tau ~ method) +
##     geom_point(position = position_dodge(width = 0.5),
##                size = 2) +
##     geom_point(data = filter(fig3dat, censor == "none", RRtype == "Power"),
##                position = position_dodge(width = 0.5), size = 2) +
##     labs(x = "k", y = "Rejection rate", color = "", shape = "",
##          title = "No publication bias") +
##     coord_flip() +
##     theme_metashowdown +
##     theme(legend.position = "bottom")


## ## some analysis
## carter2019summary <- carter2019 |>
##     mutate(missing_est = is.na(b0_estimate)) |>
##     group_by(method, k, delta, tau, qrpEnv, censor) |>
##     summarise(prop_missing = mean(missing_est)) |>
##     ungroup()

## ## fit decision tree
## library(rpart)
## library(rpart.plot)
## ## carter2019$missing <- factor(is.na(carter2019$b0_estimate),
## ##                              levels = c(FALSE, TRUE),
## ##                              labels = c("obs", "miss"))
## ## tree1 <- rpart(missing ~ ., data = carter2019, method = "class",
## ##                control = rpart.control(maxdepth = 10))
## ## prp(tree1, type = 4, extra = 101, prefix = "Prop. Miss = ")
## tree2 <- rpart(prop_missing ~ ., data = carter2019summary,
##                method = "anova",
##                control = rpart.control(maxdepth = 10))
## prp(tree2, type = 4, extra = 101, prefix = "Prop. Miss = ")
