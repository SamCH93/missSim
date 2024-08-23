## R script to analyze data from simulation study on publication bias adjustment
## methods for meta-analysis by Carter et al. (2019) regarding different ways to
## handle non-convergence

## packages and data
library(dplyr)
library(tidyr)
library(ggplot2)
## setwd("case-studies/Carter2019")
load("carter2019.rda")


## case-wise deletion vs. list-wise deletion vs. baseline replacement -- how
## does it change bias and rejection rate (RR)?
## -----------------------------------------------------------------------------
## extract baseline method (RE meta-analysis) data
carter2019rema <- carter2019 |>
    filter(method == "RE") |>
    rename(b0_estimaterema = b0_estimate,
           H0.reject.posrema = H0.reject.pos) |>
    select(id, k, delta, tau, qrpEnv, censor, b0_estimaterema, H0.reject.posrema)

## compute RR, bias, other metrics for different missingness handling approaches
carter2019summaries <- left_join(carter2019, carter2019rema) |>
    mutate(b0_estimate.base = ifelse(is.na(b0_estimate), b0_estimaterema,
                                     b0_estimate),
           H0.reject.pos.base = ifelse(is.na(H0.reject.pos), H0.reject.posrema,
                                       H0.reject.pos)) |>
    select(-b0_estimaterema, H0.reject.posrema) |>
    mutate(missingcase = is.na(b0_estimate),
           missingbase = is.na(b0_estimate.base)) |>
    group_by(id) |>
    mutate(missinglist = any(missingcase)) |>
    ungroup() |>
    mutate(b0_estimate.list = ifelse(missinglist, NA, b0_estimate),
           H0.reject.pos.list = ifelse(missinglist, NA, H0.reject.pos)) |>
    group_by(method, k, delta, tau, qrpEnv, censor) |>
    summarise(RRcase = mean(H0.reject.pos, na.rm = TRUE),
              RRlist = mean(H0.reject.pos.list, na.rm = TRUE),
              RRbase = mean(H0.reject.pos.base, na.rm = TRUE),
              RRMCSEcase = sqrt(RRcase*(1 - RRcase)/sum(!missingcase)),
              RRMCSElist = sqrt(RRlist*(1 - RRlist)/sum(!missinglist)),
              RRMCSEbase = sqrt(RRbase*(1 - RRbase)/sum(!missingbase)),
              biascase = mean(b0_estimate - delta, na.rm = TRUE),
              biaslist = mean(b0_estimate.list - delta, na.rm = TRUE),
              biasbase = mean(b0_estimate.base - delta, na.rm = TRUE),
              biasMCSEcase = sd(b0_estimate, na.rm = TRUE)/sqrt(sum(!missingcase)),
              biasMCSElist = sd(b0_estimate.list, na.rm = TRUE)/sqrt(sum(!missinglist)),
              biasMCSEbase = sd(b0_estimate.base, na.rm = TRUE)/sqrt(sum(!missingbase)),
              missratecase = mean(missingcase),
              missratelist = mean(missinglist),
              missratebase = mean(missingbase)) |>
    ungroup() |>
    group_by(k, delta, tau, qrpEnv, censor) |>
    ## when effect = 0, lower RR (T1E) is better, otherwise higher RR (power) is better
    mutate(RRrankcase = ifelse(delta == 0, rank(RRcase), rank(-RRcase)),
           RRranklist = ifelse(delta == 0, rank(RRlist), rank(-RRlist)),
           RRrankbase = ifelse(delta == 0, rank(RRbase), rank(-RRbase)),
           biasrankcase = rank(abs(biascase)),
           biasranklist = rank(abs(biaslist)),
           biasrankbase = rank(abs(biasbase))) |>
    ungroup()

## wrangle data in long format for plots
carter2019summarieslong <- carter2019summaries |>
    filter(delta == 0, tau == 0, k == 10, censor == "none") |>
    pivot_longer(cols = c(RRcase, RRlist, RRbase), names_to = "Type",
                 values_to = "RR") |>
    mutate(RRMCSE =  case_when(Type == "RRcase" ~ RRMCSEcase,
                               Type == "RRlist" ~ RRMCSElist,
                               TRUE ~ RRMCSEbase),
           bias = case_when(Type == "RRcase" ~ biascase,
                            Type == "RRlist" ~ biaslist,
                            TRUE ~ biasbase),
           biasMCSE = case_when(Type == "RRcase" ~ biasMCSEcase,
                                Type == "RRlist" ~ biasMCSElist,
                                TRUE ~ biasMCSEbase),
           missrate =  case_when(Type == "RRcase" ~ missratecase,
                                 Type == "RRlist" ~ missratelist,
                                 TRUE ~ missratebase),
           Type = factor(Type, levels = c("RRbase", "RRlist", "RRcase"),
                         labels = c("baseline method (RE) replacement",
                                    "list-wise deletion",
                                    "case-wise deletion")),
           QRP = factor(qrpEnv, levels = c("none", "med", "high"),
                        labels = c("no QRPs environment",
                                   "medium QRPs environment",
                                   "high QRPs environment"))) |>
    ungroup() |>
    select(method, k, delta, tau, Type, QRP, RR, RRMCSE, bias, biasMCSE, missrate)

## plot RR for interesting conditions
dodge <- 0.7
cols <- palette.colors(n = 4, palette = "Okabe-Ito")[-1]
ggplot(data = carter2019summarieslong,
       aes(x = method, y = RR, color = Type)) +
  facet_grid(~ QRP) +
  geom_vline(xintercept = seq(1.5, 6.5), lty = 3, alpha = 0.25) + 
  geom_hline(yintercept = 0.05, lty = 2, alpha = 0.6) +
  geom_errorbar(aes(ymin = RR - RRMCSE, ymax = RR + RRMCSE),
                position = position_dodge(width = dodge), width = 0) +
  geom_point(position = position_dodge(width = dodge)) +
  geom_text(aes(y = 0.115,
                label = paste0(round(missrate*100), "%")),
            position = position_dodge(width = dodge), size = 3,
            show.legend = FALSE, hjust = 1) +
  labs(x = "Method", color = "", y = bquote("Type I error rate " %+-% "MCSE")
       # subtitle = bquote("no publication bias," ~ "no heterogeneity (" * 
       #                     tau == 0 * ")," ~ k == 10 ~ "studies")
  ) +
  scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1), labels = scales::percent) +
  scale_color_manual(values = cols) +
  guides(color = guide_legend(reverse = TRUE)) +
  coord_flip(ylim = c(0, 0.11)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")
ggsave("fig-carter-handling-missingness.pdf", width = 8, height = 5, scale = 0.9)


## understand occurrence of missingness
carter2019$missingcase <- is.na(carter2019$b0_estimate)
carter2019individual <- carter2019 |>
    select(method, k, delta, tau, qrpEnv, censor, missingcase)
carter2019summary <- carter2019 |>
    group_by(method, k, delta, tau, qrpEnv, censor) |>
    summarise(prop_missing = mean(missingcase)) |>
    ungroup()

## which methods-conditions show most missingness?
carter2019summary |>
    arrange(-prop_missing) |>
    print(n = 50)

## visualize missingness
library(ggbeeswarm)
plotA <- carter2019summary |>
    mutate(condition = paste(k, delta, tau, qrpEnv, censor),
           method = factor(method, levels = c("RE", "WAAP-WLS", "PET-PEESE",
                                              "TF", "3PSM", "p-curve",
                                              "p-uniform"))) |>
    ggplot(aes(x = method, y = prop_missing)) +
    ## geom_boxplot() +
    geom_line(aes(group = condition), alpha = 0.1) +
    ## geom_boxplot(alpha = 0.8, outliers = FALSE,
    ##              #position = position_nudge(x = 0.2),
    ##              width = 0.15) +
    geom_quasirandom(alpha = 0.2, width = 0.4) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Method", y = "Condition-wise non-convergence rate") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linetype = "dashed"))

## Firth regression
## library(logistf) # doesn't converge =)
## firth1 <- logistf(missingcase ~ method*(k + delta + qrpEnv + censor + tau),
##                   data = carter2019individual,
##                   firth = TRUE, plcontrol = FALSE,
##                   control = logistf.control(maxit = 1000))
## summary(firth1)
## library(brglm2) # this one converges but takes a while!
## brglmfit <- glm(missingcase ~ method*(k + delta + qrpEnv + censor + tau),
##                 data = carter2019individual,
##                 family = binomial(link = "logit"), method = "brglmFit",
##                 type = "AS_mean")
## save(object = brglmfit, file = "brglmfit.rda") # very large
## load("brglmfit.rda")
## library(broom)
## logistDF <- tidy(brglmfit) |>
##     mutate(term = factor(term, levels = rev(tidy(brglmfit)$term)),
##            lower = estimate - 1.96*std.error,
##            upper = estimate + 1.96*std.error)
## save(logistDF, file = "brglmsummary.rda")
load("brglmsummary.rda")
plotB <-
    ggplot(data = logistDF, aes(x = term, y = estimate)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
    geom_pointrange(aes(ymin = lower, ymax = upper,
                        color = ifelse(lower > 0 | upper < 0, TRUE, FALSE)),
                    show.legend = FALSE) +
    lims(y = c(-18, 18)) +
    labs(y = bquote("lower non-convergence" %<-% "Estimate"  %->% "higher non-convergence"),
         x = "Logistic regression coefficient") +
    scale_color_manual(values = c(1, 2)) +
    coord_flip() +
    theme_bw() +
    theme(panel.grid.minor = element_blank())

library(cowplot)
plot_grid(plotA, plotB, labels = c("A", "B"), ncol = 2)

## ## decision tree
## library(rpart)
## library(rpart.plot)
## tree2 <- rpart(prop_missing ~ ., data = carter2019summary,
##                method = "anova",
##                control = rpart.control(maxdepth = 10))
## prp(tree2, type = 4, prefix = "Prop. Miss = ")

## ## fit linear probability model
## lm1 <- lm(missingcase ~ method*(k + delta + qrpEnv + censor + tau),
##           data = carter2019individual)
## summary(lm1)

## ## logistic regression
## glm1 <- glm(missingcase ~ method*(k + delta + qrpEnv + censor + tau),
##             family = "binomial", data = carter2019individual)
## summary(glm1)


## ## check ranks
## carter2019summaries |>
##     pivot_longer(cols = c("RRrankcase", "RRranklist", "RRrankbase"),
##                  names_to = "type", values_to = "RRrank") |>
##     mutate(type = factor(type, levels = c("RRrankcase", "RRranklist", "RRrankbase"),
##                          labels = c("Case-wise deletion", "List-wise deletion",
##                                     "Baseline method replacement"))) |>
##     ggplot(aes(x = RRrank, fill = type, col = type)) +
##     facet_grid(censor ~ method) +
##     geom_bar(position = position_dodge2(width = 0.1),
##              alpha = 0.5) +
##     labs(x = "Rejection Rate Rank (lower is better)", color = "Approach",
##          fill = "Approach") +
##     scale_x_continuous(breaks = seq(1, 7)) +
##     theme_bw() +
##     theme(panel.grid.minor = element_blank(),
##           legend.position = "top")

## ggplot(data = carter2019summaries, aes(x = biasrankcase)) +
##     facet_grid(censor ~ method) +
##     geom_bar(width = 0.3,  alpha = 0.8) +
##     labs(x = "Bias Rank (lower is better)") +
##     scale_x_continuous(breaks = seq(1, 7)) +
##     theme_bw() +
##     theme(panel.grid.minor = element_blank())
