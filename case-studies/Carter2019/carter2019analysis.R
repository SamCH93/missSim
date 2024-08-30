## R script to analyze data from simulation study on publication bias adjustment
## methods for meta-analysis by Carter et al. (2019) regarding different ways to
## handle non-convergence

## packages and data
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)
library(broom)
library(cowplot)
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
    select(method, k, delta, tau, qrpEnv, censor, missingcase) |>
    ## recode as non-ordered factor to use treatment contrasts in regressions
    mutate(qrpEnv = factor(qrpEnv, levels = c("none", "med", "high"),
                           ordered = FALSE),
           censor = factor(censor, levels = c("none", "med", "high"),
                           ordered = FALSE))
carter2019summary <- carter2019 |>
    group_by(method, k, delta, tau, qrpEnv, censor) |>
    summarise(prop_missing = mean(missingcase)) |>
    ungroup()

## ## which methods-conditions show most missingness?
## carter2019summary |>
##     arrange(-prop_missing) |>
##     print(n = 50)

## visualize missingness
plotA <-
    carter2019summary |>
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
    geom_quasirandom(alpha = 0.2) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Method", y = "Condition-wise\nnon-convergence rate") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linetype = "dashed"))
## ggsave(filename = "fig-carter-exploring-missingness1.pdf", width = 8.5,
##        height = 4)


## ### evaluate missingness
## # use aggregated bernoulli for logistic regression
## carter2019summary_fit <- carter2019summary
## # change ordinal factors to factors (tibble)
## carter2019summary_fit$k      <- factor(carter2019summary_fit$k)
## carter2019summary_fit$delta  <- factor(carter2019summary_fit$delta)
## carter2019summary_fit$tau    <- factor(carter2019summary_fit$tau)
## carter2019summary_fit$qrpEnv <- factor(carter2019summary_fit$qrpEnv, ordered = FALSE)
## carter2019summary_fit$censor <- factor(carter2019summary_fit$censor, ordered = FALSE)

## ## this shows complete separation and doesn't converge
## fit_glm <- glm(cbind(1000*prop_missing, 1000) ~ method*(k + delta + qrpEnv + censor + tau),
##                data = carter2019summary_fit,
##                family = binomial)

## ## Firth logistic regression to overcome complete separation
## library(brglm2)
## fit_brglm <- glm(cbind(1000*prop_missing, 1000) ~ method*(k + delta + qrpEnv + censor + tau),
##                  data = carter2019summary_fit,
##                  family = binomial(link = "logit"),
##                  method = "brglmFit",
##                  type = "AS_mean",
##                  trace = TRUE)
## summary(fit_brglm)

# fit lm to the individual data
carter2019individual_fit <- carter2019individual
carter2019individual_fit$k      <- factor(carter2019individual_fit$k)
carter2019individual_fit$delta  <- factor(carter2019individual_fit$delta)
carter2019individual_fit$tau    <- factor(carter2019individual_fit$tau)

fit_lm <- lm(missingcase ~ method*(k + delta + qrpEnv + censor + tau),
             data = carter2019individual_fit)
summary(fit_lm)

# factor importance
anova(fit_lm)

## library(lsr)
## etaSquared(fit_lm) # takes couple minutes
## #                     eta.sq  eta.sq.part
## # method        4.458896e-02 4.890259e-02
## # k             9.526307e-04 1.097305e-03
## # delta         2.751415e-04 3.171742e-04
## # qrpEnv        1.987827e-05 2.292178e-05
## # censor        4.486852e-04 5.171264e-04
## # tau           1.510823e-02 1.712348e-02
## # method:k      9.260856e-03 1.056617e-02
## # method:delta  2.197361e-03 2.527446e-03
## # method:qrpEnv 4.115269e-03 4.723041e-03
## # method:censor 7.033764e-03 8.045612e-03
## # method:tau    4.879687e-02 5.327174e-02

coefDF <- tidy(fit_lm) |>
    rename(coef = term) |>
    mutate(type = case_when(!grepl(":", coef) ~ "Main effects",
                            grepl(":k", coef) ~ "# Studies - Method interaction",
                            grepl(":delta", coef) ~ "Effect - Method interaction",
                            grepl(":qrpEnv", coef) ~ "QRPs - Method interaction",
                            grepl(":censor", coef) ~ "Publication bias - Method interaction",
                            grepl(":tau", coef) ~ "Heterogeneity - Method interaction"),
           type = factor(type,
                         levels = c("Main effects",
                                    "# Studies - Method interaction",
                                    "Effect - Method interaction",
                                    "QRPs - Method interaction",
                                    "Publication bias - Method interaction",
                                    "Heterogeneity - Method interaction")),
           subtype = case_when(grepl(":k30", coef) ~ "Studies = 30",
                               grepl(":k60", coef) ~ "Studies = 60",
                               grepl(":k100", coef) ~ "Studies = 100",
                               grepl(":delta0.2", coef) ~ "Effect = 0.2",
                               grepl(":delta0.5", coef) ~ "Effect = 0.5",
                               grepl(":delta0.8", coef) ~ "Effect = 0.8",
                               grepl(":qrpEnvmed", coef) ~ "QRPs = medium",
                               grepl(":qrpEnvhigh", coef) ~ "QRPs = high",
                               grepl(":censormed", coef) ~ "Publication bias = medium",
                               grepl(":censorhigh", coef) ~ "Publication bias = high",
                               grepl(":tau0.2", coef) ~ "Heterogeneity = medium",
                               grepl(":tau0.4", coef) ~ "Heterogeneity = high",
                               TRUE ~ "Main effects"),
           subtype = factor(subtype,
                            levels = c("Main effect", "Studies = 30",
                                       "Studies = 60", "Studies = 100",
                                       "Effect = 0.2", "Effect = 0.5",
                                       "Effect = 0.8", "QRPs = medium", "QRPs = high",
                                       "Publication bias = medium", "Publication bias = high",
                                       "Heterogeneity = medium", "Heterogeneity = high")),
           subtype2 = case_when(grepl(":k30", coef) ~ "30",
                                grepl(":k60", coef) ~ "60",
                                grepl(":k100", coef) ~ "100",
                                grepl(":delta0.2", coef) ~ "small",
                                grepl(":delta0.5", coef) ~ "medium",
                                grepl(":delta0.8", coef) ~ "large",
                                grepl(":qrpEnvmed", coef) ~ "medium",
                                grepl(":qrpEnvhigh", coef) ~ "high",
                                grepl(":censormed", coef) ~ "medium",
                                grepl(":censorhigh", coef) ~ "high",
                                grepl(":tau0.2", coef) ~ "medium",
                                grepl(":tau0.4", coef) ~ "high",
                                TRUE ~ " "),
           method = stringr::str_extract(coef, "^[^:]*"),
           method = gsub("method", "", method),
           method = factor(method,
                           levels = c("(Intercept)", "TF", "WAAP-WLS","p-curve",
                                      "p-uniform", "PET-PEESE", "3PSM", "k30",
                                      "k60", "k100", "delta0.2", "delta0.5",
                                      "delta0.8", "qrpEnvmed", "qrpEnvhigh" ,
                                      "censormed", "censorhigh", "tau0.2",
                                      "tau0.4"),
                           labels = c("Intercept", "TF", "WAAP-WLS","p-curve",
                                      "p-uniform", "PET-PEESE", "3PSM", "# Studies = 30",
                                      "# Studies = 60", "# Studies = 100", "effect = small", "effect = medium",
                                      "effect = large", "QRPs = medium", "QRPs = high" ,
                                      "PubBias = medium", "PubBias = high", "heterogeneity = medium",
                                      "heterogeneity = high")),
           lower = estimate - qnorm(p = 0.995)*std.error,
           upper = estimate + qnorm(p = 0.995)*std.error,
           ## renaming coefficients
           coef = gsub("method", "", coef),
           coef = gsub(":delta", " : effect = ", coef),
           coef = gsub("delta", "effect = ", coef),
           coef = gsub(":k", " : # studies = ", coef),
           coef = gsub("k", "# studies = ", coef),
           coef = gsub(":qrpEnv", " : QRPs = ", coef),
           coef = gsub("qrpEnv", "QRPs = ", coef),
           coef = gsub(":censor", " : PubBias = ", coef),
           coef = gsub("censor", "PubBias = ", coef),
           coef = gsub(":tau", " : Heterogeneity = ", coef),
           coef = gsub("tau", "Heterogeneity = ", coef),
           coef = gsub("(Intercept)", "Intercept ", coef)
           )
## plotB <-
##     coefDF |>
##     mutate(coef = factor(coef, levels = rev(coefDF$coef))) |>
##     ggplot(aes(x = coef, y = estimate)) +
##     facet_wrap(~ type, scales = "free", ncol = 1) +
##     geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
##     geom_pointrange(aes(ymin = lower, ymax = upper,
##                         color = ifelse(lower > 0 | upper < 0, TRUE, FALSE)),
##                     show.legend = FALSE, fatten = 1) +
##     labs(y = bquote("lower non-convergence" %<-% "Estimate"  %->% "higher non-convergence"),
##          x = "Regression coefficient") +
##     scale_color_manual(values = c(1, 2)) +
##     coord_flip() +
##     theme_bw() +
##     theme(panel.grid.minor = element_blank())
## plot_grid(plotA, plotB, labels = c("A", "B"), ncol = 1, rel_heights = c(1/4, 3/4))
## ggsave(filename = "fig-carter-exploring-missingness.pdf",
##        width = 8, height = 12, scale = 1.4)

subplots <- lapply(X = c("Main effects",
                         "# Studies - Method interaction",
                         "Effect - Method interaction",
                         "QRPs - Method interaction",
                         "Publication bias - Method interaction",
                         "Heterogeneity - Method interaction"),
                   FUN = function(typei) {
                       coefDF |>
                           filter(type == typei) |>
                           mutate(method = factor(method, levels = rev(unique(coefDF$method))),
                                  subtype2 = factor(subtype2, levels = unique(coefDF$subtype2))) |>
                           ggplot(aes(x = method, y = estimate)) +
                           facet_grid(subtype2 ~ type) +
                           geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
                           geom_pointrange(aes(ymin = lower, ymax = upper,
                                               color = ifelse(lower > 0 | upper < 0, TRUE, FALSE)),
                                           show.legend = FALSE, fatten = 1) +
                           labs(y = NULL, x = NULL) +
                           scale_color_manual(values = c(1, 2)) +
                           coord_flip() +
                           theme_bw() +
                           theme(panel.grid.minor = element_blank(),
                                 axis.text.y = element_text(size = rel(1)))
                   })

subplots[[5]] <- subplots[[5]] +
    labs(y = "")
subplots[[6]] <- subplots[[6]] +
    ## labs(y = bquote("lower non-convergence" %<-% "Estimated regression coefficient with 99% CI"  %->% "higher non-convergence"))
    labs(y = "")
plotC1 <- plot_grid(plotlist = subplots, ncol = 2, rel_heights = c(1.4, 1.25, 1.25, 1, 1, 1.1))
plotC <- ggdraw(add_sub(plotC1,
                        bquote("lower non-convergence" %<-% "Estimated regression coefficient with 99% CI"  %->% "higher non-convergence"),
                        vpadding = grid::unit(0, "lines"), y = 5, x = 0.5, vjust = 4.5,
                        size = 12))
plot_grid(plotA, plotC, ncol = 1, rel_heights = c(1, 4), labels = c("A", "B"))
ggsave(filename = "fig-carter-exploring-missingness.pdf", width = 8.5,
       height = 10.5, scale = 1.05)
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
