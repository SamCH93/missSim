library(dplyr)
library(tidyr)
library(ggplot2)
setwd("case-studies/Carter2019")
load("carter2019.rda")

## Replicate some figures from paper
## -----------------------------------------------------------------------------

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
                      fill = factor(delta), color = factor(delta), shape = qrpEnv)) +
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
        geom_text(data = filter(fig4dat, censor == pubbias, missperc != "0%", delta == 0),
                  aes(y = - 0.45, label = missperc), size = 3,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        geom_text(data = filter(fig4dat, censor == pubbias, missperc != "0%", delta == 0.5),
                  aes(y = 1.4, label = missperc), size = 3,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
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

## figure 3a
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
                        position = position_dodge(width = 0.75), size = 2, alpha = 0.9,
                        fatten = 1) +
        ## missingness warning labels
        geom_text(aes(y = RR - mcse - 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
        geom_text(data = filter(fig3dat, censor == pubbias, delta == 0.5),
                  aes(y = RR + mcse + 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
         ## missingness rates when larger than 0%
        geom_text(data = filter(fig3dat, censor == pubbias, missperc != "0%", delta == 0),
                  aes(y = - 0.1, label = missperc), size = 2.5,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
        geom_text(data = filter(fig3dat, censor == pubbias, missperc != "0%", delta == 0.5),
                  aes(y = 1.125, label = missperc), size = 2.5,
                  position = position_dodge(width = 0.75), show.legend = FALSE) +
        scale_y_continuous(limits = c(-0.125, 1.175), breaks = c(0.05, 0.5, 0.8, 1),
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

## ## replicate some figures from paper
## fig4(data = carter2019, pubbias = "none")
## fig3(data = carter2019, pubbias = "none")
## ggsave("carter2019-fig3a.pdf", plot = fig3a, height = 1.5, width = 2.5, scale = 6)


## case-wise vs. list-wise deletion vs. baseline replacement -- how does it
## change bias and RR?
## -----------------------------------------------------------------------------
## Replace missing values by baseline method (RE meta-analysis)
carter2019rema <- carter2019 |>
    filter(method == "RE") |>
    rename(b0_estimaterema = b0_estimate,
           H0.reject.posrema = H0.reject.pos) |>
    select(id, k, delta, tau, qrpEnv, censor, b0_estimaterema, H0.reject.posrema)

## compute rejection rate and bias
carter2019summaries <- left_join(carter2019, carter2019rema) |>
    mutate(b0_estimate.base = ifelse(is.na(b0_estimate), b0_estimaterema,
                                     b0_estimate),
           H0.reject.pos.base = ifelse(is.na(H0.reject.pos), H0.reject.posrema,
                                       H0.reject.pos)) |>
    select(-b0_estimaterema, H0.reject.posrema) |>
    mutate(missing = is.na(b0_estimate),
           missingbase = is.na(b0_estimate.base)) |>
    group_by(id) |>
    mutate(listmissing = any(missing)) |>
    ungroup() |>
    mutate(b0_estimate.list = ifelse(listmissing, NA, b0_estimate),
           H0.reject.pos.list = ifelse(listmissing, NA, H0.reject.pos)) |>
    group_by(method, k, delta, tau, qrpEnv, censor) |>
    summarise(RRcase = mean(H0.reject.pos, na.rm = TRUE),
              RRlist = mean(H0.reject.pos.list, na.rm = TRUE),
              RRbase = mean(H0.reject.pos.base, na.rm = TRUE),
              RRMCSEcase = sqrt(RRcase*(1 - RRcase)/sum(!missing)),
              RRMCSElist = sqrt(RRlist*(1 - RRlist)/sum(!listmissing)),
              RRMCSEbase = sqrt(RRbase*(1 - RRbase)/sum(!missingbase)),
              biascase = mean(b0_estimate - delta, na.rm = TRUE),
              biaslist = mean(b0_estimate.list - delta, na.rm = TRUE),
              biasbase = mean(b0_estimate.base - delta, na.rm = TRUE),
              biasMCSEcase = sd(b0_estimate, na.rm = TRUE)/sqrt(sum(!missing)),
              biasMCSElist = sd(b0_estimate.list, na.rm = TRUE)/sqrt(sum(!listmissing)),
              biasMCSEbase = sd(b0_estimate.base, na.rm = TRUE)/sqrt(sum(!missingbase)),
              missratecase = mean(missing),
              missratelist = mean(listmissing),
              missratebase = mean(missingbase)) |>
    ungroup() |>
    group_by(k, delta, tau, qrpEnv, censor) |>
    ## when effect = 0, lower RR (T1E) is better, otherwise higher RR (power) is better
    mutate(RRrankcase = ifelse(delta == 0, rank(RRcase), rank(-RRcase)),
           RRranklist = ifelse(delta == 0, rank(RRlist), rank(-RRlist)),
           RRrankbase = ifelse(delta == 0, rank(RRbase), rank(-RRbase)),
           biasrankcase = rank(abs(biascase)),
           biasranklist = rank(abs(biaslist)),
           biasrankbase = rank(abs(biasbase)))

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

## ## Omit missing values and the associated repetition (list-wise deletion)
## carter2019list <- carter2019 |>
##     group_by(id) |>
##     mutate(listmissing = any(is.na(b0_estimate))) |>
##     mutate(b0_estimate = ifelse(listmissing, NA, b0_estimate)) |>
##     ungroup()

## ## Estimation
## a <- figa4(data = filter(carter2019, tau == 0)) + ggtitle("Case-wise deletion")
## b <- figa4(data = filter(carter2019list, tau == 0)) + ggtitle("List-wise deletion")
## ggpubr::ggarrange(a, b, ncol = 1, common.legend = TRUE)

## ## Hypothesis testing
## carter2019list <- carter2019 |>
##     group_by(id) |>
##     mutate(idmiss = any(is.na(H0.reject.pos))) |>
##     mutate(H0.reject.pos = ifelse(idmiss, NA, H0.reject.pos)) |>
##     ungroup()
## a2 <- fig3(data = filter(carter2019, tau == 0)) + ggtitle("Case-wise deletion")
## b2 <- fig3(data = filter(carter2019list, tau == 0)) + ggtitle("List-wise deletion")
## ggpubr::ggarrange(a2, b2, ncol = 1, common.legend = TRUE)


carter2019summarieslong <- carter2019summaries |>
  filter(delta == 0, tau == 0, k == 10, censor == "none") |>
  pivot_longer(cols = c(RRcase, RRlist, RRbase), names_to = "Type", values_to = "RR") |>
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
  geom_text(aes(y = max(RR + RRMCSE + 0.01),
                label = paste0(round(missrate*100), "%")),
            position = position_dodge(width = dodge), size = 3,
            show.legend = FALSE) +
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


## ## Fit model to understand occurrence of missingness
## ## -----------------------------------------------------------------------------
## ## some analysis
## carter2019$missing <- is.na(carter2019$b0_estimate)
## carter2019summary <- carter2019 |>
##     group_by(method, k, delta, tau, qrpEnv, censor) |>
##     summarise(prop_missing = mean(missing)) |>
##     ungroup()
## carter2019individual <- carter2019 |>
##     select(method, k, delta, tau, qrpEnv, censor, missing)

## ## fit decision tree
## library(rpart)
## library(rpart.plot)
## tree2 <- rpart(prop_missing ~ ., data = carter2019summary,
##                method = "anova",
##                control = rpart.control(maxdepth = 10))
## prp(tree2, type = 4, prefix = "Prop. Miss = ")

## ## fit linear probability model
## lm1 <- lm(missing ~ method*(k + delta + qrpEnv + censor + tau),
##           data = carter2019individual)
## summary(lm1)

## ## fit logistic regression
## glm1 <- glm(missing ~ method*(k + delta + qrpEnv + censor + tau),
##             family = "binomial", data = carter2019individual)
## summary(glm1)

## ## fit Firth regression
## library(logistf)
## firth1 <- logistf(missing ~ method*(k + delta + qrpEnv + censor + tau),
##                   data = carter2019individual)
## summary(firth1)
