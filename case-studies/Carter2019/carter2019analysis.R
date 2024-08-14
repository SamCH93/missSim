library(dplyr)
library(ggplot2)
setwd("case-studies/Carter2019")
load("carter2019.rda")


## Report missingness
## -----------------------------------------------------------------------------

fig4a <- function(data) {
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
    plt <- ggplot(data = filter(fig4dat, censor == "none", delta == 0),
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
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        geom_text(data = filter(fig4dat, censor == "none", delta == 0.5),
                  aes(y = upper + 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        ## report missingness rates when larger than 0%
        geom_text(data = filter(fig4dat, censor == "none", missperc != "0%", delta == 0),
                  aes(y = - 0.45, label = missperc), size = 3,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        geom_text(data = filter(fig4dat, censor == "none", missperc != "0%", delta == 0.5),
                  aes(y = 1.4, label = missperc), size = 3,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        scale_y_continuous(breaks = c(-0.6, 0, 0.5, 1)) +
        scale_shape_manual(values = c(21, 22, 24)) +
        scale_fill_manual(values = c("skyblue", "black")) +
        scale_color_manual(values = c("skyblue", "black")) +
        labs(x = "Studies k", y = "Effect estimate", color = "True effect",
             shape = "QRPs", fill = "True effect", title = "No publication bias") +
        coord_flip(ylim = c(-0.5, 1.5)) +
        theme_bw() +
        theme(legend.position = "bottom", panel.grid = element_blank())
    return(plt)
}

## figure 3a
fig3a <- function(data) {
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

    ggplot(data = filter(fig3dat, censor == "none", RRtype == "T1E"),
           aes(x = factor(k), y = RR, ymin = RR - mcse, ymax = RR + mcse,
               color = RRtype, fill = RRtype, shape = qrpEnv)) +
        facet_grid(tau ~ method) +
        geom_pointrange(position = position_dodge(width = 0.5), size = 2,
                        fatten = 1) +
        geom_pointrange(data = filter(fig3dat, censor == "none", RRtype == "Power"),
                        position = position_dodge(width = 0.5), size = 2,
                        fatten = 1) +
        ## missingness warning labels
        geom_text(aes(y = RR - mcse - 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        geom_text(data = filter(fig3dat, censor == "none", delta == 0.5),
                  aes(y = RR + mcse + 0.05, label = misswarn), size = 7,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
         ## missingness rates when larger than 0%
        geom_text(data = filter(fig3dat, censor == "none", missperc != "0%", delta == 0),
                  aes(y = - 0.075, label = missperc), size = 3,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        geom_text(data = filter(fig3dat, censor == "none", missperc != "0%", delta == 0.5),
                  aes(y = 1.075, label = missperc), size = 3,
                  position = position_dodge(width = 0.6), show.legend = FALSE) +
        scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0.05, 0.5, 0.8, 1),
                           labels = scales::percent) +
        labs(x = "Studies k", y = "Rejection rate", color = "True effect",
             shape = "QRPs", fill = "True effect", title = "No publication bias") +
        scale_shape_manual(values = c(21, 22, 24)) +
        scale_fill_manual(values = c("skyblue", "black")) +
        scale_color_manual(values = c("skyblue", "black")) +
        labs(x = "Studies k", y = "Effect estimate", color = "True effect",
             shape = "QRPs", fill = "True effect", title = "No publication bias") +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

## replicate some figures from paper
fig4a(data = carter2019)
fig3a(data = carter2019)

## Fit model to understand occurrence of missingness
## -----------------------------------------------------------------------------
## some analysis
carter2019$missing <- is.na(carter2019$b0_estimate)
carter2019summary <- carter2019 |>
    group_by(method, k, delta, tau, qrpEnv, censor) |>
    summarise(prop_missing = mean(missing)) |>
    ungroup()
carter2019individual <- carter2019 |>
    select(method, k, delta, tau, qrpEnv, censor, missing)

## fit decision tree
library(rpart)
library(rpart.plot)
tree2 <- rpart(prop_missing ~ ., data = carter2019summary,
               method = "anova",
               control = rpart.control(maxdepth = 10))
prp(tree2, type = 4, prefix = "Prop. Miss = ")


## fit logistic regression
glm1 <- glm(missing ~ method*(k + delta + qrpEnv + censor + tau),
            family = "binomial", data = carter2019individual)
summary(glm1)

## fit Firth regression
library(logistf)
firth1 <- logistf(missing ~ method*(k + delta + qrpEnv + censor + tau),
                  data = carter2019individual)
summary(firth1)

## Do not analyze performance of conditions/methods with too much missingness
## -----------------------------------------------------------------------------


## Omit missing values and the associated repetition (list-wise deletion)
## -----------------------------------------------------------------------------
carter2019list <- carter2019 |>
    group_by(id) |>
    mutate(idmiss = any(is.na(b0_estimate))) |>
    mutate(b0_estimate = ifelse(idmiss, NA, b0_estimate)) |>
    ungroup()
a <- fig4a(data = filter(carter2019, tau == 0)) + ggtitle("Case-wise deletion")
b <- fig4a(data = filter(carter2019list, tau == 0)) + ggtitle("List-wise deletion")

ggpubr::ggarrange(a, b, ncol = 1, common.legend = TRUE)
