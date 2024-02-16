library(tidyverse)
library(dplyr)
library(haven)
library(Rmisc)
library(broom)
library(ggpubr)
library(estimatr)
library(sjPlot)
library(margins)
library(stargazer)
library(miceadds)
library(lmtest)
library(multiwayvcov)


## cleaning
# setwd("X") # insert working directory here if you are not working in a project #

load("F1.RData") 

F1 <- F1 %>% 
  filter(response != "2") # eliminating those who have received but not opened the mail

F1 <- F1 %>% 
  filter(response != "3") # eliminating those where the mail got bounced

F1$response <- as.numeric(F1$response)
F1$random <- as.numeric(F1$random)

## creating sub-datasets by party
afd <- F1 %>%
  filter(F1$party == "afd")

fdp <- F1 %>%
  filter(F1$party == "fdp")

green <- F1 %>%
  filter(F1$party == "green")

left <- F1 %>%
  filter(F1$party == "left")

spd <- F1 %>%
  filter(F1$party == "spd")

union <- F1 %>%
  filter(F1$party == "union")

issueowner <- F1 %>%
  filter(F1$party == "green" | F1$party == "afd")

noissueowner <- F1 %>%
  filter(F1$party == "left" | F1$party == "union" | F1$party == "spd" | F1$party == "fdp")


F1$issueowner <- 0
F1$issueowner[F1$party =="green" | F1$party == "afd"] <- 1

## helper functions for DIM analysis 
add_parens <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

make_se_entry <- function(est, se, digits = 2){
  paste0(format_num(est, digits = digits)," ", add_parens(se, digits = digits))
}



## DIM analysis 
fullmodel <- lm_robust(response ~ random, data = F1)
tidy(fullmodel)
summary(fullmodel)
nobs(fullmodel)

afdmodel <- lm_robust(response ~ random, data = afd)
tidy(afdmodel)
summary(afdmodel)
nobs(afdmodel)

unionmodel <- lm_robust(response ~ random, data = union)
tidy(unionmodel)
summary(unionmodel)
nobs(unionmodel)

fdpmodel <- lm_robust(response ~ random, data = fdp)
tidy(fdpmodel)
summary(fdpmodel)
nobs(fdpmodel)

spdmodel <- lm_robust(response ~ random, data = spd)
tidy(spdmodel)
summary(spdmodel)
nobs(spdmodel)

greenmodel <- lm_robust(response ~ random, data = green)
tidy(greenmodel)
summary(greenmodel)
nobs(greenmodel)

leftmodel <- lm_robust(response ~ random, data = left)
tidy(leftmodel)
summary(leftmodel)
nobs(leftmodel)

issuemodel <- lm_robust(response ~ random, data = issueowner)
tidy(issuemodel)
summary(issuemodel)
nobs(issuemodel)

noissuemodel <- lm_robust(response ~ random, data = noissueowner)
tidy(issuemodel)
summary(issuemodel)
nobs(issuemodel)


model1 <-
  bind_rows(
    ` DIM Full Sample` = tidy(fullmodel)[2,],
    ` DIM AfD` = tidy(afdmodel)[2,],
    ` DIM CDU/CSU` = tidy(unionmodel)[2,],
    ` DIM FDP` = tidy(fdpmodel)[2,],
    ` DIM SPD` = tidy(spdmodel)[2,],
    ` DIM Greens` = tidy(greenmodel)[2,],
    ` DIM The Left` = tidy(leftmodel)[2,],
    ` DIM Issue Ownership` = tidy(issuemodel)[2,],
    ` DIM No Issue Ownership` = tidy(noissuemodel)[2,],
    .id = "Model"
  ) %>% 
  mutate(se_entry = make_se_entry(est = estimate,se = std.error,digits = 3))
model1

## DIM plot
plot_model1<- 
  ggplot(model1, aes(estimate, Model)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_text(aes(label=se_entry), nudge_y = 0.15 ) +
  theme_bw() +
  labs(x = "Average Treatment Effect", 
       y = "Estimator") +
  theme(plot.title = element_text(size = 10), 
        axis.text.y = element_text(size = 8))
plot_model1


## Logistic Regression Analysis and Plots
F1$treatment <- NA
F1$treatment[F1$random == 1] <- "MMB recruitment"
F1$treatment[F1$random == 0] <- "General member recruitment"

allplot <- glm(response ~ treatment, 
                family = binomial(link = "logit"), 
                data = F1)
summary(allplot)


### Manuscript Figure ###
plot1 <- plot_model(allplot, 
                    type = "pred", 
                    ci.lvl = 0.95, 
                    title = "", 
                    axis.title = c("Treatment Condition", 
                                   "Probability of Response"),
                    dot.size = 20, 
                    line.size = 5)
plot1

plot1[[1]] +
  ylim(.35, .55) +
  theme_grey(base_size = 20) +
  geom_hline(yintercept = .45, color = "red", size = 1, linetype='dotted') +
  geom_hline(yintercept = .5, color = "red", size = 1, linetype='dotted') +
  geom_hline(yintercept = .4, color = "red", size = 1, linetype='dotted') +
  geom_hline(yintercept = .55, color = "red", size = 1, linetype='dotted') +
  geom_hline(yintercept = .35, color = "red", size = 1, linetype='dotted')


### ###

# check with cluster-robust SE
vcov_both <- cluster.vcov(allplot, cbind(F1$state, F1$party))
aptab1 <- coeftest(allplot, vcov_both)

# check with block fixed effects
allfixed <- glm(response ~ treatment + state + party, 
               family = binomial(link = "logit"), 
               data = F1)
summary(allfixed)
aptab2 <- allfixed

stargazer(aptab1, aptab2, type = "html", out = "clusteredandfixed.html")

# sub plots
leftplot <- glm(response ~ random, 
               family = binomial(link = "logit"), 
               data = left)
plot2 <- plot_model(leftplot, type = "pred", ci.lvl = 0.95, title = "The Left", axis.title = c("Treatment", "Probability of Response"))

spdplot <- glm(response ~ random, 
               family = binomial(link = "logit"), 
               data = spd)
plot3 <- plot_model(spdplot, type = "pred", ci.lvl = 0.95, title = "SPD", axis.title = c("Treatment", "Probability of Response"))

greenplot <- glm(response ~ random, 
               family = binomial(link = "logit"), 
               data = green)
plot4 <- plot_model(greenplot, type = "pred", ci.lvl = 0.95, title = "The Greens", axis.title = c("Treatment", "Probability of Response"))

fdpplot <- glm(response ~ random, 
               family = binomial(link = "logit"), 
               data = fdp)
plot5 <- plot_model(fdpplot, type = "pred", ci.lvl = 0.95, title = "FDP", axis.title = c("Treatment", "Probability of Response"))

unionplot <- glm(response ~ random, 
               family = binomial(link = "logit"), 
               data = union)
plot6 <- plot_model(unionplot, type = "pred", ci.lvl = 0.95, title = "CDU/CSU", axis.title = c("Treatment", "Probability of Response"))

afdplot <- glm(response ~ random, 
               family = binomial(link = "logit"), 
               data = afd)
plot7 <- plot_model(afdplot, type = "pred", ci.lvl = 0.95, title = "AfD", axis.title = c("Treatment", "Probability of Response"))

issueplot <- glm(response ~ random, 
                 family = binomial(link = "logit"), 
                 data = issueowner)
plot8 <- plot_model(issueplot, type = "pred", ci.lvl = 0.95, title = "Issue Ownership", axis.title = c("Treatment", "Probability of Response"))

noissueplot <- glm(response ~ random, 
               family = binomial(link = "logit"), 
               data = noissueowner)
plot9 <- plot_model(noissueplot, type = "pred", ci.lvl = 0.95, title = "No Issue Ownership", axis.title = c("Treatment", "Probability of Response"))


# other plots
marginplotsingle <- ggarrange(plotlist = c(plot2, plot3, plot4, plot5, plot6, plot7), ncol = 3, nrow = 2)
marginplotsingle

marginplotgroup <- ggarrange(plotlist = c(plot8, plot9), ncol = 2, nrow = 1)
marginplotgroup

# plot for manuscript
plot1

stargazer(leftplot, spdplot, greenplot, fdpplot, unionplot, afdplot, type = "html", out = "Parties.html")
stargazer(allplot, issueplot, noissueplot, type = "html", out = "Partygroups.html")

