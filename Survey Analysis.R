library(cjoint)
library(readr)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(forcats)
library(wordcloud)
library(ggpubr)
library(tidyr)


# Descriptive Analyses


# cleaning 

# setwd("X") # insert working directory here if you are not working in a project #

load("S0.RData") # load data with text values

S0$pie <- NA
S0$pie[S0$Q21 == "Ja (Welcher Art?)"] <- "Yes"
S0$pie[S0$Q21 == "Nein"] <- "No"

S0$Q16_1[S0$Q16_1 == 100] <- NA
S0$Q16_1 <- as.numeric(S0$Q16_1)

left <- S0 %>% 
  filter(Q11 == "Die Linke")

green <- S0 %>% 
  filter(Q11 == "Bündnis 90/Die Grünen")

spd <- S0 %>% 
  filter(Q11 == "SPD")

fdp <- S0 %>% 
  filter(Q11 == "FDP")

union <- S0 %>% 
  filter(Q11 == "CDU/CSU")

afd <- S0 %>% 
  filter(Q11 == "AfD")


## median response time
### filter for those who completed the entire survey
length <- S0 %>% 
  filter(Q21 == "Ja (Welcher Art?)" | Q21 == "Nein")
### median
length$`Duration (in seconds)` <- as.numeric(length$`Duration (in seconds)`)
median(length$`Duration (in seconds)`, na.rm = TRUE)


## Estimated Proportion of Migrant Background Members in Local Party Chapters
mean(S0$Q16_1, na.rm = TRUE)

proportionplot <- S0 %>% 
  filter(S0$Q11 == "Bündnis 90/Die Grünen" | S0$Q11 == "Die Linke" | S0$Q11 == "SPD" | S0$Q11 == "FDP" | S0$Q11 == "CDU/CSU" | S0$Q11 == "AfD")

proportionplot$test[proportionplot$Q11 == "Die Linke"] <- "Left"
proportionplot$test[proportionplot$Q11 == "Bündnis 90/Die Grünen"] <- "Greens"

proportionplot$Q11[proportionplot$Q11 == "Die Linke"] <- "Left"
proportionplot$Q11[proportionplot$Q11 == "Bündnis 90/Die Grünen"] <- "Greens"

proportionplot$Q11 <- fct_reorder(proportionplot$Q11, proportionplot$Q16_1, mean, na.rm = TRUE)

ggplot(data = proportionplot, mapping = aes(x = Q11, y = Q16_1)) +
  stat_summary(fun.data=mean_sdl, geom = "bar", fill = c("yellow", "green", "blue", "black", "purple", "red")) + 
  xlab("Party") + ylab("Estimated Percentage of Migrant Members")


## Importance of Attracting Migrants (also in comparison to other marginalized groups)
imp <- S0 %>% 
  separate_rows(Q14, sep = ",")

imp$eng <- NA
imp$eng[imp$Q14 == "Frauen"] <- "Women"
imp$eng[imp$Q14 == "Junge Menschen"] <- "Young People"
imp$eng[imp$Q14 == "Menschen mit Behinderung"] <- "Disabled People"
imp$eng[imp$Q14 == "LGBTQ+"] <- "LGBTQ+"
imp$eng[imp$Q14 == "Ethnische Minderheiten"] <- "Ethnic Minorities"
imp$eng[imp$Q14 == "Menschen mit Migrationshintergrund"] <- "Migrants"
imp$eng[imp$Q14 == "Sozioökonomisch schwächer gestellte Menschen"] <- "Low SES People"

afdimp <- imp %>% 
  filter(Q11 == "AfD")

unionimp <- imp %>% 
  filter(Q11 == "CDU/CSU")

fdpimp <- imp %>% 
  filter(Q11 == "FDP")

spdimp <- imp %>% 
  filter(Q11 == "SPD")

greenimp <- imp %>% 
  filter(Q11 == "Bündnis 90/Die Grünen")

leftimp <- imp %>% 
  filter(Q11 == "Die Linke")

tabafd <- afdimp %>% 
  dplyr::count(eng) %>% 
  arrange(desc(n))

tabunion <- unionimp %>% 
  dplyr::count(eng) %>% 
  arrange(desc(n))

tabfdp <- fdpimp %>% 
  dplyr::count(eng) %>% 
  arrange(desc(n))

tabspd <- spdimp %>% 
  dplyr::count(eng) %>% 
  arrange(desc(n))

tabgreen <- greenimp %>% 
  dplyr::count(eng) %>% 
  arrange(desc(n))

tableft <- leftimp %>% 
  dplyr::count(eng) %>% 
  arrange(desc(n))

taball <- imp %>% 
  dplyr::count(eng) %>% 
  arrange(desc(n))

par(mfrow=c(2,3))
wordcloud(words = tableft$eng, freq = tableft$n, colors = "purple", scale = c(2.8,.5), random.order = FALSE, rot.per = 0, min.freq = 1)
wordcloud(words = tabgreen$eng, freq = tabgreen$n, colors = "green", scale = c(2.8,.5), random.order = FALSE, rot.per = 0, min.freq  = 1)
wordcloud(words = tabspd$eng, freq = tabspd$n, colors = "red", scale = c(2.8,.5), random.order = FALSE, rot.per = 0, min.freq = 1)
wordcloud(words = tabfdp$eng, freq = tabfdp$n, colors = "#999900", scale = c(2.8,.5), random.order = FALSE, rot.per = 0, min.freq = 1)
wordcloud(words = tabunion$eng, freq = tabunion$n, colors = "black", scale = c(2.8,.5), random.order = FALSE, rot.per = 0, min.freq = 1)
wordcloud(words = tabafd$eng, freq = tabafd$n, colors = "blue", scale = c(2.8,.5), random.order = FALSE, rot.per = 0, min.freq = 1)

### Manuscript Figure ###
par(mfrow=c(1,1))
wordcloud(words = taball$eng, freq = taball$n, colors = "black", scale = c(4,.35), random.order = FALSE, rot.per = 0, min.freq = 1)

taball <- taball %>% 
  drop_na(eng)

ggplot(taball, aes(x = reorder(eng, n), y = n)) +
  coord_flip() +
  geom_bar(size = 7, position = position_dodge(1.8), stat = "identity") +
  theme_light(base_size = 40) +
  xlab("") +
  ylab("Number of times the group has been mentioned") 
### ###


### Party Figure ###
tableft <- tableft %>% 
  drop_na(eng)

tabafd <- tabafd %>% 
  drop_na(eng)

tabfdp <- tabfdp %>% 
  drop_na(eng)

tabgreen <- tabgreen %>% 
  drop_na(eng)

tabspd <- tabspd %>% 
  drop_na(eng)

tabunion <- tabunion %>% 
  drop_na(eng)

leftbar <- ggplot(tableft, aes(x = eng, y = n)) +
  coord_flip() +
  geom_bar(size = 7, position = position_dodge(1.8), stat = "identity", fill = "purple") +
  theme_light(base_size = 30) +
  xlab("") +
  ylab("") +
  ggtitle("Left") 

afdbar <- ggplot(tabafd, aes(x = eng, y = n)) +
  coord_flip() +
  geom_bar(size = 7, position = position_dodge(1.8), stat = "identity", fill = "blue") +
  theme_light(base_size = 30) +
  xlab("") +
  ylab("") +
  ggtitle("AfD") 

fdpbar <- ggplot(tabfdp, aes(x = eng, y = n)) +
  coord_flip() +
  geom_bar(size = 7, position = position_dodge(1.8), stat = "identity", fill = "#999900") +
  theme_light(base_size = 30) +
  xlab("") +
  ylab("") +
  ggtitle("FDP") 

greenbar <- ggplot(tabgreen, aes(x = eng, y = n)) +
  coord_flip() +
  geom_bar(size = 7, position = position_dodge(1.8), stat = "identity", fill = "green") +
  theme_light(base_size = 30) +
  xlab("") +
  ylab("") +
  ggtitle("Greens")

spdbar <- ggplot(tabspd, aes(x = eng, y = n)) +
  coord_flip() +
  geom_bar(size = 7, position = position_dodge(1.8), stat = "identity", fill = "red") +
  theme_light(base_size = 30) +
  xlab("") +
  ylab("")  +
  ggtitle("SPD")

unionbar <- ggplot(tabunion, aes(x = eng, y = n)) +
  coord_flip() +
  geom_bar(size = 7, position = position_dodge(1.8), stat = "identity", fill = "black") +
  theme_light(base_size = 30) +
  xlab("") +
  ylab("") +
  ggtitle("CDU/CSU")


## Manuscript Figure
ggarrange(leftbar, greenbar, spdbar, fdpbar, unionbar, afdbar)

## Strategies to Attract Migrants
afdpie <- as.data.frame(table(afd$pie))
afdpie <- afdpie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(afdpie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
pie6 <- ggplot(afdpie, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
  scale_fill_manual(values = c("turquoise", "blue"))
pie6

unionpie <- as.data.frame(table(union$pie))
unionpie <- unionpie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(unionpie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
pie5 <- ggplot(unionpie, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
  scale_fill_manual(values = c("darkgrey", "black"))
pie5

fdppie <- as.data.frame(table(fdp$pie))
fdppie <- fdppie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(fdppie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
pie4 <- ggplot(fdppie, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
  scale_fill_manual(values = c("yellow", "orange2"))
pie4

spdpie <- as.data.frame(table(spd$pie))
spdpie <- spdpie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(spdpie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
pie3 <- ggplot(spdpie, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
  scale_fill_manual(values = c("red", "darkred"))
pie3

greenpie <- as.data.frame(table(green$pie))
greenpie <- greenpie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(greenpie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
pie2 <- ggplot(greenpie, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
  scale_fill_manual(values = c("green", "darkgreen"))
pie2

leftpie <- as.data.frame(table(left$pie))
leftpie <- leftpie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(leftpie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
pie1 <- ggplot(leftpie, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
  scale_fill_manual(values = c("purple", "purple4"))
pie1

pieplot <- ggarrange(pie1, pie2, pie3, pie4, pie5, pie6, ncol = 3, nrow = 2)
pieplot

leftstrat <- as.data.frame(left$Q21_1_TEXT)
greenstrat <- as.data.frame(green$Q21_1_TEXT)
spdstrat <- as.data.frame(spd$Q21_1_TEXT)
fdpstrat <- as.data.frame(fdp$Q21_1_TEXT)
unionstrat <- as.data.frame(union$Q21_1_TEXT)
afdstrat <- as.data.frame(afd$Q21_1_TEXT)


### Manuscript Figure ###
fullpie <- as.data.frame(table(S0$pie))
fullpie <- fullpie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(fullpie$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
pie0 <- ggplot(fullpie, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Var1), color = "white", size=6) +
  scale_fill_manual(values = c("grey", "black"))
pie0
### ###


# Conjoint Analyses
load("S1.RData") # load same data, but with numeric values

## reducing data to those who participated in the conjoint 
CJprep <- S1 %>% 
  drop_na(Q26)

  

## read.qualtrics
CJprep <- CJprep[-c(2),] 

CJprep <- apply(CJprep,2,function(x) gsub("Ã¤",'ae',x))
CJprep <- as.data.frame(CJprep)
CJprep <- apply(CJprep,2,function(x) gsub("Ã¼",'ue',x))
CJprep <- as.data.frame(CJprep)
CJprep <- apply(CJprep,2,function(x) gsub("Ã¶",'oe',x))
CJprep <- as.data.frame(CJprep)
CJprep <- apply(CJprep,2,function(x) gsub("/",'I',x))
CJprep <- as.data.frame(CJprep)

write.csv(CJprep, "CJprep.csv")
CJ <- read.qualtrics("CJprep.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJ, "CJ.csv")
CJ <- read.csv("CJ.csv")

## Analysis and Plot Fullsample
baselines <- list()
baselines$Herkunft <- "Westdeutschland"
allplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJ, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

levels.eng <- list()
levels.eng[["Alter"]] <- c("35", "50", "65")
levels.eng[["Beruf"]] <- c("Unemployed", "Medical Doctor", "Kiosk Owner", "Teacher", "Business Owner", "Store Clerk")
levels.eng[["Freizeitaktivität"]] <- c("Charity", "Environmental Protection", "Church Board", "Mosque Association", "Sports")
levels.eng[["Geschlecht"]] <- c("Male", "Female")
levels.eng[["Herkunft"]] <- c("West Germany", "Bosnia", "France", "East Germany", "Turkey", "UK")
levels.eng[["church"]] <- c("Yes", "No")

### Manuscript Figure ###
plot(allplot, ci = .95, colors = "black", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)
### ###

## Analysis and Plot Right Wing Parties
rightwing <- CJprep %>% 
  filter(Q11 == 2 | Q11 == 5 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(rightwing, "rightwing.csv")
CJright <- read.qualtrics("rightwing.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJright, "CJright.csv")

CJright <- read.csv("CJright.csv")

rightplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJright, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(rightplot, ci = .95, colors = "darkblue", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)

## Analysis and Plot Left Wing Parties
leftwing <- CJprep %>% 
  filter(Q11 == 3 | Q11 == 6 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(leftwing, "leftwing.csv")
CJleft <- read.qualtrics("leftwing.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJleft, "CJleft.csv")

CJleft <- read.csv("CJleft.csv")

leftplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJleft, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(leftplot, ci = .95, colors = "deeppink", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)


## Analysis and Plot Centrist Parties
cent <- CJprep %>% 
  filter(Q11 == 1 | Q11 == 4 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(cent, "cent.csv")
CJcent <- read.qualtrics("cent.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJcent, "CJcent.csv")

CJcent <- read.csv("CJcent.csv")

centplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJcent, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(centplot, ci = .95, colors = "darkorange", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)


## Analysis and Plot SPD
SPD <- CJprep %>% 
  filter(Q11 == 1 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(SPD, "SPD.csv")
CJSPD <- read.qualtrics("SPD.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJSPD, "CJSPD.csv")

CJSPD <- read.csv("CJSPD.csv")

SPDplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJSPD, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(SPDplot, ci = .95, colors = "red", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)


## Analysis and Plot Union
UNION <- CJprep %>% 
  filter(Q11 == 2 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(UNION, "UNION.csv")
CJUNION <- read.qualtrics("UNION.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJUNION, "CJUNION.csv")

CJUNION <- read.csv("CJUNION.csv")

UNIONplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJUNION, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(UNIONplot, ci = .95, colors = "black", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)


## Analysis and Plot Greens
GREEN <- CJprep %>% 
  filter(Q11 == 3 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(GREEN, "GREEN.csv")
CJGREEN <- read.qualtrics("GREEN.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJGREEN, "CJGREEN.csv")

CJGREEN <- read.csv("CJGREEN.csv")

GREENplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJGREEN, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(GREENplot, ci = .95, colors = "green", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)


## Analysis and Plot FDP
FDP <- CJprep %>% 
  filter(Q11 == 4 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(FDP, "FDP.csv")
CJFDP <- read.qualtrics("FDP.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJFDP, "CJFDP.csv")

CJFDP <- read.csv("CJFDP.csv")

FDPplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJFDP, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(FDPplot, ci = .95, colors = "orange", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)


## Analysis and Plot AFD
AFD <- CJprep %>% 
  filter(Q11 == 5 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(AFD, "AFD.csv")
CJAFD <- read.qualtrics("AFD.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJAFD, "CJAFD.csv")

CJAFD <- read.csv("CJAFD.csv")

AFDplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJAFD, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(AFDplot, ci = .95, colors = "blue", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)


## Analysis and Plot LEFT
LEFT <- CJprep %>% 
  filter(Q11 == 6 | Q11 == "Welcher Partei gehören Sie an?")
write.csv(LEFT, "LEFT.csv")
CJLEFT <- read.qualtrics("LEFT.csv", responses = c("Q22", "Q23", "Q24", "Q25", "Q26"))
write.csv(CJLEFT, "CJLEFT.csv")

CJLEFT <- read.csv("CJLEFT.csv")

LEFTplot <- amce(selected ~ Alter + Beruf + Freizeitaktivität + Geschlecht + Herkunft, data = CJLEFT, baselines = baselines, respondent.id = "respondent", cluster = TRUE)

plot(LEFTplot, ci = .95, colors = "purple", attribute.names = c("Age", "Occupation", "Hobby", "Gender", "Origin"), level.names = levels.eng)
