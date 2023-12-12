library(tidyverse)
library(data.table)
library(readxl)
options(scipen = 999)

# Sankeys FOG and Gait Impairment over conditions -----------------------

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))


length(unique(clinical_df$patient)) # 18

unique(clinical_df$condition)
# "MedOFFStimON"  "MedOFFStimOFF" "MedOnStimOFF"  "MedOnStimON"   "OFFpreOP"      "ONpreOP"     

length(unique(clinical_df$condition)) # 6

head(clinical_df)

clinical_df <- clinical_df %>% select(patient, `3.10`, `3.11`, condition) %>%
  mutate(`3.10`=as.numeric(`3.10`), `3.11`=as.numeric(`3.11`))

unique(clinical_df$condition)

clinical_df <- clinical_df %>% mutate(gait_impair=ifelse(`3.10`>=2, "yes", "no"), 
                       fog_impair=ifelse(`3.11`>=1, "yes", "no"))  %>%
  mutate(eval=ifelse(grepl("pre", condition), "PreOP", "PostOP"))

clinical_df %>% group_by(patient) %>% count()



clinical_df %>% select(patient, condition, gait_impair) %>%
  filter(condition!="MedOFFStimON"&condition!="MedOnStimOFF") %>%
  spread(key=condition, value=gait_impair) %>%
  group_by(OFFpreOP, ONpreOP, MedOFFStimOFF, MedOnStimON ) %>% count()

#   OFFpreOP ONpreOP MedOFFStimOFF MedOnStimON     n
# 1 no       no      yes           yes             3
# 2 yes      no      yes           no              4
# 3 yes      no      yes           yes            10
# 4 yes      yes     yes           yes             1


clinical_df %>% select(patient, condition, fog_impair) %>%
  filter(condition!="MedOFFStimON"&condition!="MedOnStimOFF") %>%
  spread(key=condition, value=fog_impair) %>%
  group_by(OFFpreOP, ONpreOP, MedOFFStimOFF, MedOnStimON ) %>% count()


#    OFFpreOP ONpreOP MedOFFStimOFF MedOnStimON     n
# 1 no       no      no            no              3
# 2 no       no      yes           no              1
# 3 no       yes     no            no              1
# 4 yes      no      no            no              1
# 5 yes      no      yes           no              6
# 6 yes      no      yes           yes             5
# 7 yes      yes     yes           no              1


clinical_df %>% select(patient, condition, gait_impair) %>%
  group_by(condition, gait_impair) %>% count() %>%
  spread(key=condition, value=n) %>%
  select(gait_impair, OFFpreOP, ONpreOP, MedOFFStimOFF, MedOnStimOFF, MedOFFStimON, MedOnStimON)

#   gait_impair OFFpreOP ONpreOP MedOFFStimOFF MedOnStimOFF MedOFFStimON MedOnStimON
# 1 no                 3      17            NA            4            1           4
# 2 yes               15       1            18           14           17          14


library(RVAideMemoire)

tab.cont1 <- matrix(c(3,15,17,1,0,18,4,14,1,17,4,14),
                    ncol=2,
                    dimnames=list(c("OFF_PreOP","ON_PreOP","OFFOFF_PostOP", 
                                    "ONOFF_PostOP", "OFFON_PostOP", "ONON_PostOP"),
                                  c("No","Yes")),byrow=TRUE)

fisher.test(tab.cont1)

fisher.multcomp(tab.cont1)


#         Pairwise comparisons using Fisher's exact test for count data
# 
# data:  tab.cont1
# 
#                OFF_PreOP      ON_PreOP OFFOFF_PostOP ONOFF_PostOP OFFON_PostOP
# ON_PreOP      0.00002031             -             -            -            -
# OFFOFF_PostOP 0.42857143 0.00000006281             -            -            -
# ONOFF_PostOP  1.00000000 0.00006023381        0.2226            -            -
# OFFON_PostOP  0.82172373 0.00000053718        1.0000       0.5065            -
# ONON_PostOP   1.00000000 0.00006023381        0.2226       1.0000       0.5065
# 
# P value adjustment method: fdr



clinical_df %>% select(patient, condition, gait_impair) %>%
  spread(key=condition, value=gait_impair) %>%
  group_by(OFFpreOP, ONpreOP, MedOFFStimOFF, MedOnStimOFF, MedOFFStimON, MedOnStimON ) %>% count()


#   OFFpreOP ONpreOP MedOFFStimOFF MedOnStimOFF MedOFFStimON MedOnStimON     n
# 1 no       no      yes           yes          yes          yes             3
# 2 yes      no      yes           no           no           no              1
# 3 yes      no      yes           no           yes          no              2
# 4 yes      no      yes           no           yes          yes             1
# 5 yes      no      yes           yes          yes          no              1
# 6 yes      no      yes           yes          yes          yes             9
# 7 yes      yes     yes           yes          yes          yes             1



clinical_df %>% select(patient, condition, fog_impair) %>%
  group_by(condition, fog_impair) %>% count() %>%
  spread(key=condition, value=n) %>%
  select(fog_impair, OFFpreOP, ONpreOP, MedOFFStimOFF, MedOnStimOFF, MedOFFStimON, MedOnStimON)

#   fog_impair OFFpreOP ONpreOP MedOFFStimOFF MedOnStimOFF MedOFFStimON MedOnStimON
# 1 no                5      16             5           11           11          13
# 2 yes              13       2            13            7            7           5


tab.cont1 <- matrix(c(5,13,16,2,5,13,11,7,11,7,13,5),
                    ncol=2,
                    dimnames=list(c("OFF_PreOP","ON_PreOP","OFFOFF_PostOP", 
                                    "ONOFF_PostOP", "OFFON_PostOP", "ONON_PostOP"),
                                  c("No","Yes")),byrow=TRUE)

fisher.test(tab.cont1)

fisher.multcomp(tab.cont1)



clinical_df %>% select(patient, condition, fog_impair) %>%
  spread(key=condition, value=fog_impair) %>%
  group_by(OFFpreOP, ONpreOP, MedOFFStimOFF, MedOnStimOFF, MedOFFStimON, MedOnStimON ) %>% count()

#    OFFpreOP ONpreOP MedOFFStimOFF MedOnStimOFF MedOFFStimON MedOnStimON     n
#  1 no       no      no            no           no           no              3
#  2 no       no      yes           no           yes          no              1
#  3 no       yes     no            yes          no           no              1
#  4 yes      no      no            no           no           no              1
#  5 yes      no      yes           no           no           no              4
#  6 yes      no      yes           no           yes          no              1
#  7 yes      no      yes           yes          no           yes             1
#  8 yes      no      yes           yes          yes          no              1
#  9 yes      no      yes           yes          yes          yes             4
# 10 yes      yes     yes           no           no           no              1

# ----------------------------

# Plot bar charts with mean + SEM -----------------------------


clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))


length(unique(clinical_df$patient)) # 18


# MDS UPDRS III
# AXIAL score (3.9, 3.10, 3.11, 3.12)
# Akinesia score (3.4, 3.6, 3.7, 3.8, 3.9, 3.14)
# Tremor: 3.15, 3.16,3.17, 3.18

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% 
  mutate(AXIAL=`3.9`+`3.10`+`3.11`+`3.12`) %>%
  mutate(AKINESIA=`3.4R`+ `3.4L`+ `3.6R` + `3.6Liff` + `3.7R` + `3.7L` + `3.8R` + `3.8L` + `3.9` + `3.14`) %>%
  mutate(TREMOR=`3.15R` + `3.15L` + `3.16R` + `3.16L` + `3.17R` + `3.17L` + `3.18`) %>%
  select(patient, `UPDRS III`, AXIAL, AKINESIA, TREMOR, condition)


clinical_df <- gather(clinical_df, Var, Score,  `UPDRS III`:TREMOR)

unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  mutate(condition=ifelse(condition=="MedOnStimOFF", "MedONStimOFF",
                          ifelse(condition=="MedOnStimON", "MedONStimON", condition))) %>%
  mutate(condition=factor(condition, levels=c("OFFpreOP", "ONpreOP", "MedOFFStimOFF", "MedONStimOFF", "MedOFFStimON", "MedONStimON")))

clinical_df <- clinical_df %>% arrange(condition)


clinical_df %>% group_by(condition, Var) %>% summarise(n=mean(Score)) %>% spread(key=condition, value=n)
 
#   Var       OFFpreOP ONpreOP MedOFFStimOFF MedONStimOFF MedOFFStimON MedONStimON
# 1 AKINESIA     20.4     9.67         23.7          17.3        17.4       13.3  
# 2 AXIAL         5.5     1.06          5.89          3.5         4.56       2.94 
# 3 TREMOR        4.39    1.78          3.28          2           1.83       0.889
# 4 UPDRS III    49.9    23.2          54.4          39.7        40.7       30.7

clinical_df %>% group_by(condition, Var) %>% summarise(n=sd(Score)/sqrt(18)) %>% spread(key=condition, value=n)

#   Var       OFFpreOP ONpreOP MedOFFStimOFF MedONStimOFF MedOFFStimON MedONStimON
# 1 AKINESIA     1.11    1.24          1.04         1.21         0.919       1.27 
# 2 AXIAL        0.682   0.308         0.855        0.668        0.785       0.569
# 3 TREMOR       1.27    0.597         0.775        0.577        0.437       0.227
# 4 UPDRS III    2.59    2.03          2.14         2.61         1.92        2.38 


# AKINESIA

mean_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="AKINESIA",], function(x) mean(x))
sd_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="AKINESIA",], function(x) sd(x)/sqrt(18))
mean_stats <- mean_stats %>% select(-Var)
sd_stats <- sd_stats %>% select(-Var)
summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = Score_mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df[clinical_df$Var=="AKINESIA",], 
              aes(x = condition, 
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "Akinesia Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  scale_fill_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  xlab("\n Condiiton") + ylab("Akinesia Score \n [ x\u0305 \u00B1 \u03C3 ] \n")



# TREMOR

mean_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="TREMOR",], function(x) mean(x))
sd_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="TREMOR",], function(x) sd(x)/sqrt(18))
mean_stats <- mean_stats %>% select(-Var)
sd_stats <- sd_stats %>% select(-Var)
summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = Score_mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df[clinical_df$Var=="TREMOR",], 
              aes(x = condition, 
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "Tremor Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  scale_fill_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  xlab("\n Condiiton") + ylab("Tremor Score \n [ x\u0305 \u00B1 \u03C3 ] \n")



# AXIAL

mean_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="AXIAL",], function(x) mean(x))
sd_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="AXIAL",], function(x) sd(x)/sqrt(18))
mean_stats <- mean_stats %>% select(-Var)
sd_stats <- sd_stats %>% select(-Var)
summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = Score_mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df[clinical_df$Var=="AXIAL",], 
              aes(x = condition, 
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "Axial Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  scale_fill_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  xlab("\n Condiiton") + ylab("Axial Score \n [ x\u0305 \u00B1 \u03C3 ] \n")



# UPDRSIII

unique(clinical_df$Var)

mean_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="UPDRS III",], function(x) mean(x))
sd_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="UPDRS III",], function(x) sd(x)/sqrt(18))
mean_stats <- mean_stats %>% select(-Var)
sd_stats <- sd_stats %>% select(-Var)
summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = Score_mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df[clinical_df$Var=="UPDRS III",], 
              aes(x = condition, 
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "UPDRS III Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  scale_fill_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  xlab("\n Condiiton") + ylab("UPDRS III Score \n [ x\u0305 \u00B1 \u03C3 ] \n")



# Item 10 , Item 11

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

names(clinical_df)

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% 
  select(patient, `3.10`, `3.11` , condition)

clinical_df <- gather(clinical_df, Var, Score,  `3.10`:`3.11`)

unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  mutate(condition=ifelse(condition=="MedOnStimOFF", "MedONStimOFF",
                          ifelse(condition=="MedOnStimON", "MedONStimON", condition))) %>%
  mutate(condition=factor(condition, levels=c("OFFpreOP", "ONpreOP", "MedOFFStimOFF", "MedONStimOFF", "MedOFFStimON", "MedONStimON")))

clinical_df <- clinical_df %>% arrange(condition)


clinical_df %>% group_by(condition, Var) %>% summarise(n=mean(Score)) %>% spread(key=condition, value=n)
 
#   Var   OFFpreOP ONpreOP MedOFFStimOFF MedONStimOFF MedOFFStimON MedONStimON
# 1 3.10      1.94   0.5            2.33        1.78         2.17        1.78 
# 2 3.11      1.61   0.167          1.44        0.722        0.889       0.389

clinical_df %>% group_by(condition, Var) %>% summarise(n=sd(Score)/sqrt(18)) %>% spread(key=condition, value=n)

#   Var   OFFpreOP ONpreOP MedOFFStimOFF MedONStimOFF MedOFFStimON MedONStimON
# 1 3.10     0.151   0.146         0.140        0.152        0.121       0.152
# 2 3.11     0.325   0.121         0.336        0.253        0.332       0.164


# 3.10 Gait

mean_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="3.10",], function(x) mean(x))
sd_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="3.10",], function(x) sd(x)/sqrt(18))
mean_stats <- mean_stats %>% select(-Var)
sd_stats <- sd_stats %>% select(-Var)
summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = Score_mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df[clinical_df$Var=="3.10",], 
              aes(x = condition, 
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "3.10 (Gait) Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  scale_fill_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  xlab("\n Condiiton") + ylab("3.10 (Gait) Score \n [ x\u0305 \u00B1 \u03C3 ] \n")


# 3.11 Freezing



mean_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="3.11",], function(x) mean(x))
sd_stats <- aggregate(Score ~ condition + Var, data = clinical_df[clinical_df$Var=="3.11",], function(x) sd(x)/sqrt(18))
mean_stats <- mean_stats %>% select(-Var)
sd_stats <- sd_stats %>% select(-Var)
summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = Score_mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df[clinical_df$Var=="3.11",], 
              aes(x = condition, 
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "3.11 (Freezing) Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  scale_fill_manual(values=c("#E3534C", "#A8234C",  "#0087CA", "#00639B", "#003B64", "#001932")) +
  xlab("\n Condiiton") + ylab("3.11 (Freezing) Score \n [ x\u0305 \u00B1 \u03C3 ] \n")



# ---------------------------------------------------------------------
# Compare means across conditions Friedman --------------------------------


clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

# MDS UPDRS III
# AXIAL score (3.9, 3.10, 3.11, 3.12)
# Akinesia score (3.4, 3.6, 3.7, 3.8, 3.9, 3.14)
# Tremor: 3.15, 3.16,3.17, 3.18
# 3.10 , 3.11

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% 
  mutate(AXIAL=`3.9`+`3.10`+`3.11`+`3.12`) %>%
  mutate(AKINESIA=`3.4R`+ `3.4L`+ `3.6R` + `3.6Liff` + `3.7R` + `3.7L` + `3.8R` + `3.8L` + `3.9` + `3.14`) %>%
  mutate(TREMOR=`3.15R` + `3.15L` + `3.16R` + `3.16L` + `3.17R` + `3.17L` + `3.18`) %>%
  select(patient, `UPDRS III`, AXIAL, AKINESIA, TREMOR, `3.10`, `3.11`, condition)

clinical_df <- gather(clinical_df, Var, Score,  `UPDRS III`:`3.11`)

unique(clinical_df$condition)

# clinical_df <- clinical_df %>%
#   mutate(condition=ifelse(condition=="MedOnStimOFF", "MedONStimOFF",
#                           ifelse(condition=="MedOnStimON", "MedONStimON", condition))) %>%
#   mutate(condition=factor(condition, levels=c("OFFpreOP", "ONpreOP", "MedOFFStimOFF", "MedONStimOFF", "MedOFFStimON", "MedONStimON")))

# clinical_df <- clinical_df %>% arrange(condition)






# UPDRS III

UPDRS_III <- clinical_df[clinical_df$Var=="UPDRS III", ]
UPDRS_III <- UPDRS_III %>% select(-Var)
UPDRS_III$patient <- rep(LETTERS[1:18], times = 6)

friedman.test(y=UPDRS_III$Score,  groups=UPDRS_III$condition, blocks=UPDRS_III$patient)

# 	Friedman rank sum test
# 
# data:  UPDRS_III$Score, UPDRS_III$condition and UPDRS_III$patient
# Friedman chi-squared = 70.911, df = 5, p-value = 0.00000000000006624


pairwise.wilcox.test(UPDRS_III$Score, UPDRS_III$condition, p.adj = "bonferroni", paired=T)

#              MedOFFStimOFF MedOFFStimON MedOnStimOFF MedOnStimON OFFpreOP
# MedOFFStimON 0.0032        -            -            -           -       
# MedOnStimOFF 0.0031        1.0000       -            -           -       
# MedOnStimON  0.0032        0.0038       0.0032       -           -       
# OFFpreOP     1.0000        0.1524       0.1119       0.0053      -       
# ONpreOP      0.0032        0.0032       0.0048       0.1946      0.0032



# AXIAL

unique(clinical_df$Var)

AXIAL <- clinical_df[clinical_df$Var=="AXIAL", ]
AXIAL <- AXIAL %>% select(-Var)
AXIAL$patient <- rep(LETTERS[1:18], times = 6)

friedman.test(y=AXIAL$Score,  groups=AXIAL$condition, blocks=AXIAL$patient)

# 	Friedman rank sum test
# 
# data:  AXIAL$Score, AXIAL$condition and AXIAL$patient
# Friedman chi-squared = 49.364, df = 5, p-value = 0.00000000187


pairwise.wilcox.test(AXIAL$Score, AXIAL$condition, p.adj = "bonferroni", paired=T)

#              MedOFFStimOFF MedOFFStimON MedOnStimOFF MedOnStimON OFFpreOP
# MedOFFStimON 0.0795        -            -            -           -       
# MedOnStimOFF 0.0149        0.4242       -            -           -       
# MedOnStimON  0.0069        0.0389       0.1542       -           -       
# OFFpreOP     1.0000        1.0000       0.4852       0.1219      -       
# ONpreOP      0.0047        0.0104       0.0323       0.0764      0.0046



# TREMOR

unique(clinical_df$Var)

TREMOR <- clinical_df[clinical_df$Var=="TREMOR", ]
TREMOR <- TREMOR %>% select(-Var)
TREMOR$patient <- rep(LETTERS[1:18], times = 6)

friedman.test(y=TREMOR$Score,  groups=TREMOR$condition, blocks=TREMOR$patient)
# 
# 	Friedman rank sum test
# 
# data:  TREMOR$Score, TREMOR$condition and TREMOR$patient
# Friedman chi-squared = 11.506, df = 5, p-value = 0.04222


pairwise.wilcox.test(TREMOR$Score, TREMOR$condition, p.adj = "bonferroni", paired=T)


#              MedOFFStimOFF MedOFFStimON MedOnStimOFF MedOnStimON OFFpreOP
# MedOFFStimON 0.20          -            -            -           -       
# MedOnStimOFF 0.35          1.00         -            -           -       
# MedOnStimON  0.07          0.60         0.87         -           -       
# OFFpreOP     1.00          1.00         0.64         0.49        -       
# ONpreOP      1.00          1.00         1.00         1.00        0.14    



# AKINESIA

unique(clinical_df$Var)

AKINESIA <- clinical_df[clinical_df$Var=="AKINESIA", ]
AKINESIA <- AKINESIA %>% select(-Var)
AKINESIA$patient <- rep(LETTERS[1:18], times = 6)

friedman.test(y=AKINESIA$Score,  groups=AKINESIA$condition, blocks=AKINESIA$patient)
# 
# 	Friedman rank sum test
# 
# data:  AKINESIA$Score, AKINESIA$condition and AKINESIA$patient
# Friedman chi-squared = 63.052, df = 5, p-value = 0.00000000000284


pairwise.wilcox.test(AKINESIA$Score, AKINESIA$condition, p.adj = "bonferroni", paired=T)


#              MedOFFStimOFF MedOFFStimON MedOnStimOFF MedOnStimON OFFpreOP
# MedOFFStimON 0.0030        -            -            -           -       
# MedOnStimOFF 0.0040        1.0000       -            -           -       
# MedOnStimON  0.0032        0.0251       0.0107       -           -       
# OFFpreOP     0.2944        0.9100       0.9662       0.0157      -       
# ONpreOP      0.0032        0.0062       0.0110       0.2049      0.0031




# 3.10

unique(clinical_df$Var)

Item3.10 <- clinical_df[clinical_df$Var=="3.10", ]
Item3.10 <- Item3.10 %>% select(-Var)
Item3.10$patient <- rep(LETTERS[1:18], times = 6)

friedman.test(y=Item3.10$Score,  groups=Item3.10$condition, blocks=Item3.10$patient)
# 
# 	Friedman rank sum test
# 
# data:  Item3.10$Score, Item3.10$condition and Item3.10$patient
# Friedman chi-squared = 57.119, df = 5, p-value = 0.00000000004779


pairwise.wilcox.test(Item3.10$Score, Item3.10$condition, p.adj = "bonferroni", paired=T)


#              MedOFFStimOFF MedOFFStimON MedOnStimOFF MedOnStimON OFFpreOP
# MedOFFStimON 1.0000        -            -            -           -       
# MedOnStimOFF 0.2682        0.3940       -            -           -       
# MedOnStimON  0.1542        0.1610       1.0000       -           -       
# OFFpreOP     0.8001        1.0000       1.0000       1.0000      -       
# ONpreOP      0.0033        0.0029       0.0108       0.0075      0.0034  






# 3.11

unique(clinical_df$Var)

Item3.11 <- clinical_df[clinical_df$Var=="3.11", ]
Item3.11 <- Item3.11 %>% select(-Var)
Item3.11$patient <- rep(LETTERS[1:18], times = 6)

friedman.test(y=Item3.11$Score,  groups=Item3.11$condition, blocks=Item3.11$patient)
# 
# 	Friedman rank sum test
# 
# data:  Item3.11$Score, Item3.11$condition and Item3.11$patient
# Friedman chi-squared = 29.83, df = 5, p-value = 0.00001593


pairwise.wilcox.test(Item3.11$Score, Item3.11$condition, p.adj = "bonferroni", paired=T)

# 
#              MedOFFStimOFF MedOFFStimON MedOnStimOFF MedOnStimON OFFpreOP
# MedOFFStimON 0.268         -            -            -           -       
# MedOnStimOFF 0.086         1.000        -            -           -       
# MedOnStimON  0.030         1.000        0.295        -           -       
# OFFpreOP     1.000         0.611        0.280        0.064       -       
# ONpreOP      0.087         1.000        1.000        1.000       0.038

# -----------------------


# Proportion of patients with clinically significant responses ---------


clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)



names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

clinical_df <- clinical_df %>% select(patient, condition, `UPDRS III`) 

clinical_df$`UPDRS III` <- as.numeric(clinical_df$`UPDRS III`)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% spread(key=condition, value=`UPDRS III`)

clinical_df <- clinical_df %>% mutate(ImprovON=ifelse(MedOnStimON-OFFpreOP<(-3.25),1,0 )) %>%
  mutate(WorseOFF=ifelse(MedOFFStimOFF-OFFpreOP>4.63,1,0))

clinical_df <- clinical_df %>%  mutate(WorseON=ifelse(MedOnStimON-ONpreOP<(-3.25),1,0))
  
clinical_df %>% group_by(ImprovON) %>% count()

#   ImprovON     n
# 1        0     2
# 2        1    16

clinical_df %>% group_by(WorseOFF) %>% count()

#   WorseOFF     n
# 1        0    10
# 2        1     8

clinical_df %>% group_by(WorseON) %>% count()

#   WorseON     n
# 1       0    15
# 2       1     3

# ---------------

# SWS and # FOG events -------

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

clinical_df <- clinical_df %>% select(patient, condition,  SWSFOG_) 

sum(is.na(clinical_df))

clinical_df$SWSFOG_ <- as.numeric(clinical_df$SWSFOG_)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="MedOFFStimOFF"|condition=="MedOnStimOFF"|condition=="MedOFFStimON")

clinical_df %>%
  ggplot(aes(x = condition, y = SWSFOG_, color = factor(condition), group = patient)) +
  geom_jitter(position = position_dodge(width = 0.2), size=2, alpha=0.6, show.legend = F) +
  geom_line(position = position_dodge(width = 0.2), show.legend = F) +
  theme_minimal() +
  ylim(0,10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_colour_manual(values=c( "#0087CA", "#001932", "#A8234C")) +
  scale_fill_manual(values=c("#0087CA", "#001932", "#A8234C")) +
  xlab("\n Condiiton") + ylab("# FOG Events  \n")






clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

clinical_df <- clinical_df %>% select(patient, condition,  SWSTime_) 

sum(is.na(clinical_df))

clinical_df$SWSTime_ <- as.numeric(clinical_df$SWSTime_)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="MedOFFStimOFF"|condition=="MedOnStimOFF"|condition=="MedOFFStimON")

sum(is.na(clinical_df))

clinical_df %>%
  ggplot(aes(x = condition, y = SWSTime_, color = factor(condition), group = patient)) +
  geom_jitter(position = position_dodge(width = 0.2), size=2, alpha=0.6, show.legend = F) +
  geom_line(position = position_dodge(width = 0.2), show.legend = F) +
  theme_minimal() +
  #ylim(0,10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_colour_manual(values=c( "#0087CA", "#001932", "#A8234C")) +
  scale_fill_manual(values=c("#0087CA", "#001932", "#A8234C")) +
  xlab("\n Condiiton") + ylab("SWS Time (s)  \n")














clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

clinical_df <- clinical_df %>% filter(!grepl("preOP", condition) & condition!="MedOnStimON") 

unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  select(patient, condition, SWSFOG_) %>% mutate(SWSFOG_=as.numeric(SWSFOG_))


unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  mutate(condition=ifelse(condition=="MedOnStimOFF", "MedONStimOFF",
                          ifelse(condition=="MedOFFStimON", "MedOFFStimON", condition))) %>%
  mutate(condition=factor(condition, levels=c( "MedOFFStimOFF", "MedOFFStimON", "MedONStimOFF")))

clinical_df <- clinical_df %>% arrange(condition)



mean_stats <- aggregate(SWSFOG_ ~ condition , data = clinical_df, function(x) mean(x))
sd_stats <- aggregate(SWSFOG_ ~ condition , data = clinical_df, function(x) sd(x)/sqrt(18))

summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = SWSFOG__mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = SWSFOG__mean  - SWSFOG__sd, ymax = SWSFOG__mean  + SWSFOG__sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df, 
              aes(x = condition, 
                  y = SWSFOG_, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "# FOG Events ") +
  theme_minimal() +
  ylim(0,10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c( "#A8234C",  "#00639B", "#001932")) +
  scale_fill_manual(values=c( "#A8234C",  "#00639B",  "#001932")) +
  xlab("\n Condiiton") + ylab("# FOG Events \n [ x\u0305 \u00B1 \u03C3 ] \n")






clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

clinical_df <- clinical_df %>% filter(!grepl("preOP", condition) & condition!="MedOnStimON") 

unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  select(patient, condition, SWSTime_) %>% mutate(SWSTime_=as.numeric(SWSTime_))


unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  mutate(condition=ifelse(condition=="MedOnStimOFF", "MedONStimOFF",
                          ifelse(condition=="MedOFFStimON", "MedOFFStimON", condition))) %>%
  mutate(condition=factor(condition, levels=c( "MedOFFStimOFF", "MedOFFStimON", "MedONStimOFF")))

clinical_df <- clinical_df %>% arrange(condition)



mean_stats <- aggregate(SWSTime_ ~ condition , data = clinical_df, function(x) mean(x))
sd_stats <- aggregate(SWSTime_ ~ condition , data = clinical_df, function(x) sd(x)/sqrt(18))

summary_stats <- merge(mean_stats, sd_stats, by = "condition", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% arrange(condition)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = condition, y = SWSTime__mean, fill=condition, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = condition, colour=condition, ymin = SWSTime__mean  - SWSTime__sd, ymax = SWSTime__mean  + SWSTime__sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = clinical_df, 
              aes(x = condition, 
                  y = SWSTime_, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "SWS Time (s) ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c( "#A8234C",  "#00639B", "#001932")) +
  scale_fill_manual(values=c( "#A8234C",  "#00639B",  "#001932")) +
  xlab("\n Condiiton") + ylab("SWS Time (s) \n [ x\u0305 \u00B1 \u03C3 ] \n")



