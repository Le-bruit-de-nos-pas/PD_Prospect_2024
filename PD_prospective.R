library(tidyverse)
library(data.table)
library(readxl)
# library(missMDA)
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

clinical_df %>% group_by(eval, gait_impair, fog_impair) %>% count()

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



clinical_df %>% select(patient, condition, fog_impair) %>%
  group_by(condition, fog_impair) %>% count() %>%
  spread(key=condition, value=n) %>%
  select(fog_impair, OFFpreOP, ONpreOP, MedOFFStimOFF, MedOnStimOFF, MedOFFStimON, MedOnStimON)

#   fog_impair OFFpreOP ONpreOP MedOFFStimOFF MedOnStimOFF MedOFFStimON MedOnStimON
#   <chr>         <int>   <int>         <int>        <int>        <int>       <int>
# 1 no                5      16             5           11           11          13
# 2 yes              13       2            13            7            7           5


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
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.6, height=0.1, width=0.3) +
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
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.6, height=0.1, width=0.3) +
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
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.6, height=0.1, width=0.3) +
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
                  y = Score, color = condition),  show.legend = FALSE, size=3, alpha=0.6, height=0.1, width=0.3) +
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


clinical_df %>% drop_na() %>% group_by(condition) %>% summarise(mean=mean(SWSFOG_))

clinical_df %>%
  ggplot(aes(x = condition, y = SWSFOG_, color = factor(condition), group = patient)) +
  geom_jitter(position = position_dodge(width = 0.2), size=2, alpha=0.6, show.legend = F) +
  geom_line(position = position_dodge(width = 0.2), show.legend = F) +
  theme_minimal() +
 # ylim(0,10) +
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
  #ylim(0,10) +
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




# ---------------------------
# Summary demographics and compare each one Pre vs Post surgery ---------------
demographicdata <- read_xlsx(path="demographicdata.xlsx", skip=0, trim_ws = TRUE)

names(demographicdata)[1] <- "patient"

data.frame(names(demographicdata))

length(unique(demographicdata$patient))

sum(is.na(demographicdata))

head(demographicdata)

demographicdata$R_Voltage <- as.numeric(demographicdata$R_Voltage)
demographicdata$LEDDpreop <- as.numeric(demographicdata$LEDDpreop)
demographicdata$L_Voltage <- as.numeric(demographicdata$L_Voltage)

Imputed <- imputePCA(demographicdata[,-c(1)],ncp=2, scale = T)

demographicdata <- demographicdata %>% select(patient) %>% bind_cols(Imputed$completeObs)

sum(demographicdata<0)

head(demographicdata)

names(demographicdata)

table(demographicdata$Sex) 
mean(demographicdata$AgeDiseaseOnset) ; sd(demographicdata$AgeDiseaseOnset) ; quantile(demographicdata$AgeDiseaseOnset)
mean(demographicdata$AgeSurgery) ; sd(demographicdata$AgeSurgery) ; quantile(demographicdata$AgeSurgery)
mean(demographicdata$AgeEvaluation) ; sd(demographicdata$AgeEvaluation) ; quantile(demographicdata$AgeEvaluation)
mean(demographicdata$DiseaseDurationSurgery) ; sd(demographicdata$DiseaseDurationSurgery) ; quantile(demographicdata$DiseaseDurationSurgery)
mean(demographicdata$DiseaseDurationEvaluation) ; sd(demographicdata$DiseaseDurationEvaluation) ; quantile(demographicdata$DiseaseDurationEvaluation)
mean(demographicdata$Time_after_surgery_m) ; sd(demographicdata$Time_after_surgery_m) ; quantile(demographicdata$Time_after_surgery_m)
mean(demographicdata$LEDDpreop) ; sd(demographicdata$LEDDpreop) ; quantile(demographicdata$LEDDpreop)
mean(demographicdata$`LEDD evaluation`) ; sd(demographicdata$`LEDD evaluation`) ; quantile(demographicdata$`LEDD evaluation`)
table(demographicdata$R_mode) /18
table(demographicdata$L_mode) /18
mean(demographicdata$R_Voltage) ; sd(demographicdata$R_Voltage) ; quantile(demographicdata$R_Voltage)
mean(demographicdata$L_Voltage) ; sd(demographicdata$L_Voltage) ; quantile(demographicdata$L_Voltage)
mean(demographicdata$R_freq) ; sd(demographicdata$R_freq) ; quantile(demographicdata$R_freq)
mean(demographicdata$L_Freq) ; sd(demographicdata$L_Freq) ; quantile(demographicdata$L_Freq)
mean(demographicdata$R_PW) ; sd(demographicdata$R_PW) ; quantile(demographicdata$R_PW)
mean(demographicdata$L_PW) ; sd(demographicdata$L_PW) ; quantile(demographicdata$L_PW)
mean(demographicdata$UPDRS_I_OF_preOP) ; sd(demographicdata$UPDRS_I_OF_preOP) ; quantile(demographicdata$UPDRS_I_OF_preOP)
mean(demographicdata$UPDRS_I_ON_preOP) ; sd(demographicdata$UPDRS_I_ON_preOP) ; quantile(demographicdata$UPDRS_I_ON_preOP)
mean(demographicdata$UPDRS_II_OFF_preOP) ; sd(demographicdata$UPDRS_II_OFF_preOP) ; quantile(demographicdata$UPDRS_II_OFF_preOP)
mean(demographicdata$UPDRS_II_ON_preOP) ; sd(demographicdata$UPDRS_II_ON_preOP) ; quantile(demographicdata$UPDRS_II_ON_preOP)
mean(demographicdata$UPDRS_IV_preOP) ; sd(demographicdata$UPDRS_IV_preOP) ; quantile(demographicdata$UPDRS_IV_preOP)
mean(demographicdata$UPDRS_IV) ; sd(demographicdata$UPDRS_IV) ; quantile(demographicdata$UPDRS_IV)
mean(demographicdata$`Dose LCT_preOP`) ; sd(demographicdata$`Dose LCT_preOP`) ; quantile(demographicdata$`Dose LCT_preOP`)
mean(demographicdata$`Dose LCT`) ; sd(demographicdata$`Dose LCT`) ; quantile(demographicdata$`Dose LCT`)
mean(demographicdata$UPDRS_I_OFF) ; sd(demographicdata$UPDRS_I_OFF) ; quantile(demographicdata$UPDRS_I_OFF)
mean(demographicdata$UPDRS_I_ON) ; sd(demographicdata$UPDRS_I_ON) ; quantile(demographicdata$UPDRS_I_ON)
mean(demographicdata$UPDRS_II_OFF) ; sd(demographicdata$UPDRS_II_OFF) ; quantile(demographicdata$UPDRS_II_OFF)
mean(demographicdata$UPDRS_II_ON) ; sd(demographicdata$UPDRS_II_ON) ; quantile(demographicdata$UPDRS_II_ON)
mean(demographicdata$MMSEpreOP) ; sd(demographicdata$MMSEpreOP) ; quantile(demographicdata$MMSEpreOP)
table(demographicdata$NEUROPSYDXPreop) ; sd(demographicdata$NEUROPSYDXPreop) ; quantile(demographicdata$NEUROPSYDXPreop)


#LEDD

wilcox.test(demographicdata$LEDDpreop, demographicdata$`LEDD evaluation`, paired = TRUE)

# Wilcoxon signed rank exact test
# 
# data:  demographicdata$LEDDpreop and demographicdata$`LEDD evaluation`
# V = 169, p-value = 0.00002289
# alternative hypothesis: true location shift is not equal to 0


df <- demographicdata %>% select(LEDDpreop, `LEDD evaluation`) %>% gather(Var, Score, LEDDpreop:`LEDD evaluation`) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "LEDDpreop", "A- LEDD Pre-OP", "B- LEDD Post-OP")) %>% arrange(Var)
df <- df %>% mutate(Var=ifelse(Var == "LEDDpreop", "A- LEDD Pre-OP", "B- LEDD Post-OP")) %>% arrange(Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "LEDD (mg)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#A8234C", "#00639B")) +
  scale_fill_manual(values=c("#A8234C", "#00639B")) +
  xlab("\n Pre- vs Post-OP Eval") + ylab("LEDD (mg) \n [ x\u0305 \u00B1 \u03C3 ] \n")





#UPDRS I ON
wilcox.test(demographicdata$UPDRS_I_OF_preOP, demographicdata$UPDRS_I_OFF, paired = TRUE)

# Wilcoxon signed rank test with continuity
# correction
# 
# data:  demographicdata$UPDRS_I_OF_preOP and demographicdata$UPDRS_I_OFF
# V = 76.5, p-value = 0.678
# alternative hypothesis: true location shift is not equal to 0

df <- demographicdata %>% select(UPDRS_I_OF_preOP, UPDRS_I_OFF) %>% gather(Var, Score, UPDRS_I_OF_preOP:UPDRS_I_OFF) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "UPDRS_I_OF_preOP", "A- UPDRS I OFF Pre-OP", "B- UPDRS I OFF Post-OP")) %>% arrange(Var)
df <- df %>%  mutate(Var=ifelse(Var == "UPDRS_I_OF_preOP", "A- UPDRS I OFF Pre-OP", "B- UPDRS I OFF Post-OP")) %>% arrange(Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "UPDRS I OFF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#A8234C", "#00639B")) +
  scale_fill_manual(values=c("#A8234C", "#00639B")) +
  xlab("\n Pre- vs Post-OP Eval") + ylab("UPDRS I OFF \n [ x\u0305 \u00B1 \u03C3 ] \n")





#UPDRS I ON
wilcox.test(demographicdata$UPDRS_I_ON, demographicdata$UPDRS_I_ON_preOP, paired = TRUE)

# Wilcoxon signed rank test with continuity correction
# 
# data:  demographicdata$UPDRS_I_ON and demographicdata$UPDRS_I_ON_preOP
# V = 114, p-value = 0.07897
# alternative hypothesis: true location shift is not equal to 0

df <- demographicdata %>% select(UPDRS_I_ON, UPDRS_I_ON_preOP) %>% gather(Var, Score, UPDRS_I_ON:UPDRS_I_ON_preOP) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "UPDRS_I_ON_preOP", "A- UPDRS I ON Pre-OP", "B- UPDRS I ON Post-OP")) %>% arrange(Var)
df <- df %>%  mutate(Var=ifelse(Var == "UPDRS_I_ON_preOP", "A- UPDRS I ON Pre-OP", "B- UPDRS I ON Post-OP")) %>% arrange(Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "UPDRS I ON") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#A8234C", "#00639B")) +
  scale_fill_manual(values=c("#A8234C", "#00639B")) +
  xlab("\n Pre- vs Post-OP Eval") + ylab("UPDRS I ON \n [ x\u0305 \u00B1 \u03C3 ] \n")



names(demographicdata)



#UPDRS II OFF
wilcox.test(demographicdata$UPDRS_II_OFF_preOP, demographicdata$UPDRS_II_OFF, paired = TRUE)

# Wilcoxon signed rank test with continuity correction
# 
# data:  demographicdata$UPDRS_II_OFF_preOP and demographicdata$UPDRS_II_OFF
# V = 111.5, p-value = 0.102
# alternative hypothesis: true location shift is not equal to 0

df <- demographicdata %>% select(UPDRS_II_OFF_preOP, UPDRS_II_OFF) %>% gather(Var, Score, UPDRS_II_OFF_preOP:UPDRS_II_OFF) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "UPDRS_II_OFF_preOP", "A- UPDRS II OFF Pre-OP", "B- UPDRS II OFF Post-OP")) %>% arrange(Var)
df <- df %>%  mutate(Var=ifelse(Var == "UPDRS_II_OFF_preOP", "A- UPDRS II OFF Pre-OP", "B- UPDRS II OFF Post-OP")) %>% arrange(Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "UPDRS II OFF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#A8234C", "#00639B")) +
  scale_fill_manual(values=c("#A8234C", "#00639B")) +
  xlab("\n Pre- vs Post-OP Eval") + ylab("UPDRS II OFF \n [ x\u0305 \u00B1 \u03C3 ] \n")



#UPDRS II ON
wilcox.test(demographicdata$UPDRS_II_ON, demographicdata$UPDRS_II_ON_preOP, paired = TRUE)

# Wilcoxon signed rank test with
# continuity correction
# 
# data:  demographicdata$UPDRS_II_ON and demographicdata$UPDRS_II_ON_preOP
# V = 147, p-value = 0.007813
# alternative hypothesis: true location shift is not equal to 0

df <- demographicdata %>% select(UPDRS_II_ON_preOP, UPDRS_II_ON) %>% gather(Var, Score, UPDRS_II_ON_preOP:UPDRS_II_ON) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "UPDRS_II_ON_preOP", "A- UPDRS II ON Pre-OP", "B- UPDRS II ON Post-OP")) %>% arrange(Var)
df <- df %>%  mutate(Var=ifelse(Var == "UPDRS_II_ON_preOP", "A- UPDRS II ON Pre-OP", "B- UPDRS II ON Post-OP")) %>% arrange(Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "UPDRS II ON") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#A8234C", "#00639B")) +
  scale_fill_manual(values=c("#A8234C", "#00639B")) +
  xlab("\n Pre- vs Post-OP Eval") + ylab("UPDRS II ON \n [ x\u0305 \u00B1 \u03C3 ] \n")



#UPDRS IV 
wilcox.test(demographicdata$UPDRS_IV_preOP, demographicdata$UPDRS_IV, paired = TRUE)

# Wilcoxon signed rank test with continuity correction
# 
# data:  demographicdata$UPDRS_IV_preOP and demographicdata$UPDRS_IV
# V = 162.5, p-value = 0.0008537
# alternative hypothesis: true location shift is not equal to 0

df <- demographicdata %>% select(UPDRS_IV_preOP, UPDRS_IV) %>% gather(Var, Score, UPDRS_IV_preOP:UPDRS_IV) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "UPDRS_IV_preOP", "A- UPDRS IV Pre-OP", "B- UPDRS IV Post-OP")) %>% arrange(Var)
df <- df %>%  mutate(Var=ifelse(Var == "UPDRS_IV_preOP", "A- UPDRS IV Pre-OP", "B- UPDRS IV Post-OP")) %>% arrange(Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "UPDRS IV") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#A8234C", "#00639B")) +
  scale_fill_manual(values=c("#A8234C", "#00639B")) +
  xlab("\n Pre- vs Post-OP Eval") + ylab("UPDRS IV \n [ x\u0305 \u00B1 \u03C3 ] \n")



names(demographicdata)


#LCT change 

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))


length(unique(clinical_df$patient)) # 18

unique(clinical_df$condition)

LCT_change <- clinical_df %>% select(patient, `UPDRS III`, condition) %>% 
  mutate(`UPDRS III`=as.numeric(`UPDRS III`)) %>%
  filter(condition=="OFFpreOP"|condition=="ONpreOP") %>%
  spread(key=condition, value=`UPDRS III`) %>%
  mutate(ChangePreOP= (OFFpreOP-ONpreOP)/OFFpreOP) %>%
  select(patient, ChangePreOP) %>%
  left_join(
    clinical_df %>% select(patient, `UPDRS III`, condition) %>% 
      mutate(`UPDRS III`=as.numeric(`UPDRS III`)) %>%
      filter(condition=="MedOFFStimOFF"|condition=="MedOnStimOFF") %>%
      spread(key=condition, value=`UPDRS III`) %>%
      mutate(ChangePostOP= (MedOFFStimOFF-MedOnStimOFF)/MedOFFStimOFF) %>%
      select(patient, ChangePostOP)
  ) %>% select(-patient)




mean(LCT_change$ChangePreOP) # 0.5405349
sd(LCT_change$ChangePreOP)  # 0.1216855
quantile(LCT_change$ChangePreOP)
# 0%       25%       50%       75%      100% 
# 0.3018868 0.4952532 0.5394042 0.6091074 0.7377049

mean(LCT_change$ChangePostOP) # 0.2773776
sd(LCT_change$ChangePostOP)  # 0.1367003
quantile(LCT_change$ChangePostOP)
# 0%        25%        50%        75%       100% 
# 0.01851852 0.18241571 0.26846154 0.40909091 0.47619048 

wilcox.test(LCT_change$ChangePreOP, LCT_change$ChangePostOP, paired = TRUE)


# Wilcoxon signed rank exact test
# 
# data:  LCT_change$ChangePreOP and LCT_change$ChangePostOP
# V = 171, p-value = 0.000007629
# alternative hypothesis: true location shift is not equal to 0

df <- LCT_change %>% select(ChangePreOP , ChangePostOP) %>% gather(Var, Score, ChangePreOP:ChangePostOP) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "ChangePreOP", "A- LCT % Resp Pre-OP", "B- LCT % Resp Post-OP")) %>% arrange(Var)
df <- df %>%  mutate(Var=ifelse(Var == "ChangePreOP", "A- LCT % Resp Pre-OP", "B- LCT % Resp Post-OP")) %>% arrange(Var)

unique(df$Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "LCT % Resp Drop") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#A8234C", "#00639B")) +
  scale_fill_manual(values=c("#A8234C", "#00639B")) +
  xlab("\n Pre- vs Post-OP Eval") + ylab("LCT % Response \n [ x\u0305 \u00B1 \u03C3 ] \n")






#Prop Resp Tp Med or Stim Post OP only

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

unique(clinical_df$condition)

LCT_change <- clinical_df %>% select(patient, `UPDRS III`, condition) %>% 
  mutate(`UPDRS III`=as.numeric(`UPDRS III`)) %>%
  filter(condition=="MedOFFStimOFF"|condition=="MedOnStimOFF") %>%
  spread(key=condition, value=`UPDRS III`) %>%
  mutate(ChangeOFFtoMed= (MedOFFStimOFF-MedOnStimOFF)/MedOFFStimOFF) %>%
  select(patient, ChangeOFFtoMed) %>%
  left_join(
    clinical_df %>% select(patient, `UPDRS III`, condition) %>% 
      mutate(`UPDRS III`=as.numeric(`UPDRS III`)) %>%
      filter(condition=="MedOFFStimOFF"|condition=="MedOFFStimON") %>%
      spread(key=condition, value=`UPDRS III`) %>%
      mutate(ChangeOFFtoStim= (MedOFFStimOFF-MedOFFStimON)/MedOFFStimOFF) %>%
      select(patient, ChangeOFFtoStim)
  ) %>% 
  left_join(
    clinical_df %>% select(patient, `UPDRS III`, condition) %>% 
      mutate(`UPDRS III`=as.numeric(`UPDRS III`)) %>%
      filter(condition=="MedOFFStimOFF"|condition=="MedOnStimON") %>%
      spread(key=condition, value=`UPDRS III`) %>%
      mutate(ChangeOFFtoBoth= (MedOFFStimOFF-MedOnStimON)/MedOFFStimOFF) %>%
      select(patient, ChangeOFFtoBoth)
  ) %>% select(-patient)




mean(LCT_change$ChangeOFFtoMed) # 0.2773776
sd(LCT_change$ChangeOFFtoMed)  # 0.1367003
quantile(LCT_change$ChangeOFFtoMed)
# 0%        25%        50%        75%       100% 
# 0.01851852 0.18241571 0.26846154 0.40909091 0.47619048 

mean(LCT_change$ChangeOFFtoStim) # 0.2526892
sd(LCT_change$ChangeOFFtoStim)  # 0.08157657
quantile(LCT_change$ChangeOFFtoStim)
#       0%       25%       50%       75%      100% 
# 0.1538462 0.1917702 0.2409888 0.2735507 0.4385965 

mean(LCT_change$ChangeOFFtoBoth) # 0.444241
sd(LCT_change$ChangeOFFtoBoth)  # 0.126574
quantile(LCT_change$ChangeOFFtoBoth)
# 0%       25%       50%       75%      100% 
# 0.2786885 0.3330268 0.4242424 0.5232919 0.6315789 


wilcox.test(LCT_change$ChangeOFFtoMed, LCT_change$ChangeOFFtoStim, paired = TRUE) # ns
wilcox.test(LCT_change$ChangeOFFtoMed, LCT_change$ChangeOFFtoBoth, paired = TRUE) #***
wilcox.test(LCT_change$ChangeOFFtoStim, LCT_change$ChangeOFFtoBoth, paired = TRUE) #***






df <- LCT_change %>% select(ChangeOFFtoMed , ChangeOFFtoStim, ChangeOFFtoBoth) %>% gather(Var, Score, ChangeOFFtoMed:ChangeOFFtoBoth) 

mean_stats <- aggregate(Score ~ Var, data = df, function(x) mean(x))
sd_stats <- aggregate(Score ~  Var, data = df, function(x) sd(x)/sqrt(18))
summary_stats <- merge(mean_stats, sd_stats, by = "Var", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(Var=ifelse(Var == "ChangeOFFtoMed", "A- % Resp MDS-UPDRS LD", ifelse(Var =="ChangeOFFtoStim", "B- % Resp MDS-UPDRS STIM",  "C- % Resp MDS-UPDRS LD + STIM"))) %>% arrange(Var)
df <- df %>%  mutate(Var=ifelse(Var == "ChangeOFFtoMed", "A- % Resp MDS-UPDRS LD", ifelse(Var =="ChangeOFFtoStim", "B- % Resp MDS-UPDRS STIM",  "C- % Resp MDS-UPDRS LD + STIM"))) %>% arrange(Var)

unique(df$Var)

names(summary_stats)

ggplot() +
  geom_bar(data = summary_stats, aes(x = Var, y = Score_mean, fill=Var, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.3 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = Var, colour=Var, ymin = Score_mean  - Score_sd, ymax = Score_mean  + Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  geom_jitter(data = df, 
              aes(x = Var, 
                  y = Score, color = Var),  show.legend = FALSE, size=3, alpha=0.7, width=0.3) +
  labs(title = "\u2206 MDS-UPDRS Post-OP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#00639B" , "#00438F", "#A8234C")) +
  scale_fill_manual(values=c("#00639B" , "#00438F", "#A8234C")) +
  xlab("\n Med \u00B1 Stim") + ylab("LCT % Resp \n [ x\u0305 \u00B1 \u03C3 ] \n")





# --------------------------------
# Kinematics summary and fold change OFF to ONs Post-OP -----------------


# Summary post

kine_postop_only <- read_xlsx(path="kine_postop_only.xlsx", skip=0, trim_ws = TRUE)
names(kine_postop_only)[1] <- "patient"
names(kine_postop_only)

length(unique(kine_postop_only$patient))

kine_postop_only
sum(kine_postop_only<0, na.rm=T)

Imputed <- missMDA::imputePCA(kine_postop_only[,-c(1,27)],ncp=2, scale = T)

kine_postop_only <- kine_postop_only %>% select(patient, condition) %>% bind_cols(Imputed$completeObs)

sum(kine_postop_only<0)

names(kine_postop_only)

kine_postop_only$`Step Time_Assym` <- abs(kine_postop_only$`Step Time_Assym`)
kine_postop_only$Step_Lenght_Assym <- abs(kine_postop_only$Step_Lenght_Assym)
kine_postop_only$`Stance Time_Assym` <- abs(kine_postop_only$`Stance Time_Assym`)
kine_postop_only$`Swing Time_Assym` <- abs(kine_postop_only$`Swing Time_Assym`)
kine_postop_only$`Doubble support_Assym` <- abs(kine_postop_only$`Doubble support_Assym`)
kine_postop_only$`Single Support_Assym` <- abs(kine_postop_only$`Single Support_Assym`)

names(kine_postop_only) <- str_replace_all(names(kine_postop_only), " ", "_")
names(kine_postop_only) <- str_replace_all(names(kine_postop_only), "/", "")

unique(kine_postop_only$condition)

fwrite(kine_postop_only, "kine_postop_only_Imp.txt")

kine_postop_only <- fread("kine_postop_only_Imp.txt")

data.frame(names(kine_postop_only))

kine_postop_only <- kine_postop_only %>% select(-patient)

kine_postop_only %>% 
  group_by(condition) %>%
  summarise(across(everything(), mean)) %>%
  transpose()

kine_postop_only %>% 
  group_by(condition) %>%
  summarise(across(everything(), sd)) %>%
  transpose()

kine_postop_only %>% 
  group_by(condition) %>%
  summarise(across(everything(), median)) %>%
  transpose()

kine_postop_only %>%
  group_by(condition) %>%
  summarise(across(everything(), ~quantile(., 0.25))) %>%
  tidyr::unnest(cols = everything()) %>%
  transpose()

kine_postop_only %>%
  group_by(condition) %>%
  summarise(across(everything(), ~quantile(., 0.75))) %>%
  tidyr::unnest(cols = everything()) %>%
  transpose()



# Summary pre

kine_pre_post <- read_xlsx(path="kine_pre_post.xlsx", skip=0, trim_ws = TRUE)
names(kine_pre_post)[1] <- "patient"
names(kine_pre_post)

kine_pre_post
sum(kine_pre_post<0, na.rm=T)

Imputed <- missMDA::imputePCA(kine_pre_post[,-c(1,27)],ncp=2, scale = T)

kine_pre_post <- kine_pre_post %>% select(patient, condition) %>% bind_cols(Imputed$completeObs)

sum(kine_pre_post<0)

names(kine_pre_post)

kine_pre_post$`Step Time_Assym` <- abs(kine_pre_post$`Step Time_Assym`)
kine_pre_post$Step_Lenght_Assym <- abs(kine_pre_post$Step_Lenght_Assym)
kine_pre_post$`Stance Time_Assym` <- abs(kine_pre_post$`Stance Time_Assym`)
kine_pre_post$`Swing Time_Assym` <- abs(kine_pre_post$`Swing Time_Assym`)
kine_pre_post$`Doubble support_Assym` <- abs(kine_pre_post$`Doubble support_Assym`)
kine_pre_post$`Single Support_Assym` <- abs(kine_pre_post$`Single Support_Assym`)

names(kine_pre_post) <- str_replace_all(names(kine_pre_post), " ", "_")
names(kine_pre_post) <- str_replace_all(names(kine_pre_post), "/", "")

unique(kine_pre_post$condition)

fwrite(kine_pre_post, "kine_pre_post_Imp.txt")
kine_pre_post <- fread( "kine_pre_post_Imp.txt")

kine_pre_post <- kine_pre_post %>% select(-patient)


kine_pre_post %>% filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON") %>%
  group_by(condition) %>%
  summarise(across(everything(), mean)) %>%
  transpose()

kine_pre_post %>% filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON") %>%
  group_by(condition) %>%
  summarise(across(everything(), sd)) %>%
  transpose()

kine_pre_post %>% filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON") %>%
  group_by(condition) %>%
  summarise(across(everything(), median)) %>%
  transpose()

kine_pre_post %>% filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON") %>%
  group_by(condition) %>%
  summarise(across(everything(), ~quantile(., 0.25))) %>%
  tidyr::unnest(cols = everything()) %>%
  transpose()

kine_pre_post %>% filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON") %>%
  group_by(condition) %>%
  summarise(across(everything(), ~quantile(., 0.75))) %>%
  tidyr::unnest(cols = everything()) %>%
  transpose()





ON_OFF <- kine_postop_only %>% filter(condition=="MedON/StimOFF") %>% select(-condition)
OFF_ON <- kine_postop_only %>% filter(condition=="MedOFFStimON") %>% select(-condition)
OFF_OFF <- kine_postop_only %>% filter(condition=="MedOFFStimOFF") %>% select(-condition)

DeltaON_OFF <- ON_OFF - OFF_OFF
DeltaOFF_ON <- OFF_ON - OFF_OFF

DeltaON_OFF$Single_Support_Assym
DeltaOFF_ON$Single_Support_Assym

mean(DeltaOFF_ON$`Speed_(ms)`) ; sd(DeltaOFF_ON$`Speed_(ms)`)
mean(DeltaON_OFF$`Speed_(ms)`) ; sd(DeltaON_OFF$`Speed_(ms)`)

mean(DeltaOFF_ON$`Cadence_(stepsmin)`) ; sd(DeltaOFF_ON$`Cadence_(stepsmin)`)
mean(DeltaON_OFF$`Cadence_(stepsmin)`) ; sd(DeltaON_OFF$`Cadence_(stepsmin)`)

mean(DeltaOFF_ON$`Step_Lenght_Var`) ; sd(DeltaOFF_ON$`Step_Lenght_Var`)
mean(DeltaON_OFF$`Step_Lenght_Var`) ; sd(DeltaON_OFF$`Step_Lenght_Var`)

wilcox.test(DeltaOFF_ON$`Speed_(ms)`, DeltaON_OFF$`Speed_(ms)`, paired=T)
wilcox.test(DeltaOFF_ON$hr_vert, DeltaON_OFF$hr_vert, paired=T)
wilcox.test(DeltaOFF_ON$`Cadence_(stepsmin)`, DeltaON_OFF$`Cadence_(stepsmin)`, paired=T)



DeltaON_OFF %>% 
  summarise(across(everything(), mean)) %>%
  transpose() %>% rename("mean"="V1") %>%
  bind_cols(
    DeltaON_OFF %>% 
      summarise(across(everything(), sd)) %>%
      transpose() %>% rename("sd"="V1")
  ) %>% mutate(stand_diffMed=mean/sd) %>% select(stand_diffMed) %>%
  bind_cols(
    DeltaOFF_ON %>% 
      summarise(across(everything(), mean)) %>%
      transpose() %>% rename("mean"="V1") %>%
      bind_cols(
        DeltaOFF_ON %>% 
          summarise(across(everything(), sd)) %>%
          transpose() %>% rename("sd"="V1")
      ) %>% mutate(stand_diffStim=mean/sd) %>% select(stand_diffStim) 
  ) %>%
  bind_cols(data.frame(names(DeltaON_OFF))) %>%
  gather(Cond, value, stand_diffMed:stand_diffStim) %>%
  ggplot(aes(factor(names.DeltaON_OFF., levels=unique(names.DeltaON_OFF.)), value, colour=Cond, fill=Cond)) +
  geom_col(position="dodge", width=0.6, alpha=0.75) +
  scale_fill_manual(values=c("#A8234C", "#001932")) +
  scale_colour_manual(values=c("#A8234C", "#001932")) +
  coord_flip() +
  theme_minimal() +
  ylab("\n Standardized Mean Change \n OFF/OFF to  Med-ON || Stim-ON") + xlab("")





common_column_names <- intersect(names(DeltaON_OFF), names(DeltaOFF_ON))





for (col_name in common_column_names) {
  
  wilcox_result <- wilcox.test(DeltaON_OFF[[col_name]], 
                               DeltaOFF_ON[[col_name]], paired = TRUE)
  
  cat("Wilcoxon Test for", col_name, ":\n")
  
  print(wilcox_result)
  
  cat("\n")
}


# Wilcoxon Test for Speed_(ms) :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 119, p-value = 0.04477
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Cadence_(stepsmin) :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 75, p-value = 0.9632
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Time_(s) :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 58, p-value = 0.4038
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Lenght_(m) :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 122, p-value = 0.03052
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Time_(s) :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 56, p-value = 0.3529
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Length_(m) :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 121, p-value = 0.03479
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Width_(m) :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 57, p-value = 0.3778
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Speed_Var :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 77, p-value = 1
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Time_Var :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 78, p-value = 0.9632
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Lenght_Var :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 67, p-value = 0.6777
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_width_Var :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 0, p-value = 0.00001526
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Lenght_Var :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 61, p-value = 0.4874
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Time_Var :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 88, p-value = 0.6112
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Time_Assym :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 79, p-value = 0.9265
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Lenght_Assym :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 78, p-value = 0.9632
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stance_Time_Assym :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 53, p-value = 0.2842
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Swing_Time_Assym :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 52, p-value = 0.2633
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Doubble_support_Assym :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 88, p-value = 0.6112
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Single_Support_Assym :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 49, p-value = 0.2069
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for entropy_ap :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 61, p-value = 0.4874
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for entropy_vert :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 96, p-value = 0.3778
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for entropy_ml :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 52, p-value = 0.2633
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for hr_ap :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 79, p-value = 0.9265
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for hr_vert :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 80, p-value = 0.89
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for hr_ml :
#   
#   Wilcoxon signed rank exact test
# 
# data:  DeltaON_OFF[[col_name]] and DeltaOFF_ON[[col_name]]
# V = 114, p-value = 0.07968
# alternative hypothesis: true location shift is not equal to 0



# --------------------------------------
# Friedman and paired wilcox Post-OP -------------------------

kine_postop_only <- fread("kine_postop_only_Imp.txt")
kine_postop_only <- kine_postop_only %>% 
  filter(condition=="MedOFFStimON"|condition=="MedON/StimOFF"|condition=="MedOFFStimOFF")


column_names <- names(kine_postop_only)[3:length(names(kine_postop_only))]  


for (col_name in column_names) {
  
  friedman_result <- friedman.test(y=kine_postop_only[[col_name]], 
                                   groups=kine_postop_only$condition,
                                   blocks=kine_postop_only$patient)
  
  cat("Friedman Test for", col_name, ":\n")
  print(friedman_result)
  cat("\n")
}




# Friedman Test for Speed_(ms) :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 3.2941, df = 2, p-value = 0.1926
# 
# 
# Friedman Test for Cadence_(stepsmin) :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 3.2941, df = 2, p-value = 0.1926
# 
# 
# Friedman Test for Step_Time_(s) :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 3.2941, df = 2, p-value = 0.1926
# 
# 
# Friedman Test for Step_Lenght_(m) :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 3.2941, df = 2, p-value = 0.1926
# 
# 
# Friedman Test for Stride_Time_(s) :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 4.2353, df = 2, p-value = 0.1203
# 
# 
# Friedman Test for Stride_Length_(m) :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 4.5882, df = 2, p-value = 0.1009
# 
# 
# Friedman Test for Step_Width_(m) :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 1.5294, df = 2, p-value = 0.4655
# 
# 
# Friedman Test for Speed_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 0.11765, df = 2, p-value = 0.9429
# 
# 
# Friedman Test for Stride_Time_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 4.5882, df = 2, p-value = 0.1009
# 
# 
# Friedman Test for Stride_Lenght_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 0.82353, df = 2, p-value = 0.6625
# 
# 
# Friedman Test for Step_width_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 23.059, df = 2, p-value = 0.000009836
# 
# 
# Friedman Test for Step_Lenght_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 1.5294, df = 2, p-value = 0.4655
# 
# 
# Friedman Test for Step_Time_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 1.0588, df = 2, p-value = 0.589
# 
# 
# Friedman Test for Step_Time_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 0.11765, df = 2, p-value = 0.9429
# 
# 
# Friedman Test for Step_Lenght_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 0.11765, df = 2, p-value = 0.9429
# 
# 
# Friedman Test for Stance_Time_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 1.5294, df = 2, p-value = 0.4655
# 
# 
# Friedman Test for Swing_Time_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 1.5294, df = 2, p-value = 0.4655
# 
# 
# Friedman Test for Doubble_support_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 6.7059, df = 2, p-value = 0.03498
# 
# 
# Friedman Test for Single_Support_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 2.2353, df = 2, p-value = 0.327
# 
# 
# Friedman Test for entropy_ap :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 0.47059, df = 2, p-value = 0.7903
# 
# 
# Friedman Test for entropy_vert :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 4.5882, df = 2, p-value = 0.1009
# 
# 
# Friedman Test for entropy_ml :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 0.11765, df = 2, p-value = 0.9429
# 
# 
# Friedman Test for hr_ap :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 0.82353, df = 2, p-value = 0.6625
# 
# 
# Friedman Test for hr_vert :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 3.6471, df = 2, p-value = 0.1615
# 
# 
# Friedman Test for hr_ml :
#   
#   Friedman rank sum test
# 
# data:  kine_postop_only[[col_name]], kine_postop_only$condition and kine_postop_only$patient
# Friedman chi-squared = 1.5294, df = 2, p-value = 0.4655








for (col_name in column_names) {
  
  result <- pairwise.wilcox.test(kine_postop_only[[col_name]], 
                                 kine_postop_only$condition,
                                 p.adj = "bonferroni", paired=T)
  
  cat("pairwise.wilcox.test for", col_name, ":\n")
  print(result$p.value)
  cat("\n")
}



# pairwise.wilcox.test for Speed_(ms) :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      1.0000000           NA
# MedON/StimOFF     0.2659607    0.1343079
# 
# pairwise.wilcox.test for Cadence_(stepsmin) :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.6206818           NA
# MedON/StimOFF     0.1706543            1
# 
# pairwise.wilcox.test for Step_Time_(s) :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.5702820           NA
# MedON/StimOFF     0.1343079            1
# 
# pairwise.wilcox.test for Step_Lenght_(m) :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      1.0000000           NA
# MedON/StimOFF     0.3266602   0.09155273
# 
# pairwise.wilcox.test for Stride_Time_(s) :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON     0.43629456           NA
# MedON/StimOFF    0.09155273            1
# 
# pairwise.wilcox.test for Stride_Length_(m) :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      1.0000000           NA
# MedON/StimOFF     0.4362946    0.1043701
# 
# pairwise.wilcox.test for Step_Width_(m) :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.6206818           NA
# MedON/StimOFF     1.0000000            1
# 
# pairwise.wilcox.test for Speed_Var :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON              1           NA
# MedON/StimOFF             1            1
# 
# pairwise.wilcox.test for Stride_Time_Var :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.1343079           NA
# MedON/StimOFF     0.1186066            1
# 
# pairwise.wilcox.test for Stride_Lenght_Var :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.9868469           NA
# MedON/StimOFF     1.0000000            1
# 
# pairwise.wilcox.test for Step_width_Var :
#   MedOFFStimOFF  MedOFFStimON
# MedOFFStimON  0.00009155273            NA
# MedON/StimOFF 1.00000000000 0.00004577637
# 
# pairwise.wilcox.test for Step_Lenght_Var :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.7899628           NA
# MedON/StimOFF     1.0000000            1
# 
# pairwise.wilcox.test for Step_Time_Var :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON              1           NA
# MedON/StimOFF             1            1
# 
# pairwise.wilcox.test for Step_Time_Assym :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON              1           NA
# MedON/StimOFF             1            1
# 
# pairwise.wilcox.test for Step_Lenght_Assym :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON              1           NA
# MedON/StimOFF             1            1
# 
# pairwise.wilcox.test for Stance_Time_Assym :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.5227661           NA
# MedON/StimOFF     0.8524933    0.8524933
# 
# pairwise.wilcox.test for Swing_Time_Assym :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.3266602           NA
# MedON/StimOFF     0.7899628    0.7899628
# 
# pairwise.wilcox.test for Doubble_support_Assym :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.3606262           NA
# MedON/StimOFF     0.3971100            1
# 
# pairwise.wilcox.test for Single_Support_Assym :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON              1           NA
# MedON/StimOFF             1    0.6206818
# 
# pairwise.wilcox.test for entropy_ap :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.6206818           NA
# MedON/StimOFF     1.0000000            1
# 
# pairwise.wilcox.test for entropy_vert :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.4781342           NA
# MedON/StimOFF     0.1343079            1
# 
# pairwise.wilcox.test for entropy_ml :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON              1           NA
# MedON/StimOFF             1    0.7899628
# 
# pairwise.wilcox.test for hr_ap :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON              1           NA
# MedON/StimOFF             1            1
# 
# pairwise.wilcox.test for hr_vert :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.9181366           NA
# MedON/StimOFF     1.0000000            1
# 
# pairwise.wilcox.test for hr_ml :
#   MedOFFStimOFF MedOFFStimON
# MedOFFStimON      0.1516113           NA
# MedON/StimOFF     1.0000000    0.2390442

# --------------------------------------------
# Kinematics summary and fold change OFF to BestONs -------------------------

kine_pre_post_Imp <- fread("kine_pre_post_Imp.txt")
unique(kine_pre_post_Imp$condition)
unique(kine_pre_post_Imp$patient)

kine_pre_post_Imp 

kine_pre_post_Imp <- kine_pre_post_Imp %>% 
  filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON"|condition=="MedON/StimON")


kine_pre_post_Imp <- kine_pre_post_Imp %>% select(-patient)

Post_ON <- kine_pre_post_Imp %>% filter(condition=="MedON/StimON") %>% select(-condition)
Pre_ON <- kine_pre_post_Imp %>% filter(condition=="Pre_op_ON") %>% select(-condition)
Pre_OFF <- kine_pre_post_Imp %>% filter(condition=="Pre_op_OFF") %>% select(-condition)

DeltaPre_ON <- Pre_ON - Pre_OFF
DeltaPost_ON <- Post_ON - Pre_OFF

DeltaPre_ON$Single_Support_Assym
DeltaPost_ON$Single_Support_Assym

mean(DeltaPre_ON$`Speed_(ms)`) ; sd(DeltaPre_ON$`Speed_(ms)`)
mean(DeltaPost_ON$`Speed_(ms)`) ; sd(DeltaPost_ON$`Speed_(ms)`)

mean(DeltaPre_ON$`Cadence_(stepsmin)`) ; sd(DeltaPre_ON$`Cadence_(stepsmin)`)
mean(DeltaPost_ON$`Cadence_(stepsmin)`) ; sd(DeltaPost_ON$`Cadence_(stepsmin)`)

mean(DeltaPre_ON$`Step_Lenght_Var`) ; sd(DeltaPre_ON$`Step_Lenght_Var`)
mean(DeltaPost_ON$`Step_Lenght_Var`) ; sd(DeltaPost_ON$`Step_Lenght_Var`)

wilcox.test(DeltaPre_ON$`Speed_(ms)`, DeltaPost_ON$`Speed_(ms)`, paired=T)
wilcox.test(DeltaPre_ON$hr_vert, DeltaPost_ON$hr_vert, paired=T)
wilcox.test(DeltaPre_ON$`Cadence_(stepsmin)`, DeltaPost_ON$`Cadence_(stepsmin)`, paired=T)


DeltaPre_ON %>% 
  summarise(across(everything(), mean)) %>%
  transpose() %>% rename("mean"="V1") %>%
  bind_cols(
    DeltaPre_ON %>% 
      summarise(across(everything(), sd)) %>%
      transpose() %>% rename("sd"="V1")
  ) %>% mutate(stand_diff_Pred=mean/sd) %>% select(stand_diff_Pred) %>%
  bind_cols(
    DeltaPost_ON %>% 
      summarise(across(everything(), mean)) %>%
      transpose() %>% rename("mean"="V1") %>%
      bind_cols(
        DeltaPost_ON %>% 
          summarise(across(everything(), sd)) %>%
          transpose() %>% rename("sd"="V1")
      ) %>% mutate(stand_diff_Post=mean/sd) %>% select(stand_diff_Post) 
  ) %>%
  bind_cols(data.frame(names(DeltaPre_ON))) %>%
  gather(Cond, value, stand_diff_Pred:stand_diff_Post) %>%
  ggplot(aes(factor(names.DeltaPre_ON., levels=unique(names.DeltaPre_ON.)), value, colour=Cond, fill=Cond)) +
  geom_col(position="dodge", width=0.6, alpha=0.75) +
  scale_fill_manual(values=c("#A8234C", "#001932")) +
  scale_colour_manual(values=c("#A8234C", "#001932")) +
  coord_flip() +
  theme_minimal() +
  ylab("\n Standardized Mean Change \n Pre-OP OFF to  Pre-OP Med-ON || PostOP Med-ON/Stim-ON") + xlab("")






common_column_names <- intersect(names(DeltaPre_ON), names(DeltaPost_ON))



for (col_name in common_column_names) {
  
  wilcox_result <- wilcox.test(DeltaPre_ON[[col_name]], 
                               DeltaPost_ON[[col_name]], paired = TRUE)
  
  cat("Wilcoxon Test for", col_name, ":\n")
  
  print(wilcox_result)
  
  cat("\n")
}


# Wilcoxon Test for Speed_(ms) :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 63, p-value = 0.004883
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Cadence_(stepsmin) :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 31, p-value = 0.8984
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Time_(s) :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 26, p-value = 0.5771
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Lenght_(s) :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 66, p-value = 0.0009766
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Time_(s) :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 23, p-value = 0.4131
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Length_(m) :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 66, p-value = 0.0009766
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Width_(m) :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 50, p-value = 0.1475
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Speed_Var :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 22, p-value = 0.3652
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Time_Var :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 0, p-value = 0.0009766
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stride_Lenght_Var :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 10, p-value = 0.04199
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_width_Var :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 0, p-value = 0.0009766
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Lenght_Var :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 19, p-value = 0.2402
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Time_Var :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 5, p-value = 0.009766
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Time_Assym :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 23, p-value = 0.4131
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Step_Lenght_Assym :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 22, p-value = 0.3652
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Stance_Time_Assym :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 29, p-value = 0.7646
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Swing_Time_Assym :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 29, p-value = 0.7646
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Doubble_support_Assym :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 21, p-value = 0.3203
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for Single_Support_Assym :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 29, p-value = 0.7646
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for entropy_ap :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 0, p-value = 0.0009766
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for entropy_vert :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 33, p-value = 1
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for entropy_ml :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 28, p-value = 0.7002
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for hr_ap :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 56, p-value = 0.04199
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for hr_vert :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 53, p-value = 0.08301
# alternative hypothesis: true location shift is not equal to 0
# 
# 
# Wilcoxon Test for hr_ml :
# 
# 	Wilcoxon signed rank exact test
# 
# data:  DeltaPre_ON[[col_name]] and DeltaPost_ON[[col_name]]
# V = 23, p-value = 0.4131
# alternative hypothesis: true location shift is not equal to 0
# 
# 











# --------------------------------------------
# Friedman and paired wilcox Pre to Post to BestON -------------------------

kine_pre_post_Imp <- fread("kine_pre_post_Imp.txt")
unique(kine_pre_post_Imp$condition)
unique(kine_pre_post_Imp$patient)

kine_pre_post_Imp 

kine_pre_post_Imp <- kine_pre_post_Imp %>% 
  filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON"|condition=="MedON/StimON")













column_names <- names(kine_pre_post_Imp)[3:length(names(kine_pre_post_Imp))]  


for (col_name in column_names) {
  
  friedman_result <- friedman.test(y=kine_pre_post_Imp[[col_name]], 
                                   groups=kine_pre_post_Imp$condition,
                                   blocks=kine_pre_post_Imp$patient)
  
  cat("Friedman Test for", col_name, ":\n")
  print(friedman_result)
  cat("\n")
}




# Friedman Test for Speed_(ms) :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 6.7273, df = 2, p-value = 0.03461
# 
# 
# Friedman Test for Cadence_(stepsmin) :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 1.2727, df = 2, p-value = 0.5292
# 
# 
# Friedman Test for Step_Time_(s) :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0, df = 2, p-value = 1
# 
# 
# Friedman Test for Step_Lenght_(s) :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 13.818, df = 2, p-value = 0.0009987
# 
# 
# Friedman Test for Stride_Time_(s) :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0, df = 2, p-value = 1
# 
# 
# Friedman Test for Stride_Length_(m) :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 13.818, df = 2, p-value = 0.0009987
# 
# 
# Friedman Test for Step_Width_(m) :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 1.2727, df = 2, p-value = 0.5292
# 
# 
# Friedman Test for Speed_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.72727, df = 2, p-value = 0.6951
# 
# 
# Friedman Test for Stride_Time_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 17.636, df = 2, p-value = 0.000148
# 
# 
# Friedman Test for Stride_Lenght_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 8.7273, df = 2, p-value = 0.01273
# 
# 
# Friedman Test for Step_width_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 17.636, df = 2, p-value = 0.000148
# 
# 
# Friedman Test for Step_Lenght_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 16.909, df = 2, p-value = 0.0002129
# 
# 
# Friedman Test for Step_Time_Var :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 6.7273, df = 2, p-value = 0.03461
# 
# 
# Friedman Test for Step_Time_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.54545, df = 2, p-value = 0.7613
# 
# 
# Friedman Test for Step_Lenght_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 1.2727, df = 2, p-value = 0.5292
# 
# 
# Friedman Test for Stance_Time_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.18182, df = 2, p-value = 0.9131
# 
# 
# Friedman Test for Swing_Time_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.18182, df = 2, p-value = 0.9131
# 
# 
# Friedman Test for Doubble_support_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 2.9091, df = 2, p-value = 0.2335
# 
# 
# Friedman Test for Single_Support_Assym :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.72727, df = 2, p-value = 0.6951
# 
# 
# Friedman Test for entropy_ap :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 16.909, df = 2, p-value = 0.0002129
# 
# 
# Friedman Test for entropy_vert :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 11.091, df = 2, p-value = 0.003905
# 
# 
# Friedman Test for entropy_ml :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.54545, df = 2, p-value = 0.7613
# 
# 
# Friedman Test for hr_ap :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 6.7273, df = 2, p-value = 0.03461
# 
# 
# Friedman Test for hr_vert :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 8.9091, df = 2, p-value = 0.01163
# 
# 
# Friedman Test for hr_ml :
#   
#   Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.18182, df = 2, p-value = 0.9131
# 
# 




for (col_name in column_names) {
  
  result <- pairwise.wilcox.test(kine_pre_post_Imp[[col_name]], 
                                 kine_pre_post_Imp$condition,
                                 p.adj = "bonferroni", paired=T)
  
  cat("pairwise.wilcox.test for", col_name, ":\n")
  print(result$p.value)
  cat("\n")
}


# pairwise.wilcox.test for Speed_(ms) :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF   1.00000000         NA
# Pre_op_ON    0.01464844 0.02929687
# 
# pairwise.wilcox.test for Cadence_(stepsmin) :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1          1
# 
# pairwise.wilcox.test for Step_Time_(s) :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1          1
# 
# pairwise.wilcox.test for Step_Lenght_(s) :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF  0.618164062         NA
# Pre_op_ON   0.002929687 0.02050781
# 
# pairwise.wilcox.test for Stride_Time_(s) :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1          1
# 
# pairwise.wilcox.test for Stride_Length_(m) :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF  0.524414062         NA
# Pre_op_ON   0.002929687 0.01464844
# 
# pairwise.wilcox.test for Step_Width_(m) :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    0.6181641         NA
# Pre_op_ON     0.4423828          1
# 
# pairwise.wilcox.test for Speed_Var :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1          1
# 
# pairwise.wilcox.test for Stride_Time_Var :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF  0.002929687         NA
# Pre_op_ON   0.002929687  0.1259766
# 
# pairwise.wilcox.test for Stride_Lenght_Var :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    0.9609375         NA
# Pre_op_ON     0.1259766  0.2490234
# 
# pairwise.wilcox.test for Step_width_Var :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF  0.002929687         NA
# Pre_op_ON   0.002929687  0.4423828
# 
# pairwise.wilcox.test for Step_Lenght_Var :
#   MedON/StimON  Pre_op_OFF
# Pre_op_OFF  0.002929687          NA
# Pre_op_ON   0.720703125 0.002929687
# 
# pairwise.wilcox.test for Step_Time_Var :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF   1.00000000         NA
# Pre_op_ON    0.02929687  0.1259766
# 
# pairwise.wilcox.test for Step_Time_Assym :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1          1
# 
# pairwise.wilcox.test for Step_Lenght_Assym :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    0.7207031         NA
# Pre_op_ON     1.0000000  0.5244141
# 
# pairwise.wilcox.test for Stance_Time_Assym :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1  0.6181641
# 
# pairwise.wilcox.test for Swing_Time_Assym :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1  0.5244141
# 
# pairwise.wilcox.test for Doubble_support_Assym :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    1.0000000         NA
# Pre_op_ON     0.9609375  0.1611328
# 
# pairwise.wilcox.test for Single_Support_Assym :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1  0.4423828
# 
# pairwise.wilcox.test for entropy_ap :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF  0.002929687         NA
# Pre_op_ON   0.002929687          1
# 
# pairwise.wilcox.test for entropy_vert :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    0.1259766         NA
# Pre_op_ON     1.0000000 0.02929687
# 
# pairwise.wilcox.test for entropy_ml :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    0.9609375         NA
# Pre_op_ON     1.0000000          1
# 
# pairwise.wilcox.test for hr_ap :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    1.0000000         NA
# Pre_op_ON     0.1259766  0.4423828
# 
# pairwise.wilcox.test for hr_vert :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF    1.0000000         NA
# Pre_op_ON     0.2490234 0.07324219
# 
# pairwise.wilcox.test for hr_ml :
#   MedON/StimON Pre_op_OFF
# Pre_op_OFF            1         NA
# Pre_op_ON             1          1


# --------------------------------------------
# Predict change in Item 3.11 -----------------------------

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
names(clinical_df)[1] <- "patient"
data.frame(names(clinical_df))
unique(clinical_df$condition)

Delta3.11 <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(patient, `3.11`) %>% rename("Pre_OFF_3.11"="3.11") %>%
  left_join(clinical_df %>% filter(condition=="MedOnStimON") %>% select(patient, `3.11`) %>% rename("Post_ON_3.11"="3.11")) %>%
  mutate(Delta3.11 = as.numeric(Post_ON_3.11)-as.numeric(Pre_OFF_3.11))

Delta3.11 

kine_pre_post_Imp <- fread("kine_pre_post_Imp.txt")
unique(kine_pre_post_Imp$condition)

Delta3.11$patient <- str_replace_all(Delta3.11$patient, "carv", "Carv")


OFF <- kine_pre_post_Imp %>% filter(condition=="Pre_op_OFF") %>% select(-c(condition, patient))
ON <- kine_pre_post_Imp %>% filter(condition=="MedON/StimON") %>% select(-c(condition, patient))

DeltaKin <- ON - OFF

DeltaKin <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaKin)


temp <- DeltaKin %>% inner_join(Delta3.11 %>% select(-c(Pre_OFF_3.11, Post_ON_3.11))) %>% select(-patient)


library(xgboost)
library(caret)



shap.score.rank <- function(xgb_model = xgb_mod, shap_approx = TRUE, 
                            X_train = mydata$train_mm){
  require(xgboost)
  require(data.table)
  shap_contrib <- predict(xgb_model, X_train,
                          predcontrib = TRUE, approxcontrib = shap_approx)
  shap_contrib <- as.data.table(shap_contrib)
  shap_contrib[,BIAS:=NULL]
  cat('make SHAP score by decreasing order\n\n')
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = (mean_shap_score)))
}

# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}


# prep shap data
shap.prep <- function(shap  = shap_result, X_train = mydata$train_mm, top_n){
  require(ggforce)
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
  require(data.table)
  shap_score_sub <- as.data.table(shap$shap_score)
  shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  
  # feature values: the values in the original dataset
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue; 
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2) 
}

plot.shap.summary <- function(data_long){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 1, alpha = 0.5,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="gold1", high="blue4", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") 
  return(plot1)
}






var_importance <- function(shap_result, top_n=10)
{
  var_importance=tibble(var=names(shap_result$mean_shap_score), importance=shap_result$mean_shap_score)
  
  var_importance=var_importance[1:top_n,]
  
  ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
}


names(temp)
dim(temp)

model_hd = xgboost(data = as.matrix(temp[,-26]),
                   nround = 500,
                   # objective = "binary:logistic",
                   label=as.matrix(temp[,26]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(temp[,-26]),
                              shap_approx = F)


var_importance(shap_result, top_n=25)

cor(temp$entropy_ap, temp$Delta3.11) 
cor(temp$`Speed_(ms)`, temp$Delta3.11) 

temp %>% ggplot(aes(entropy_ap, Delta3.11)) +
  geom_point() +
  geom_smooth()

shap_long_hd = shap.prep(X_train = as.matrix(temp[,-26]) , top_n = 25)

plot.shap.summary(data_long = shap_long_hd)


temp





# --------------------------
# Delta Kinematics Pre-OP OFF to ON -> 3.11 Pre OFF to Post ON ---------------------

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
names(clinical_df)[1] <- "patient"
data.frame(names(clinical_df))
unique(clinical_df$condition)
unique(clinical_df$patient)

Delta3.11_PreOFF_PostON <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(patient, `3.11`) %>% rename("Pre_OFF_3.11"="3.11") %>%
  left_join(clinical_df %>% filter(condition=="MedOnStimON") %>% select(patient, `3.11`) %>% rename("Post_ON_3.11"="3.11")) %>%
  mutate(Delta3.11 = as.numeric(Post_ON_3.11)-as.numeric(Pre_OFF_3.11)) # The higher the worst they got, the lower the better

Delta3.11_PreOFF_PostON 

kine_pre_post_Imp <- fread("kine_pre_post_Imp.txt")
unique(kine_pre_post_Imp$condition)

Delta3.11_PreOFF_PostON$patient <- str_replace_all(Delta3.11_PreOFF_PostON$patient, "carv", "Carv")


Kine_PreOFF <- kine_pre_post_Imp %>% filter(condition=="Pre_op_OFF") %>% select(-c(condition, patient))
Kine_PreON <- kine_pre_post_Imp %>% filter(condition=="Pre_op_ON") %>% select(-c(condition, patient))

DeltaKin_PreOP <- Kine_PreON - Kine_PreOFF # The higher the delta the more they move (e.g., higher speed)

DeltaKin_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaKin_PreOP)

temp <- DeltaKin_PreOP %>% inner_join(Delta3.11_PreOFF_PostON %>% select(-c(Pre_OFF_3.11, Post_ON_3.11))) %>% select(-patient)

setDT(temp)

correlations <- transpose(temp[, lapply(.SD, function(x) cor(temp$Delta3.11, x, method="spearman")), .SDcols = -"Delta3.11"])
names(correlations)[1] <- "corr_coef"
correlations <- data.frame(names(temp)[1:25]) %>% bind_cols( correlations )

setDT(correlations)[order(abs(corr_coef))]

#         names.temp..1.25.   corr_coef
#  1:          entropy_vert -0.01405804
#  2: Doubble_support_Assym -0.03748810
#  3:         Step_Time_Var  0.04686013
#  4:        Step_width_Var  0.06560418
#  5:       Step_Lenght_(s) -0.07966222
#  6:             Speed_Var -0.12183634
#  7:  Single_Support_Assym -0.13120836
#  8:     Stride_Length_(m) -0.14526640
#  9:       Step_Lenght_Var -0.14526640
# 10:     Stance_Time_Assym -0.14526640
# 11:      Swing_Time_Assym -0.14526640
# 12:            Speed_(ms) -0.15463843
# 13:        Step_Width_(m)  0.16869647
# 14:               hr_vert -0.18275451
# 15:     Step_Lenght_Assym  0.19212653
# 16:         Step_Time_(s)  0.21087058
# 17:    Cadence_(stepsmin) -0.27647477
# 18:       Step_Time_Assym -0.29990483
# 19:     Stride_Lenght_Var  0.30459084
# 20:       Stride_Time_Var  0.38425306
# 21:       Stride_Time_(s)  0.39362509
# 22:            entropy_ap  0.45454326
# 23:                 hr_ml -0.45922927
# 24:                 hr_ap -0.46391528
# 25:            entropy_ml  0.62792574


correlations <- transpose(temp[, lapply(.SD, function(x) {cor.test(temp$Delta3.11, x, method = "spearman")$p.value}), .SDcols = -"Delta3.11"])

names(correlations)[1] <- "p_value"

correlations <- data.frame(names(temp)[1:25]) %>% bind_cols(correlations)

setDT(correlations)[order(abs(p_value))]



# Using UPDRS Instead

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))
length(unique(clinical_df$patient)) # 18


# MDS UPDRS III
# AXIAL score (3.9, 3.10, 3.11, 3.12)

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% mutate(AXIAL=`3.9`+`3.10`+`3.11`+`3.12`) %>% 
  select(patient, `UPDRS III`, AXIAL, condition)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="OFFpreOP"|condition=="ONpreOP")

clinical_df <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% inner_join(clinical_df) 

UPDRS_PreOFF <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(-c(condition, patient, AXIAL))
UPDRS_PreON <- clinical_df %>% filter(condition=="ONpreOP") %>% select(-c(condition, patient, AXIAL))

DeltaUPDRS_PreOP <- UPDRS_PreON - UPDRS_PreOFF # The more negative the more they benefited 

DeltaUPDRS_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaUPDRS_PreOP)

temp <- DeltaUPDRS_PreOP %>% inner_join(Delta3.11_PreOFF_PostON %>% select(-c(Pre_OFF_3.11, Post_ON_3.11))) %>% select(-patient)

cor(temp$`UPDRS III`, temp$Delta3.11, method="spearman") # 0.3357734

cor.test(temp$`UPDRS III`, temp$Delta3.11, method="spearman") # 0.3357734

# Using AXIAL Score Instead

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))
length(unique(clinical_df$patient)) # 18


# MDS UPDRS III
# AXIAL score (3.9, 3.10, 3.11, 3.12)

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% mutate(AXIAL=`3.9`+`3.10`+`3.11`+`3.12`) %>% 
  select(patient, `UPDRS III`, AXIAL, condition)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="OFFpreOP"|condition=="ONpreOP")

clinical_df <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% inner_join(clinical_df) 

Axial_PreOFF <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(-c(condition, patient,  `UPDRS III`))
Axial_PreON <- clinical_df %>% filter(condition=="ONpreOP") %>% select(-c(condition, patient,  `UPDRS III`))

DeltaAxial_PreOP <- Axial_PreON - Axial_PreOFF # The more negative the more they benefited 

DeltaAxial_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaAxial_PreOP)

temp <- DeltaAxial_PreOP %>% inner_join(Delta3.11_PreOFF_PostON %>% select(-c(Pre_OFF_3.11, Post_ON_3.11))) %>% select(-patient)

cor(temp$AXIAL, temp$Delta3.11, method="spearman") # 0.5696026

cor.test(temp$AXIAL, temp$Delta3.11, method="spearman") # 0.3357734


# Using PreOP Age, disease duration, LEDD  Score Instead

demographicdata <- read_xlsx(path="demographicdata.xlsx", skip=0, trim_ws = TRUE)

names(demographicdata)[1] <- "patient"

data.frame(names(demographicdata))
length(unique(demographicdata$patient)) # 18


demographicdata <- demographicdata %>%
  select(patient,  AgeSurgery, DiseaseDurationSurgery, LEDDpreop) 

names(demographicdata)

demographicdata <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% inner_join(demographicdata)

cor(demographicdata$AgeSurgery, temp$Delta3.11, method="spearman") # 0.5107754

cor(demographicdata$DiseaseDurationSurgery, temp$Delta3.11, method="spearman") # -0.4421805

cor( as.numeric(demographicdata$LEDDpreop), temp$Delta3.11, method="spearman") # -0.1171503









# Compare the correlation  coeficientes
# 
# all_deltas <- temp_kine %>% bind_cols(
#   temp_updrs %>% select(-Delta3.11)
# ) %>% 
#   bind_cols(
#     temp_axial %>%  select(-Delta3.11)
#   )
# 
# 
# install.packages("cocor")
# library(cocor)
# 
# n_subjects <- 11
# cor_entropy_ml <- cor( all_deltas$Delta3.11, all_deltas$entropy_ml, method="spearman")
# cor_axial <- cor( all_deltas$Delta3.11, all_deltas$AXIAL, method="spearman")
# cor_entropy_axial <- cor( all_deltas$AXIAL, all_deltas$entropy_ml, method="spearman")
# 
# result_comparison <- cocor.dep.groups.overlap(r.jk = cor_entropy_ml, 
#                                               r.jh = cor_axial, 
#                                               r.kh = cor_entropy_axial, 
#                                               n = n_subjects)
# 
# result_comparison@pearson1898$p.value
# 
# 
# 
# n_subjects <- 11
# cor_updrs <- cor( all_deltas$Delta3.11, all_deltas$`UPDRS III`, method="spearman")
# cor_hrap <- cor( all_deltas$Delta3.11, all_deltas$hr_ap, method="spearman")
# cor_hrap_updrs <- cor( all_deltas$hr_ap, all_deltas$`UPDRS III`, method="spearman")
# 
# result_comparison <- cocor.dep.groups.overlap(r.jk = cor_updrs, 
#                                               r.jh = cor_hrap, 
#                                               r.kh = cor_hrap_updrs, 
#                                               n = n_subjects)
# 
# result_comparison@pearson1898$p.value



# ---------------

# Delta Kinematics Pre-OP OFF to ON -> 3.10 Pre OFF to Post ON ---------------------

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
names(clinical_df)[1] <- "patient"
data.frame(names(clinical_df))
unique(clinical_df$condition)
unique(clinical_df$patient)

Delta3.10_PreOFF_PostON <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(patient, `3.10`) %>% rename("Pre_OFF_3.10"="3.10") %>%
  left_join(clinical_df %>% filter(condition=="MedOnStimON") %>% select(patient, `3.10`) %>% rename("Post_ON_3.10"="3.10")) %>%
  mutate(Delta3.10 = as.numeric(Post_ON_3.10)-as.numeric(Pre_OFF_3.10)) # The higher the worst they got, the lower the better

Delta3.10_PreOFF_PostON 

kine_pre_post_Imp <- fread("kine_pre_post_Imp.txt")
unique(kine_pre_post_Imp$condition)

Delta3.10_PreOFF_PostON$patient <- str_replace_all(Delta3.10_PreOFF_PostON$patient, "carv", "Carv")


Kine_PreOFF <- kine_pre_post_Imp %>% filter(condition=="Pre_op_OFF") %>% select(-c(condition, patient))
Kine_PreON <- kine_pre_post_Imp %>% filter(condition=="Pre_op_ON") %>% select(-c(condition, patient))

DeltaKin_PreOP <- Kine_PreON - Kine_PreOFF # The higher the delta the more they move (e.g., higher speed)

DeltaKin_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaKin_PreOP)

temp <- DeltaKin_PreOP %>% inner_join(Delta3.10_PreOFF_PostON %>% select(-c(Pre_OFF_3.10, Post_ON_3.10))) %>% select(-patient)

setDT(temp)

correlations <- transpose(temp[, lapply(.SD, function(x) cor(temp$Delta3.10, x, method="spearman")), .SDcols = -"Delta3.10"])
names(correlations)[1] <- "corr_coef"
correlations <- data.frame(names(temp)[1:25]) %>% bind_cols( correlations )

setDT(correlations)[order(abs(corr_coef))]

#         names.temp..1.25.   corr_coef
#  1:       Stride_Time_(s) -0.005280741
#  2: Doubble_support_Assym -0.036965186
#  3:         Step_Time_(s) -0.047526668
#  4:                 hr_ap -0.068649631
#  5:        Step_Width_(m)  0.079211113
#  6:       Step_Time_Assym -0.110895558
#  7:         Step_Time_Var  0.137299263
#  8:          entropy_vert -0.153141485
#  9:            entropy_ap -0.163702967
# 10:     Stride_Lenght_Var  0.179545190
# 11:             Speed_Var -0.248194821
# 12:                 hr_ml  0.248194821
# 13:       Stride_Time_Var  0.285160007
# 14:            entropy_ml -0.306282971
# 15:       Step_Lenght_(s)  0.316844453
# 16:     Stride_Length_(m)  0.316844453
# 17:        Step_width_Var  0.364371120
# 18:            Speed_(ms)  0.374932602
# 19:    Cadence_(stepsmin)  0.374932602
# 20:               hr_vert  0.438301493
# 21:     Step_Lenght_Assym -0.459424456
# 22:       Step_Lenght_Var -0.491108902
# 23:  Single_Support_Assym -0.559758533
# 24:     Stance_Time_Assym -0.575600756
# 25:      Swing_Time_Assym -0.575600756



correlations <- transpose(temp[, lapply(.SD, function(x) {cor.test(temp$Delta3.10, x, method = "spearman")$p.value}), .SDcols = -"Delta3.10"])

names(correlations)[1] <- "p_value"

correlations <- data.frame(names(temp)[1:25]) %>% bind_cols(correlations)

setDT(correlations)[order(abs(p_value))]



# Using UPDRS Instead

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))
length(unique(clinical_df$patient)) # 18


# MDS UPDRS III
# AXIAL score (3.9, 3.10, 3.11, 3.12)

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% mutate(AXIAL=`3.9`+`3.10`+`3.11`+`3.12`) %>% 
  select(patient, `UPDRS III`, AXIAL, condition)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="OFFpreOP"|condition=="ONpreOP")

clinical_df <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% inner_join(clinical_df) 

UPDRS_PreOFF <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(-c(condition, patient, AXIAL))
UPDRS_PreON <- clinical_df %>% filter(condition=="ONpreOP") %>% select(-c(condition, patient, AXIAL))

DeltaUPDRS_PreOP <- UPDRS_PreON - UPDRS_PreOFF # The more negative the more they benefited 

DeltaUPDRS_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaUPDRS_PreOP)

temp <- DeltaUPDRS_PreOP %>% inner_join(Delta3.10_PreOFF_PostON %>% select(-c(Pre_OFF_3.10, Post_ON_3.10))) %>% select(-patient)

cor(temp$`UPDRS III`, temp$Delta3.10, method="spearman") # 0.5809059

cor.test(temp$`UPDRS III`, temp$Delta3.10, method="spearman") # 0.5809059

# Using AXIAL Score Instead

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))
length(unique(clinical_df$patient)) # 18


# MDS UPDRS III
# AXIAL score (3.9, 3.10, 3.11, 3.12)

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% mutate(AXIAL=`3.9`+`3.10`+`3.11`+`3.12`) %>% 
  select(patient, `UPDRS III`, AXIAL, condition)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="OFFpreOP"|condition=="ONpreOP")

clinical_df <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% inner_join(clinical_df) 

Axial_PreOFF <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(-c(condition, patient,  `UPDRS III`))
Axial_PreON <- clinical_df %>% filter(condition=="ONpreOP") %>% select(-c(condition, patient,  `UPDRS III`))

DeltaAxial_PreOP <- Axial_PreON - Axial_PreOFF # The more negative the more they benefited 

DeltaAxial_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaAxial_PreOP)

temp <- DeltaAxial_PreOP %>% inner_join(Delta3.10_PreOFF_PostON %>% select(-c(Pre_OFF_3.10, Post_ON_3.10))) %>% select(-patient)

cor(temp$AXIAL, temp$Delta3.10, method="spearman") # 0.5968023

cor.test(temp$AXIAL, temp$Delta3.10, method="spearman") # 0.5968023


# Using PreOP Age, disease duration, LEDD  Score Instead

demographicdata <- read_xlsx(path="demographicdata.xlsx", skip=0, trim_ws = TRUE)

names(demographicdata)[1] <- "patient"

data.frame(names(demographicdata))
length(unique(demographicdata$patient)) # 18


demographicdata <- demographicdata %>%
  select(patient,  AgeSurgery, DiseaseDurationSurgery, LEDDpreop) 

names(demographicdata)

demographicdata <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% inner_join(demographicdata)

cor(demographicdata$AgeSurgery, temp$Delta3.10, method="spearman") # 0.06864963

cor(demographicdata$DiseaseDurationSurgery, temp$Delta3.10, method="spearman") # -0.3144353

cor( as.numeric(demographicdata$LEDDpreop), temp$Delta3.10, method="spearman") # 0.448863

# ------------
# Friedman and paired wilcox Pre to Post OFF/OFF OFF/ON ON/OFF -------------------------

kine_pre_post_Imp <- fread("kine_pre_post_Imp.txt")
unique(kine_pre_post_Imp$condition)
unique(kine_pre_post_Imp$patient)

kine_pre_post_Imp 

kine_pre_post_Imp <- kine_pre_post_Imp %>% 
  filter(condition=="Pre_op_OFF"|condition=="Pre_op_ON"|condition=="MedOFF/StimON"| condition=="MedON/StimON")


column_names <- names(kine_pre_post_Imp)[3:length(names(kine_pre_post_Imp))]  


for (col_name in column_names) {
  
  friedman_result <- friedman.test(y=kine_pre_post_Imp[[col_name]], 
                                   groups=kine_pre_post_Imp$condition,
                                   blocks=kine_pre_post_Imp$patient)
  
  cat("Friedman Test for", col_name, ":\n")
  print(friedman_result)
  cat("\n")
}



# 
# Friedman Test for Speed_(ms) :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 15.873, df = 3, p-value = 0.001204
# 
# 
# Friedman Test for Cadence_(stepsmin) :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.5455, df = 3, p-value = 0.3149
# 
# 
# Friedman Test for Step_Time_(s) :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.16364, df = 3, p-value = 0.9832
# 
# 
# Friedman Test for Step_Lenght_(s) :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 20.782, df = 3, p-value = 0.0001169
# 
# 
# Friedman Test for Stride_Time_(s) :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 0.27273, df = 3, p-value = 0.9651
# 
# 
# Friedman Test for Stride_Length_(m) :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 19.691, df = 3, p-value = 0.0001967
# 
# 
# Friedman Test for Step_Width_(m) :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 5.4, df = 3, p-value = 0.1447
# 
# 
# Friedman Test for Speed_Var :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 5.0727, df = 3, p-value = 0.1665
# 
# 
# Friedman Test for Stride_Time_Var :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 21.545, df = 3, p-value = 0.00008109
# 
# 
# Friedman Test for Stride_Lenght_Var :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 9.6545, df = 3, p-value = 0.02174
# 
# 
# Friedman Test for Step_width_Var :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 27.327, df = 3, p-value = 0.000005027
# 
# 
# Friedman Test for Step_Lenght_Var :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 17.509, df = 3, p-value = 0.0005552
# 
# 
# Friedman Test for Step_Time_Var :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 8.1273, df = 3, p-value = 0.04345
# 
# 
# Friedman Test for Step_Time_Assym :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.1091, df = 3, p-value = 0.3751
# 
# 
# Friedman Test for Step_Lenght_Assym :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.3273, df = 3, p-value = 0.3439
# 
# 
# Friedman Test for Stance_Time_Assym :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.3273, df = 3, p-value = 0.3439
# 
# 
# Friedman Test for Swing_Time_Assym :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.3273, df = 3, p-value = 0.3439
# 
# 
# Friedman Test for Doubble_support_Assym :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.2182, df = 3, p-value = 0.3592
# 
# 
# Friedman Test for Single_Support_Assym :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.6545, df = 3, p-value = 0.3013
# 
# 
# Friedman Test for entropy_ap :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 18.6, df = 3, p-value = 0.0003307
# 
# 
# Friedman Test for entropy_vert :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 11.836, df = 3, p-value = 0.007965
# 
# 
# Friedman Test for entropy_ml :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 3.2182, df = 3, p-value = 0.3592
# 
# 
# Friedman Test for hr_ap :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 6.6, df = 3, p-value = 0.0858
# 
# 
# Friedman Test for hr_vert :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 9.8727, df = 3, p-value = 0.01968
# 
# 
# Friedman Test for hr_ml :
# 
# 	Friedman rank sum test
# 
# data:  kine_pre_post_Imp[[col_name]], kine_pre_post_Imp$condition and kine_pre_post_Imp$patient
# Friedman chi-squared = 1.8, df = 3, p-value = 0.6149
# 



for (col_name in column_names) {
  
  result <- pairwise.wilcox.test(kine_pre_post_Imp[[col_name]], 
                                 kine_pre_post_Imp$condition,
                                 p.adj = "bonferroni", paired=T)
  
  cat("pairwise.wilcox.test for", col_name, ":\n")
  print(result$p.value)
  cat("\n")
}


# pairwise.wilcox.test for Speed_(ms) :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON   0.111328125           NA         NA
# Pre_op_OFF     1.000000000   1.00000000         NA
# Pre_op_ON      0.005859375   0.02929687 0.05859375
# 
# pairwise.wilcox.test for Cadence_(stepsmin) :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON             1           NA         NA
# Pre_op_OFF               1            1         NA
# Pre_op_ON                1            1          1
# 
# pairwise.wilcox.test for Step_Time_(s) :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON             1           NA         NA
# Pre_op_OFF               1            1         NA
# Pre_op_ON                1            1          1
# 
# pairwise.wilcox.test for Step_Lenght_(s) :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON   0.017578125           NA         NA
# Pre_op_OFF     1.000000000  1.000000000         NA
# Pre_op_ON      0.005859375  0.005859375 0.04101562
# 
# pairwise.wilcox.test for Stride_Time_(s) :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON             1           NA         NA
# Pre_op_OFF               1            1         NA
# Pre_op_ON                1            1          1
# 
# pairwise.wilcox.test for Stride_Length_(m) :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON   0.058593750           NA         NA
# Pre_op_OFF     1.000000000  1.000000000         NA
# Pre_op_ON      0.005859375  0.005859375 0.02929687
# 
# pairwise.wilcox.test for Step_Width_(m) :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON    0.05859375           NA         NA
# Pre_op_OFF      1.00000000    1.0000000         NA
# Pre_op_ON       1.00000000    0.8847656          1
# 
# pairwise.wilcox.test for Speed_Var :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     1.0000000           NA         NA
# Pre_op_OFF       1.0000000            1         NA
# Pre_op_ON        0.3222656            1          1
# 
# pairwise.wilcox.test for Stride_Time_Var :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON   0.005859375           NA         NA
# Pre_op_OFF     0.498046875  0.005859375         NA
# Pre_op_ON      1.000000000  0.005859375  0.2519531
# 
# pairwise.wilcox.test for Stride_Lenght_Var :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     1.0000000           NA         NA
# Pre_op_OFF       1.0000000    1.0000000         NA
# Pre_op_ON        0.4042969    0.2519531  0.4980469
# 
# pairwise.wilcox.test for Step_width_Var :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON   1.000000000           NA         NA
# Pre_op_OFF     0.005859375  0.005859375         NA
# Pre_op_ON      0.005859375  0.005859375  0.8847656
# 
# pairwise.wilcox.test for Step_Lenght_Var :
#              MedOFF/StimON MedON/StimON  Pre_op_OFF
# MedON/StimON    1.00000000           NA          NA
# Pre_op_OFF      0.02929687  0.005859375          NA
# Pre_op_ON       0.88476562  1.000000000 0.005859375
# 
# pairwise.wilcox.test for Step_Time_Var :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON      1.000000           NA         NA
# Pre_op_OFF        1.000000   1.00000000         NA
# Pre_op_ON         0.609375   0.05859375  0.2519531
# 
# pairwise.wilcox.test for Step_Time_Assym :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     1.0000000           NA         NA
# Pre_op_OFF       1.0000000            1         NA
# Pre_op_ON        0.3222656            1          1
# 
# pairwise.wilcox.test for Step_Lenght_Assym :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     1.0000000           NA         NA
# Pre_op_OFF       0.7382812            1         NA
# Pre_op_ON        1.0000000            1          1
# 
# pairwise.wilcox.test for Stance_Time_Assym :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     0.7382812           NA         NA
# Pre_op_OFF       1.0000000            1         NA
# Pre_op_ON        0.6093750            1          1
# 
# pairwise.wilcox.test for Swing_Time_Assym :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     0.7382812           NA         NA
# Pre_op_OFF       1.0000000            1         NA
# Pre_op_ON        0.6093750            1          1
# 
# pairwise.wilcox.test for Doubble_support_Assym :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     1.0000000           NA         NA
# Pre_op_OFF       0.8847656            1         NA
# Pre_op_ON        1.0000000            1  0.3222656
# 
# pairwise.wilcox.test for Single_Support_Assym :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     0.6093750           NA         NA
# Pre_op_OFF       1.0000000            1         NA
# Pre_op_ON        0.4980469            1  0.8847656
# 
# pairwise.wilcox.test for entropy_ap :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON    0.49804687           NA         NA
# Pre_op_OFF      0.05859375  0.005859375         NA
# Pre_op_ON       0.14648437  0.005859375          1
# 
# pairwise.wilcox.test for entropy_vert :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     1.0000000           NA         NA
# Pre_op_OFF       0.1933594    0.2519531         NA
# Pre_op_ON        1.0000000    1.0000000 0.05859375
# 
# pairwise.wilcox.test for entropy_ml :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON    1.00000000           NA         NA
# Pre_op_OFF      0.04101562            1         NA
# Pre_op_ON       1.00000000            1          1
# 
# pairwise.wilcox.test for hr_ap :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON             1           NA         NA
# Pre_op_OFF               1    1.0000000         NA
# Pre_op_ON                1    0.2519531  0.8847656
# 
# pairwise.wilcox.test for hr_vert :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON     1.0000000           NA         NA
# Pre_op_OFF       1.0000000    1.0000000         NA
# Pre_op_ON        0.1464844    0.4980469  0.1464844
# 
# pairwise.wilcox.test for hr_ml :
#              MedOFF/StimON MedON/StimON Pre_op_OFF
# MedON/StimON             1           NA         NA
# Pre_op_OFF               1            1         NA
# Pre_op_ON                1            1          1

# -----------------


# Multiple comparisions of % FOG/Gait Impait across conditions ---------------

# Gait Impair
num <-  c( 15, 1, 18, 14, 17, 14)
names(num) <- c("PreOFF", "PreON", "PostOFF", "PostONOFF", "PostOFFON", "PostONON")
den <- c( 18, 18, 18, 18, 18, 18 )

prop.test(num, den)

# 	6-sample test for equality of proportions without continuity
# 	correction
# 
# data:  num out of den
# X-squared = 53.976, df = 5, p-value = 0.0000000002119
# alternative hypothesis: two.sided
# sample estimates:
#     prop 1     prop 2     prop 3     prop 4     prop 5     prop 6 
# 0.83333333 0.05555556 1.00000000 0.77777778 0.94444444 0.77777778 

pairwise.prop.test(num, den)

# 	Pairwise comparisons using Pairwise comparison of proportions 
# 
# data:  num out of den 
# 
#           PreOFF  PreON     PostOFF PostONOFF PostOFFON
# PreON     0.00017 -         -       -         -        
# PostOFF   1.00000 0.0000014 -       -         -        
# PostONOFF 1.00000 0.00060   1.00000 -         -        
# PostOFFON 1.00000 0.0000080 1.00000 1.00000   -        
# PostONON  1.00000 0.00060   1.00000 1.00000   1.00000  
# 
# P value adjustment method: holm


# FOG


# Gait Impair
num <-  c( 13, 2, 13, 7, 7, 5)
names(num) <- c("PreOFF", "PreON", "PostOFF", "PostONOFF", "PostOFFON", "PostONON")
den <- c( 18, 18, 18, 18, 18, 18 )

prop.test(num, den)

# 	6-sample test for equality of proportions without continuity correction
# 
# data:  num out of den
# X-squared = 21.886, df = 5, p-value = 0.0005503
# alternative hypothesis: two.sided
# sample estimates:
#    prop 1    prop 2    prop 3    prop 4    prop 5    prop 6 
# 0.7222222 0.1111111 0.7222222 0.3888889 0.3888889 0.2777778 

pairwise.prop.test(num, den)

# 	Pairwise comparisons using Pairwise comparison of proportions 
# 
# data:  num out of den 
# 
#           PreOFF PreON PostOFF PostONOFF PostOFFON
# PreON     0.011  -     -       -         -        
# PostOFF   1.000  0.011 -       -         -        
# PostONOFF 1.000  1.000 1.000   -         -        
# PostOFFON 1.000  1.000 1.000   1.000     -        
# PostONON  0.255  1.000 0.255   1.000     1.000    
# 
# P value adjustment method: holm 



# --------------
# Delta Kinematics or UPDRS III itself (Pre-OP OFF to ON) -> (UPDRS III Pre OFF to Post ON) ---------------------

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
names(clinical_df)[1] <- "patient"
data.frame(names(clinical_df))
unique(clinical_df$condition)
unique(clinical_df$patient)

DeltaUPDRS_PreOFF_PostON <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(patient, `UPDRS III`) %>% rename("Pre_OFF_UPDRS"="UPDRS III") %>%
  left_join(clinical_df %>% filter(condition=="MedOnStimON") %>% select(patient, `UPDRS III`) %>% rename("Post_ON_UPDRS"="UPDRS III")) %>%
  mutate(DeltaUPDRS = as.numeric(Post_ON_UPDRS)-as.numeric(Pre_OFF_UPDRS)) # The higher the worst they got, the lower the better

DeltaUPDRS_PreOFF_PostON 

mean(DeltaUPDRS_PreOFF_PostON$DeltaUPDRS)

kine_pre_post_Imp <- fread("kine_pre_post_Imp.txt")
unique(kine_pre_post_Imp$condition)

DeltaUPDRS_PreOFF_PostON$patient <- str_replace_all(DeltaUPDRS_PreOFF_PostON$patient, "carv", "Carv")


Kine_PreOFF <- kine_pre_post_Imp %>% filter(condition=="Pre_op_OFF") %>% select(-c(condition, patient))
Kine_PreON <- kine_pre_post_Imp %>% filter(condition=="Pre_op_ON") %>% select(-c(condition, patient))

DeltaKin_PreOP <- Kine_PreON - Kine_PreOFF # The higher the delta the more they move (e.g., higher speed)

DeltaKin_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaKin_PreOP)

temp <- DeltaKin_PreOP %>% inner_join(DeltaUPDRS_PreOFF_PostON %>% select(-c(Pre_OFF_UPDRS , Post_ON_UPDRS ))) %>% select(-patient)

setDT(temp)

temp_kine <- temp

correlations <- transpose(temp[, lapply(.SD, function(x) cor(temp$DeltaUPDRS, x, method="spearman")), .SDcols = -"DeltaUPDRS"])
names(correlations)[1] <- "corr_coef"
correlations <- data.frame(names(temp)[1:25]) %>% bind_cols( correlations )

setDT(correlations)[order(abs(corr_coef))]

# names.temp..1.25.   corr_coef
# 1:       Step_Lenght_Var  0.00000000
# 2:            Speed_(ms)  0.02277910
# 3:       Stride_Time_Var  0.02733492
# 4:       Step_Lenght_(s) -0.03189074
# 5:                 hr_ml  0.03189074
# 6:        Step_width_Var  0.04100238
# 7:     Stride_Length_(m) -0.10478387
# 8:          entropy_vert -0.10478387
# 9:         Step_Time_Var  0.11845133
# 10:            entropy_ap  0.12300715
# 11:     Stride_Lenght_Var  0.13211879
# 12:         Step_Time_(s) -0.14578626
# 13:       Stride_Time_(s) -0.15489790
# 14:               hr_vert  0.17767700
# 15:       Step_Time_Assym -0.19590028
# 16:                 hr_ap  0.19590028
# 17: Doubble_support_Assym  0.21412356
# 18:            entropy_ml -0.23234684
# 19:             Speed_Var -0.29612833
# 20:        Step_Width_(m)  0.31890743
# 21:     Step_Lenght_Assym -0.39180056
# 22:    Cadence_(stepsmin)  0.41913548
# 23:     Stance_Time_Assym -0.52847518
# 24:      Swing_Time_Assym -0.52847518
# 25:  Single_Support_Assym -0.57858920



correlations <- transpose(temp[, lapply(.SD, function(x) {cor.test(temp$DeltaUPDRS, x, method = "spearman")$p.value}), .SDcols = -"DeltaUPDRS"])

names(correlations)[1] <- "p_value"

correlations <- data.frame(names(temp)[1:25]) %>% bind_cols(correlations)

setDT(correlations)[order(abs(p_value))]





# Using UPDRS Instead

clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))
length(unique(clinical_df$patient)) # 18


# MDS UPDRS III
# AXIAL score (3.9, 3.10, 3.11, 3.12)

clinical_df <- clinical_df %>%
  select(patient, condition) %>%
  bind_cols(
    clinical_df %>% 
      select(-c(patient, Worst_side , condition)) %>%
      mutate_if(is.character,as.numeric)
  )

clinical_df <- clinical_df %>% mutate(AXIAL=`3.9`+`3.10`+`3.11`+`3.12`) %>% 
  select(patient, `UPDRS III`, AXIAL, condition)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="OFFpreOP"|condition=="ONpreOP")

clinical_df <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% inner_join(clinical_df) 

UPDRS_PreOFF <- clinical_df %>% filter(condition=="OFFpreOP") %>% select(-c(condition, patient, AXIAL))
UPDRS_PreON <- clinical_df %>% filter(condition=="ONpreOP") %>% select(-c(condition, patient, AXIAL))

DeltaUPDRS_PreOP <- UPDRS_PreON - UPDRS_PreOFF # The more negative the more they benefited 

DeltaUPDRS_PreOP <- kine_pre_post_Imp %>% select(patient) %>% distinct() %>% bind_cols(DeltaUPDRS_PreOP)

temp <- DeltaUPDRS_PreOP %>% inner_join(DeltaUPDRS_PreOFF_PostON %>% select(-c(Pre_OFF_UPDRS, Post_ON_UPDRS))) %>% select(-patient)

cor(temp$`UPDRS III`, temp$DeltaUPDRS, method="spearman") # 0.5471394

cor.test(temp$`UPDRS III`, temp$DeltaUPDRS, method="spearman") # -value = 0.08152

# -----------
# SWS and # FOG events NEW DATA July -------

clinical_df <- read_xlsx(path="clinical_table_new_july30.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

clinical_df <- clinical_df %>% select(patient, condition,  SWSFOG_) 

sum(is.na(clinical_df))

clinical_df$SWSFOG_ <- as.numeric(clinical_df$SWSFOG_)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="MedOFFStimOFF"|condition=="MedOnStimOFF"|condition=="MedOFFStimON"|condition=="MedOnStimON")

clinical_df %>% drop_na() %>% group_by(condition) %>% summarise(mean=mean(SWSFOG_))

clinical_df %>%
  ggplot(aes(x = condition, y = SWSFOG_, color = factor(condition), group = patient)) +
  geom_jitter(position = position_dodge(width = 0.2), size=2, alpha=0.6, show.legend = F) +
  geom_line(position = position_dodge(width = 0.2), show.legend = F) +
  theme_minimal() +
  # ylim(0,10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c( "#0087CA", "#001932", "#A8234C",  "#C6B615")) +
  scale_fill_manual(values=c("#0087CA", "#001932", "#A8234C", "#C6B615")) +
  xlab("\n Condiiton") + ylab("# FOG Events  \n")






clinical_df <- read_xlsx(path="clinical_table_new_july30.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18

clinical_df <- clinical_df %>% select(patient, condition,  SWSTime_) 

sum(is.na(clinical_df))

clinical_df$SWSTime_ <- as.numeric(clinical_df$SWSTime_)

unique(clinical_df$condition)

clinical_df <- clinical_df %>% filter(condition=="MedOFFStimOFF"|condition=="MedOnStimOFF"|condition=="MedOFFStimON"|condition=="MedOnStimON")

sum(is.na(clinical_df))

clinical_df %>%
  ggplot(aes(x = condition, y = SWSTime_, color = factor(condition), group = patient)) +
  geom_jitter(position = position_dodge(width = 0.2), size=2, alpha=0.6, show.legend = F) +
  geom_line(position = position_dodge(width = 0.2), show.legend = F) +
  theme_minimal() +
  ylim(0,170) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c( "#0087CA", "#001932", "#A8234C", "#C6B615")) +
  scale_fill_manual(values=c("#0087CA", "#001932", "#A8234C", "#C6B615")) +
  xlab("\n Condiiton") + ylab("SWS Time (s)  \n")














clinical_df <- read_xlsx(path="clinical_table_new_july30.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18


unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  select(patient, condition, SWSFOG_) %>% mutate(SWSFOG_=as.numeric(SWSFOG_))


unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  mutate(condition=ifelse(condition=="MedOnStimOFF", "2- MedONStimOFF",
                          ifelse(condition=="MedOFFStimON", "3- MedOFFStimON", 
                                 ifelse(condition=="MedOFFStimOFF", "1- MedOFFStimOFF", "4- MedONStimON")))) %>%
  mutate(condition=factor(condition, levels=c( "1- MedOFFStimOFF", "2- MedONStimOFF", "3- MedOFFStimON",  "4- MedONStimON")))

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
                  y = SWSFOG_, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3, height = 0.1) +
  labs(title = "# FOG Events ") +
  theme_minimal() +
  #ylim(0,10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c( "#A8234C",  "#00639B", "#001932", "#C6B615")) +
  scale_fill_manual(values=c( "#A8234C",  "#00639B",  "#001932", "#C6B615")) +
  xlab("\n Condiiton") + ylab("# FOG Events \n [ x\u0305 \u00B1 \u03C3 ] \n")






clinical_df <- read_xlsx(path="clinical_table copy2years.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

names(clinical_df)[1] <- "patient"

data.frame(names(clinical_df))

length(unique(clinical_df$patient)) # 18


unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  select(patient, condition, SWSTime_) %>% mutate(SWSTime_=as.numeric(SWSTime_))


unique(clinical_df$condition)


clinical_df <- clinical_df %>%
  mutate(condition=ifelse(condition=="MedOnStimOFF", "2- MedONStimOFF",
                          ifelse(condition=="MedOFFStimON", "3- MedOFFStimON", 
                                 ifelse(condition=="MedOFFStimOFF", "1- MedOFFStimOFF", "4- MedONStimON")))) %>%
  mutate(condition=factor(condition, levels=c( "1- MedOFFStimOFF", "2- MedONStimOFF", "3- MedOFFStimON",  "4- MedONStimON")))

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
                  y = SWSTime_, color = condition),  show.legend = FALSE, size=3, alpha=0.7, width=0.3,  height = 0.1) +
  labs(title = "SWS Time (s) ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c( "#A8234C",  "#00639B", "#001932",  "#C6B615")) +
  scale_fill_manual(values=c( "#A8234C",  "#00639B",  "#001932",  "#C6B615")) +
  xlab("\n Condiiton") + ylab("SWS Time (s) \n [ x\u0305 \u00B1 \u03C3 ] \n")


# ----------