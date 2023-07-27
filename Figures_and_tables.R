# Key Figures SSC

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(ggallin)
library(purrr)
library(patchwork)
library(forcats)
library(multcompView)
library(gmodels)

# Files and directories
setwd("C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/SSCrepository")
filename <- "no-experience_defined-by-similarity_0.45_0.45_1.05.txt"
start <- 3 #beginning of simulation = beginning of year 3
end <- 24 #end of simulation = end of year 23

# Settings
options(scipen = 999)

# Data processing
mydata <- paste0("./Netlogo_Output/", filename) %>%
  read.table(sep=";", header=TRUE, fill = TRUE)

dat_long <- mydata %>%
  select(-c(incomegap:foodgap)) %>%
  pivot_longer(cols = c(income_outcome_t1start:food_outcome_t3, income_threshold_mean:food_threshold_t3), #income_outcome_t1start:food_threshold_t3,
               names_to = "aspiration_variable",
               values_to = "aspiration_value") %>%
  mutate(aspiration_dimension = as.character(map(strsplit(aspiration_variable, split = "_"), 1))) %>%
  mutate(aspiration_variable = paste(as.character(map(strsplit(aspiration_variable, split = "_"), 2)),
                                     as.character(map(strsplit(aspiration_variable, split = "_"), 3)),
                                     sep = "_")) %>%
  pivot_wider(names_from = aspiration_variable, values_from = aspiration_value) %>%
  mutate(gap = threshold_t1start - outcome_t1end) %>%
  distinct()

initial_wealth <- dat_long %>% filter(year == start) %>% 
  mutate(initial_wealth = savings) %>%
  select(initial_wealth, household) 

dat_long <- full_join(dat_long, initial_wealth)

dat_long <- dat_long %>%
  mutate(wealth_group = ifelse(initial_wealth < 0, "group 1 (<0)", "")) %>%
  mutate(wealth_group = ifelse(initial_wealth < 50000 & initial_wealth >= 0, "group 2 (<50k)", wealth_group)) %>%
  mutate(wealth_group = ifelse(initial_wealth < 250000 & initial_wealth >= 50000, "group 3 (<250k)", wealth_group)) %>%
  mutate(wealth_group = ifelse(initial_wealth >= 250000, "group 4 (>250k)", wealth_group)) 

model=lm(dat_long$savings[dat_long$year == 3] ~ dat_long$wealth_group[dat_long$year == 3])
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'dat_long$wealth_group[dat_long$year == 3]', conf.level=0.95)
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

LABELS <- generate_label_df(TUKEY , "dat_long$wealth_group[dat_long$year == 3]")
colnames(LABELS)[2] <- "wealth_group"

dat_long <- full_join(dat_long, as.data.frame(LABELS))

wealths <- dat_long %>% select(household, savings, wealth_group, wealth_group, year, Letters) %>% filter(year == 3) %>% distinct() %>% 
  group_by(wealth_group, Letters) %>% 
  dplyr::summarise(n = n(),
                   mean = mean(savings),
                   median = median(savings),
                   sd = sd(savings),
                   mean_euro = mean(savings)*0.017,
                   sd_euro = sd(savings)*0.017)

axlabels <- c(paste0("N: ", wealths$n[1], "\nmean: ", round(wealths$mean[1]/1000), "\nsd: ", round(wealths$sd[1]/1000)),
                   paste0("N: ", wealths$n[2], "\nmean: ", round(wealths$mean[2]/1000), "\nsd: ", round(wealths$sd[2]/1000)),
                   paste0("N: ", wealths$n[3], "\nmean: ", round(wealths$mean[3]/1000), "\nsd: ", round(wealths$sd[3]/1000)),
                   paste0("N: ", wealths$n[4], "\nmean: ", round(wealths$mean[4]/1000), "\nsd: ", round(wealths$sd[4]/1000)))

# Figure 2
dat_long[dat_long$year==start,] %>%
  select(wealth_group, savings, household) %>%
  distinct() %>% ggplot(aes(x = wealth_group, y = savings/1000, fill = wealth_group)) + 
  geom_hline(yintercept = 0, colour = "indianred4", linetype = "dashed") +
  geom_hline(yintercept = 50, colour = "khaki", linetype = "dashed") +
  geom_hline(yintercept = 250, colour = "lightgreen", linetype = "dashed") +
  geom_boxplot(width = 0.3, alpha = 0.5) +
  geom_point(position= "jitter", alpha = 0.7, aes(color = wealth_group), size = 2) + 
  ylab("Initial wealth (1000 Ethiopian birr)") + xlab("") +
  scale_y_continuous(breaks = c(-250, -100, -50, 0, 50, 100, 250, 500, 1000)) +
  scale_x_discrete(labels=axlabels) +
  scale_fill_manual("Wealth groups", values = c("indianred4", "khaki", "lightgreen", "seagreen4")) +
  scale_colour_manual("Wealth groups", values = c("indianred4", "khaki", "lightgreen", "seagreen4")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22),
        legend.position = c(0.35,0.7)) 

#Table 3
wealths
quantile(dat_long$savings[dat_long$year %in% c(start,end)], probs = c(0.1,0.25,0.5,0.75, 0.9))

# Figure 3
set.seed(596)
dat_long[dat_long$year %in% c(start, end),] %>%
  select(household, year, savings, wealth_group) %>%
  distinct() %>%
  group_by(household) %>%
  pivot_wider(names_from = year, values_from = savings) %>%
  mutate(start = as.numeric(as.character(`3`)),
         end = as.numeric(as.character(`24`)),
         household = as.factor(household)) %>%
  mutate(household = fct_reorder(household, start, .desc = T),
         arrow = ifelse(start < end, "up", "down"),
         arrow = ifelse(start == end, "same", arrow)) %>%
  ggplot(aes(group = wealth_group,  colour = arrow, fill = arrow, x = wealth_group)) + 
  geom_hline(yintercept = 0) +
  ylab("Cumulated wealth (1000 ETB)") +
  xlab("") +
  geom_linerange(aes(y = start/1000, ymin = start/1000, ymax = end/1000), position = position_jitter(seed = 123, width =0.45), size = 1.5, alpha = 0.2) +
  geom_point(aes(y = start/1000), size = 4, position = position_jitter(seed = 123, width =0.45), shape = 21, alpha = 0.8) +
  geom_point(aes(y = end/1000, shape = arrow), size = 4, position = position_jitter(seed = 123, width =0.45), alpha = 0.8) + 
  scale_shape_manual(values = c(25,24)) +
  scale_colour_manual(values = c("deeppink3", "black")) +
  scale_fill_manual(values = c("#FF99CC", "grey")) +
  scale_y_continuous(breaks = c(-500, -250, 0, 250, 500, 1000, 1500, 2000, 3000)) +
  scale_x_discrete(labels = c("group 1", "group 2", "group 3", "group 4")) +
  coord_cartesian(ylim=c(NA,3050)) +
  theme_classic() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21, hjust=0.95),
        legend.position = "none")

dat_long[dat_long$year %in% c(start, end),] %>%
  select(household, year, savings, wealth_group) %>%
  distinct() %>%
  group_by(household) %>%
  pivot_wider(names_from = year, values_from = savings) %>%
  mutate(start = as.numeric(as.character(`3`)),
         end = as.numeric(as.character(`24`)),
         household = as.factor(household)) %>%
  mutate(household = fct_reorder(household, start, .desc = T),
         arrow = ifelse(start < end, "up", "down"),
         arrow = ifelse(start == end, "same", arrow)) %>% 
  ggplot(aes(group = wealth_group,  colour = arrow, fill = arrow, x = wealth_group)) + 
  geom_hline(yintercept = 0) +
  ylab("") +
  xlab("") +
  geom_linerange(aes(y = start/1000, ymin = start/1000, ymax = end/1000), position = position_jitter(seed = 123, width =0.45), size = 2, alpha = 0.2) +
  geom_point(aes(y = start/1000), shape = 21, size = 4, position = position_jitter(seed = 123, width =0.45), alpha = 0.8) +
  geom_point(aes(y = end/1000, shape = arrow), size = 4, position = position_jitter(seed = 123, width =0.45), alpha = 0.8) + 
  scale_shape_manual(values = c(25,24)) +
  scale_colour_manual(values = c("deeppink", "black")) +
  scale_fill_manual(values = c("#FF99CC", "grey")) +
  scale_y_continuous(breaks = c(1500, 2000, 7000, 10000, 15000, 20000, 50000)) +
  scale_x_discrete(labels = c("group 1", "group 2", "group 3", "group 4")) +
  coord_cartesian(ylim=c(3850,NA)) +
  theme_classic() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21, hjust=0.95),
        legend.position = "none")

# Figure 4
last_savings<- dat_long %>% 
  filter(year %in% c(3,23)) %>% 
  select(household, year,  savings) %>%
  distinct() %>%
  pivot_wider(names_prefix = "savings_year", names_from = year, values_from = savings) 

last_thresholds<- dat_long %>% 
  filter(year %in% c(3,23)) %>% 
  select(household, year, aspiration_dimension, threshold_t1end) %>%
  distinct() %>%
  pivot_wider(names_prefix = "threshold_year", names_from = year, values_from = threshold_t1end) 

dat_th <- full_join(last_thresholds, dat_long)  %>% 
  select(household, year, wealth_group,  threshold_t1end, aspiration_dimension) %>%
  distinct() %>%
  group_by(year, wealth_group, aspiration_dimension) %>%
  dplyr::summarise(threshold_lowCI = ci(threshold_t1end)[2],
                   mean_threshold_bygroup = mean(threshold_t1end),
                   threshold_highCI = ci(threshold_t1end)[3])

dat_sav <- full_join(last_savings, dat_long) %>%
  group_by(year, wealth_group) %>%
  dplyr::summarise(savings_lowCI = ci(savings)[2],
                   mean_savings_bygroup = mean(savings),
                   savings_highCI = ci(savings)[3])

dat_ths <- full_join(dat_sav, dat_th)

hospital_names <- list(
  'food'="Food self-sufficiency",
  'income'="Income",
  'leisure'="Leisure"
)

hospital_labeller <- function(variable,value){
  return(hospital_names[value])
}

ggplot(dat_long[dat_long$year >= start,], aes(x = savings/1000, y = threshold_t1start)) + 
  ylab("Aspiration thresholds") +
  xlab("Cumulated wealth per household (1000 ETB)") +
  geom_path(size = 1, alpha = 0.1,
            aes(group = household, colour = wealth_group)) +
  geom_point(data = dat_long[dat_long$year %in% c(start,end),], alpha = 0.1,
             aes(colour = wealth_group, fill = wealth_group, shape = as.factor(year)), size = 3) +
  geom_ribbon(data = dat_ths[dat_ths$year >= start,], alpha = 0.1,linetype = "dashed", size =0.5,
              aes(ymin = threshold_lowCI, ymax = threshold_highCI, y = mean_threshold_bygroup, 
                  x = mean_savings_bygroup/1000, fill = wealth_group, color = wealth_group)) +
  geom_path(data = dat_ths[dat_ths$year >= start,], lwd = 3, alpha = 0.8,
            aes(group = wealth_group, color = wealth_group, x = mean_savings_bygroup/1000, y = mean_threshold_bygroup)) +
  geom_point(data = dat_ths[dat_ths$year %in% c(start,end),], 
             aes(colour = wealth_group, fill = wealth_group, x = mean_savings_bygroup/1000, y = mean_threshold_bygroup,
                 shape = as.factor(year)), size = 6) +
  scale_shape_manual(values = c(20,24), name = "time", labels = c("0","20")) +
  scale_color_manual(labels = c("group 1", "group 2", "group 3", "group 4"), values = darken(c("indianred4", "khaki", "lightgreen", "seagreen4")), name = "wealth group") +
  scale_fill_manual(labels = c("group 1", "group 2", "group 3", "group 4"), values = c("indianred4", "khaki", "lightgreen", "seagreen4"), name = "wealth group") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = pseudolog10_trans, breaks = c(-100000,-10000, -1000, -100, -10, 0, 10, 100, 100, 1000, 10000, 100000, 1000000)) + #, 
  theme_classic() + theme(legend.position="bottom",
                          axis.text = element_text(size = 18, angle = 90, hjust = 0.95, vjust = 0.2),
                          axis.title = element_text(size = 21),
                          legend.text = element_text(size = 18),
                          legend.title = element_text(size = 21),
                          strip.text = element_text(size = 18)) + 
  facet_wrap(~aspiration_dimension, scales = "free", labeller=hospital_labeller)
