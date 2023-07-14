# Title:  Foraging valor linked with aggression: selection against completely abandoning aggression in the high-elevation ant Tetramorium alpestre?
#Marie-Luise Contala, Patrick Krapf, Florian M. Steiner, Birigt C. Schlick-Steiner

#Please note, the data can be downloaded from the journal's homepage.

# load and install packages ####
packages_needed <- c("ggplot2",
                     "tidyverse",
                     "dplyr",
                     "report",
                     "DHARMa",
                     "ggdist", 
                     "ggstance",
                     "arm",
                     "jtools",
                     "MuMIn",
                     "ggpubr",
                     "ggsignif",
                     "lme4",
                     "lmerTest"
)

pk_to_install <- packages_needed [!( packages_needed %in% rownames(installed.packages())  )]
if(length(pk_to_install)>0 ){
  install.packages(pk_to_install,repos="http://cran.r-project.org")
}

lapply(packages_needed, require, character.only = TRUE)
#Now, all libraries should be loaded. You can check this, by using the search() function
search()

# Mean differences tests
## Mean differences tests- Boldness  ####
setwd("") #set your working directory
freq1 <- read.csv("boldness_20221222.csv", sep = ",",header=TRUE)
data_Kue <- subset(freq1, pop=="K")
data_Pen <- subset(freq1, pop=="P")

sink("Boldness_Mean_test.txt")
print("##### Mean ants ####")
shapiro.test(data_Kue$MeanAnts_crossed_junction)
shapiro.test(data_Pen$MeanAnts_crossed_junction)
wilcox.test(data_Kue$MeanAnts_crossed_junction, data_Pen$MeanAnts_crossed_junction, alternative = "less") #ns
#var.test(data_Kue$MeanAnts_crossed_junction, data_Pen$MeanAnts_crossed_junction)
#t.test(data_Kue$MeanAnts_crossed_junction, data_Pen$MeanAnts_crossed_junction, alternative = "less") #ns

print("##### Max ants ####")
shapiro.test(data_Kue$MaxAnts_crossed_junction)
shapiro.test(data_Pen$MaxAnts_crossed_junction)
wilcox.test(data_Kue$MaxAnts_crossed_junction, data_Pen$MaxAnts_crossed_junction, alternative = "less") #ns
#var.test(data_Kue$MaxAnts_crossed_junction, data_Pen$MaxAnts_crossed_junction)
#t.test(data_Kue$MaxAnts_crossed_junction, data_Pen$MaxAnts_crossed_junction, alternative = "less") #ns

print("##### MeanAnts_depositing_pheromons ####")
shapiro.test(data_Kue$MeanAnts_depositing_pheromons)
shapiro.test(data_Pen$MeanAnts_depositing_pheromons)
wilcox.test(data_Kue$MeanAnts_depositing_pheromons, data_Pen$MeanAnts_depositing_pheromons, alternative = "less") #ns
#var.test(data_Kue$MeanAnts_depositing_pheromons, data_Pen$MeanAnts_depositing_pheromons)
#t.test(data_Kue$MeanAnts_depositing_pheromons, data_Pen$MeanAnts_depositing_pheromons, alternative = "less") #ns

print("##### Mean_sec_to_enter ####")
shapiro.test(data_Kue$Mean_sec_to_enter)
shapiro.test(data_Pen$Mean_sec_to_enter)
wilcox.test(data_Kue$Mean_sec_to_enter, data_Pen$Mean_sec_to_enter, alternative = "less") #ns
#var.test(data_Kue$Mean_sec_to_enter, data_Pen$Mean_sec_to_enter)
#t.test(data_Kue$Mean_sec_to_enter, data_Pen$Mean_sec_to_enter, alternative = "less") #ns

print("##### sec_to_maxWorkers ####")
shapiro.test(data_Kue$sec_to_maxWorkers)
shapiro.test(data_Pen$sec_to_maxWorkers)
wilcox.test(data_Kue$sec_to_maxWorkers, data_Pen$sec_to_maxWorkers, alternative = "less") #ns
#var.test(data_Kue$sec_to_maxWorkers, data_Pen$sec_to_maxWorkers)
#t.test(data_Kue$sec_to_maxWorkers, data_Pen$sec_to_maxWorkers, alternative = "less") #ns
sink()


## Mean differences tests - Exploratory  ####
setwd("") #set your working directory
exp_file <- read.csv("Exploratory_20221222.csv", sep = ",",header=TRUE)
data_Kue <- subset(exp_file, pop=="K")
data_Pen <- subset(exp_file, pop=="P")

sink("Exploration_Mean_test.txt")
print("##### Mean ants ####")
shapiro.test(data_Kue$mean)
shapiro.test(data_Pen$mean)
wilcox.test(data_Kue$mean, data_Pen$mean, alternative = "less") #ns
#var.test(data_Kue$mean, data_Pen$mean)
#t.test(data_Kue$mean, data_Pen$mean, alternative = "less") #ns

print("##### Max ants ####")
shapiro.test(data_Kue$max)
shapiro.test(data_Pen$max)
wilcox.test(data_Kue$max, data_Pen$max, alternative = "less") #ns
#var.test(data_Kue$max, data_Pen$max)
#t.test(data_Kue$max, data_Pen$max, alternative = "less") #ns

print("##### s_unt_in_arena_first ####")
shapiro.test(data_Kue$s_unt_in_arena_first)
shapiro.test(data_Pen$s_unt_in_arena_first)
wilcox.test(data_Kue$s_unt_in_arena_first, data_Pen$s_unt_in_arena_first, alternative = "less") #ns
#var.test(data_Kue$s_unt_in_arena_first, data_Pen$s_unt_in_arena_first)
#t.test(data_Kue$s_unt_in_arena_first, data_Pen$s_unt_in_arena_first, alternative = "less") #ns

print("##### s_until_in_arena_max ####")
shapiro.test(data_Kue$s_until_in_arena_max)
shapiro.test(data_Pen$s_until_in_arena_max)
wilcox.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max, alternative = "less") #ns
#var.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max)
#t.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max, alternative = "less") #ns
sink()


## Mean differences tests - Foraging Honey  ####
ForHoneyDr <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)
data_Kue <- subset(ForHoneyDr, test_mean_drinking_4=="x" & pop=="K")
data_Pen <- subset(ForHoneyDr, test_mean_drinking_4=="x" & pop=="P")


sink("ForagingHoney_Mean_test.txt")
print("##### Mean ants ####")
shapiro.test(data_Kue$mean)
shapiro.test(data_Pen$mean)
wilcox.test(data_Kue$mean, data_Pen$mean, alternative = "less") #ns
#var.test(data_Kue$mean, data_Pen$mean)
#t.test(data_Kue$mean, data_Pen$mean, alternative = "less") #ns

print("##### Max ants ####")
shapiro.test(data_Kue$max)
shapiro.test(data_Pen$max)
#wilcox.test(data_Kue$max, data_Pen$max, alternative = "less") #ns
var.test(data_Kue$max, data_Pen$max)
t.test(data_Kue$max, data_Pen$max, alternative = "less") #ns

print("##### s_until_first_Contact_honey ####")
shapiro.test(data_Kue$s_until_first_Contact_honey)
shapiro.test(data_Pen$s_until_first_Contact_honey)
wilcox.test(data_Kue$s_until_first_Contact_honey, data_Pen$s_until_first_Contact_honey, alternative = "less") #ns
#var.test(data_Kue$s_until_first_Contact_honey, data_Pen$s_until_first_Contact_honey)
#t.test(data_Kue$s_until_first_Contact_honey, data_Pen$s_until_first_Contact_honey, alternative = "less") #ns


print("##### s_until_on_honey_max ####")
shapiro.test(data_Kue$s_until_on_honey_max)
shapiro.test(data_Pen$s_until_on_honey_max)
#wilcox.test(data_Kue$s_until_on_honey_max, data_Pen$s_until_on_honey_max, alternative = "less") #ns
var.test(data_Kue$s_until_on_honey_max, data_Pen$s_until_on_honey_max)
t.test(data_Kue$s_until_on_honey_max, data_Pen$s_until_on_honey_max, alternative = "less") #ns
sink()


##   Mean differences tests - Foraging Arena  ####
freq1 <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)
data_Kue <- subset(freq1, test_mean_arena_4=="x" & pop=="K")
data_Pen <- subset(freq1, test_mean_arena_4=="x" & pop=="P")


sink("ForagingArena_Mean_test.txt")
print("##### Mean ants ####")
shapiro.test(data_Kue$mean)
shapiro.test(data_Pen$mean)
wilcox.test(data_Kue$mean, data_Pen$mean, alternative = "less") #ns
#var.test(data_Kue$mean, data_Pen$mean)
#t.test(data_Kue$mean, data_Pen$mean) #ns

print("##### Max ants ####")
shapiro.test(data_Kue$max)
shapiro.test(data_Pen$max)
#wilcox.test(data_Kue$max, data_Pen$max, alternative = "less") #ns
var.test(data_Kue$max, data_Pen$max)
t.test(data_Kue$max, data_Pen$max, alternative = "less") #ns


print("##### s_until_first_in_arena ####")
shapiro.test(data_Kue$s_until_first_in_arena)
shapiro.test(data_Pen$s_until_first_in_arena)
wilcox.test(data_Kue$s_until_first_in_arena, data_Pen$s_until_first_in_arena, alternative = "less") #ns
#var.test(data_Kue$s_until_first_in_arena, data_Pen$s_until_first_in_arena)
#t.test(data_Kue$s_until_first_in_arena, data_Pen$s_until_first_in_arena, alternative = "less") #ns


print("##### s_until_in_arena_max ####")
shapiro.test(data_Kue$s_until_in_arena_max)
shapiro.test(data_Pen$s_until_in_arena_max)
wilcox.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max, alternative = "less") #ns
#var.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max)
#t.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max, alternative = "less") #ns
sink()



## Mean differences tests - Risk Honey  ####
freq <- read.csv("Risk_20221222.csv", sep = ",",header=TRUE)
data_Kue <- subset(freq, ant_location_help=="honey" & pop=="K")
data_Pen <- subset(freq, ant_location_help=="honey" & pop=="P")


sink("RiskHoney_Mean_test.txt")
print("##### Mean ants ####")
shapiro.test(data_Kue$mean)
shapiro.test(data_Pen$mean)
#wilcox.test(data_Kue$mean, data_Pen$mean, alternative = "less") #
var.test(data_Kue$mean, data_Pen$mean)
t.test(data_Kue$mean, data_Pen$mean, alternative = "less") #


print("##### Max ants ####")
shapiro.test(data_Kue$max)
shapiro.test(data_Pen$max)
#wilcox.test(data_Kue$max, data_Pen$max, alternative = "less") #
var.test(data_Kue$max, data_Pen$max)
t.test(data_Kue$max, data_Pen$max, alternative = "less") #


print("##### s_until_on_honey_max ####")
shapiro.test(data_Kue$s_until_on_honey_max)
shapiro.test(data_Pen$s_until_on_honey_max)
wilcox.test(data_Kue$s_until_on_honey_max, data_Pen$s_until_on_honey_max, alternative = "less") #ns
#var.test(data_Kue$s_until_on_honey_max, data_Pen$s_until_on_honey_max)
#t.test(data_Kue$s_until_on_honey_max, data_Pen$s_until_on_honey_max, alternative = "less") #ns
sink()


## Mean differences tests - Risk Arena  ####
freq1 <- read.csv("Risk_20221222.csv", sep = ",",header=TRUE)
data_Kue <- subset(freq1, ant_location_help=="in_arena" & pop=="K")
data_Pen <- subset(freq1, ant_location_help=="in_arena" & pop=="P")

sink("RiskArena_Mean_test.txt")
print("##### Mean ants ####")
shapiro.test(data_Kue$mean)
shapiro.test(data_Pen$mean)
wilcox.test(data_Kue$mean, data_Pen$mean, alternative = "less") #ns
#var.test(data_Kue$mean, data_Pen$mean)
#t.test(data_Kue$mean, data_Pen$mean, alternative = "less") #ns


print("##### Max ants ####")
shapiro.test(data_Kue$max)
shapiro.test(data_Pen$max)
#wilcox.test(data_Kue$max, data_Pen$max, alternative = "less") #ns
var.test(data_Kue$max, data_Pen$max)
t.test(data_Kue$max, data_Pen$max, alternative = "less") #ns

# 
print("##### s_until_in_arena_max ####")
shapiro.test(data_Kue$s_until_in_arena_max)
shapiro.test(data_Pen$s_until_in_arena_max)
#wilcox.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max, alternative = "less") #ns
var.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max)
t.test(data_Kue$s_until_in_arena_max, data_Pen$s_until_in_arena_max, alternative = "less") #n
sink()



# Boxplots ####
## Boldness Boxplots ####
setwd("") #set your working directory
freq_b <- read.csv("boldness_20221222.csv", sep = ",",header=TRUE)

boldness_mean <- ggplot(freq_b, aes(y=MeanAnts_crossed_junction))+
  geom_boxplot()+
  ylab(label = "Average no. of ants crossed bridge")+
  facet_grid(~pop1 , switch = "x")+ 
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
boldness_mean

##Note, same as boldness_mean
# boldness_max <- ggplot(freq, aes(y=MaxAnts_crossed_junction))+
#   geom_boxplot()+
#   ylab(label = "Max no. of ants crossed bridge")+
#   facet_grid(~pop1 , switch = "x")+  #+ cons
#   theme_ggdist()+
#   theme(axis.ticks = element_blank(), axis.text.x = element_blank())
# boldness_max

#Sum_ants_depositing_pheromons
AntsDepositingPheromons <- ggplot(freq_b, aes(y=MeanAnts_depositing_pheromons))+  
  geom_boxplot()+
  ylab(label = "Number of ants depositing pheromones")+
  facet_grid(~pop1, switch = "x")+ 
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
AntsDepositingPheromons

boldness_meanSec2Enter <- ggplot(freq_b, aes(y=Mean_sec_to_enter))+  #Sum_ants_depositing_pheromons
  geom_boxplot()+
  ylab(label = "Sec until first ants crossed bridge")+
  facet_grid(~pop1, switch = "x")+  
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
boldness_meanSec2Enter

boldness_maxSec2Enter <- ggplot(freq_b, aes(y=sec_to_maxWorkers))+  #Sum_ants_depositing_pheromons
  geom_boxplot()+
  ylab(label = "Sec until max. no. of ants crossed bridge")+
  facet_grid(~pop1, switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
boldness_maxSec2Enter

pdf("Fig_Boxplots_boldness_KuePen_all.pdf", width=7, height=9) #, pointsize = 8
ggarrange(boldness_mean + rremove("x.text") + rremove("xlab"),
          AntsDepositingPheromons + rremove("x.text") + rremove("xlab"),
          boldness_meanSec2Enter,
          boldness_maxSec2Enter,
          ncol = 2, nrow = 2, align="v")
dev.off()


## Exploratory Boxplots ####
freq <- read.csv("Exploratory_20221222.csv", sep = ",",header=TRUE)

expl_mean <- ggplot(freq, aes(y=mean))+
  geom_boxplot()+
  ylab(label = "Mean n of workers in arena")+
  facet_grid(~pop , switch = "x") + #
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
expl_mean

expl_max <- ggplot(freq, aes(y=max))+
  geom_boxplot()+
  ylab(label = "Max n of workers in arena")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
expl_max

expl_meanSec2Enter <- ggplot(freq, aes(y=s_unt_in_arena_first))+  
  geom_boxplot()+
  ylab(label = "S until first workers in arena")+
  facet_grid(~pop1, switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
expl_meanSec2Enter

expl_maxSec2Enter <- ggplot(freq, aes(y=s_until_in_arena_max))+  
  geom_boxplot()+
  ylab(label = "S until max. no. of workers in arena")+
  facet_grid(~pop1, switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
expl_maxSec2Enter

pdf("Fig_Boxplots_expl_KP_bef_all_2022.pdf", width=7, height=9) #
ggarrange(expl_mean + rremove("x.text") + rremove("xlab"),
          expl_max + rremove("x.text") + rremove("xlab"),
          expl_meanSec2Enter,# 
          expl_maxSec2Enter,# 
          ncol = 2, nrow = 2, align="v") #
dev.off()


## Foraging Honey Boxplots ####
setwd("") #set your working directory
freq1 <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)
freq_Fh <- subset(freq1, test_mean_drinking_4=="x")

forHon_mean <- ggplot(freq_Fh, aes(y=mean))+
  geom_boxplot()+
  ylab(label = "Mean . of workers at honey")+
  facet_grid(~pop1 , switch = "x")+ 
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forHon_mean

forHon_max <- ggplot(freq_Fh, aes(y=max))+
  geom_boxplot()+
  ylab(label = "Max n of workers at honey")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forHon_max


forHon_meanSec2Enter <- ggplot(freq_Fh, aes(y=s_until_first_Contact_honey))+  
  geom_boxplot()+
  ylab(label = "S until first workers at honey")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forHon_meanSec2Enter

forHon_maxSec2Enter <- ggplot(freq_Fh, aes(y=s_until_in_arena_max))+  
  geom_boxplot()+
  ylab(label = "S until max n of workers at honey")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forHon_maxSec2Enter

pdf("Fig_Boxplots_forHon_KP_bef_all.pdf", width=7, height=9) #
ggarrange(forHon_mean + rremove("x.text") + rremove("xlab"),
          forHon_max + rremove("x.text") + rremove("xlab"),
          forHon_meanSec2Enter,
          forHon_maxSec2Enter,
          ncol = 2, nrow = 2, align="v")
dev.off()


## Foraging Arena Boxplots ####
setwd("")
freq1 <- read.csv("Foraging_20221222.csv", sep = ",", header=TRUE)
freq_fA <- subset(freq1, test_mean_arena_4=="x")

forArena_mean <- ggplot(freq_fA, aes(y=mean))+
  geom_boxplot()+
  ylab(label = "Mean n of workers in arena")+
  facet_grid(~pop1, switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forArena_mean

forArena_max <- ggplot(freq_fA, aes(y=max))+
  geom_boxplot()+
  ylab(label = "Max. n of workers in arena")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forArena_max

forArena_meanSec2Enter <- ggplot(freq_fA, aes(y=s_until_first_Contact_honey))+  
  geom_boxplot()+
  ylab(label = "Sec until first workers in arena")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forArena_meanSec2Enter

forArena_maxSec2Enter <- ggplot(freq_fA, aes(y=s_until_in_arena_max))+ 
  geom_boxplot()+
  ylab(label = "Sec until max. no. of workers in arena")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
forArena_maxSec2Enter


pdf("Fig_Boxplots_forArena_KP_bef_all.pdf", width=7, height=9) #,
ggarrange(forArena_mean + rremove("x.text") + rremove("xlab"),
          forArena_max + rremove("x.text") + rremove("xlab"),
          forArena_meanSec2Enter,# 
          forArena_maxSec2Enter,# 
          ncol = 2, nrow = 2, align="v")
dev.off()


## Risk Honey Boxplots ####
freq1 <- read.csv("Risk_20221222.csv", sep = ",", header=TRUE)
freq_RH <- subset(freq1, ant_location_help=="honey")

riskHon_mean <- ggplot(freq_RH, aes(y=mean))+
  geom_boxplot()+
  ylab(label = "Mean n of workers in arena")+
  facet_grid(~pop1, switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
riskHon_mean

riskHon_max <- ggplot(freq_RH, aes(y=max))+
  geom_boxplot()+
  ylab(label = "Max n of workers in arena")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
riskHon_max


riskHon_maxSec2Enter <- ggplot(freq_RH, aes(y=s_until_on_honey_max))+  #Sum_ants_depositing_pheromons
  geom_boxplot()+
  ylab(label = "Sec until max n of workers at honey")+
  facet_grid(~pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
riskHon_maxSec2Enter


library(ggpubr)
pdf("Fig_Boxplots_riskHon_KP_bef_all.pdf", width=7, height=9) 
ggarrange(riskHon_mean + rremove("x.text") + rremove("xlab"),
          riskHon_max + rremove("x.text") + rremove("xlab"),
          riskHon_maxSec2Enter,
          ncol = 2, nrow = 2, align="v") 
dev.off()


## Risk Arena Boxplots ####
freq1 <- read.csv("Risk_20221222.csv", sep = ",", header=TRUE)
freq_RA <- subset(freq1, ant_location_help=="in_arena")

riskArena_mean <- ggplot(freq_RA, aes(y=mean))+
  geom_boxplot()+
  ylab(label = "Mean n of workers in arena")+
  facet_grid(~ pop1 , switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
riskArena_mean

riskArena_max <- ggplot(freq_RA, aes(y=max))+
  geom_boxplot()+
  ylab(label = "Max n of workers in arena")+
  facet_grid(~pop1, switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
riskArena_max

riskArena_maxSec2Enter <- ggplot(freq_RA, aes(y=s_until_in_arena_max))+  #Sum_ants_depositing_pheromons
  geom_boxplot()+
  ylab(label = "Sec until max n of workers in arena")+
  facet_grid(~pop1, switch = "x")+
  theme_ggdist()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
riskArena_maxSec2Enter


library(ggpubr)
pdf("Fig_Boxplots_riskArena_KP_bef_all.pdf", width=7, height=9) 
ggarrange(riskArena_mean + rremove("x.text") + rremove("xlab"),
          riskArena_max + rremove("x.text") + rremove("xlab"),
          riskArena_maxSec2Enter,
          ncol = 2, nrow = 2, align="v")
dev.off()


# Fig 3 complete ####
pdf("Fig3.pdf", height = 19, width = 12)  #height = 28.7/2.54, width = 20/2.54
ggarrange(
  ggarrange(boldness_mean + rremove("x.text") + rremove("xlab"),
            AntsDepositingPheromons + rremove("x.text") + rremove("xlab"),
            boldness_meanSec2Enter,# 
            boldness_maxSec2Enter,# 
            labels = c("A", "B", "C", "D"),
            ncol = 4, nrow = 1, align="v") ,
  ggarrange(expl_mean + rremove("x.text") + rremove("xlab"),
            expl_max + rremove("x.text") + rremove("xlab"),
            expl_meanSec2Enter,# 
            expl_maxSec2Enter,# 
            labels = c("E", "F", "G", "H"),
            ncol = 4, nrow = 1, align="v") ,
  ggarrange(forHon_mean + rremove("x.text") + rremove("xlab"),
            forHon_max + rremove("x.text") + rremove("xlab"),
            forHon_meanSec2Enter,# 
            forHon_maxSec2Enter,# 
            labels = c("I", "J", "K", "L"),
            ncol = 4, nrow = 1, align="v") ,
  ggarrange(forArena_mean + rremove("x.text") + rremove("xlab"),
            forArena_max + rremove("x.text") + rremove("xlab"),
            forArena_meanSec2Enter,# 
            forArena_maxSec2Enter,# 
            labels = c("M", "N", "O", "P"),
            ncol = 4, nrow = 1, align="v") ,
  ggarrange(riskHon_mean + rremove("x.text") + rremove("xlab"),
            riskHon_max + rremove("x.text") + rremove("xlab"),
            riskHon_maxSec2Enter,# 
            labels = c("Q", "R", "S"),
            ncol = 4, nrow = 1, align="v") ,
  ggarrange(riskArena_mean + rremove("x.text") + rremove("xlab"),
            riskArena_max + rremove("x.text") + rremove("xlab"),
            riskArena_maxSec2Enter,# 
            labels = c("T", "U", "V"),
            ncol = 4, nrow = 1, align="v") ,
  nrow = 6,  ncol=1, align = "v")
dev.off()



# Linear mixed effect models (LMEM) ####
## LMEM Boldness ####
setwd("")
freq1 <- read.csv("boldness_20221222.csv", sep = ",",header=TRUE)

#Kue
freq_boldness <- subset(freq1, pop=="K")
weeks_K <- c(rep(252, 3), rep(261, 6), rep(268,4))

pdf("LMER_bold_week_Kue.pdf") 
sink("LMER_bold_week_Kue.txt")
for(i in seq(1:5)) {
  if(i == 1) dependVar <- freq_boldness$Mean_sec_to_enter
  if(i == 2) dependVar <- freq_boldness$sec_to_maxWorkers
  if(i == 3) dependVar <- freq_boldness$MeanAnts_crossed_junction
  if(i == 4) dependVar <- freq_boldness$MaxAnts_crossed_junction
  if(i == 5) dependVar <- freq_boldness$MeanAnts_depositing_pheromons

  if(i == 1) dataFile <- "#####     Mean_sec_to_enter      #####"
  if(i == 2) dataFile <- "#####     sec_to_maxWorkers     #####"
  if(i == 3) dataFile <- "#####     MeanAnts_crossed_junction     #####"
  if(i == 4) dataFile <- "#####     MaxAnts_crossed_junction     #####"
  if(i == 5) dataFile <- "#####     MeanAnts_depositing_pheromons     #####"

  print(dataFile)
  Mod1 <- lmer(dependVar ~   weeks_K + (1 | freq_boldness$colony)  +(1 | freq_boldness$rep)  , REML = T) 
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  print("#####     summary     #####"); cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance  col   #####")
  print(VarCorr(Mod1)$"freq_boldness$colony"[1] /
          (VarCorr(Mod1)$"freq_boldness$colony"[1] +
             VarCorr(Mod1)$"freq_boldness$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance  rep   #####")
  print(VarCorr(Mod1)$"freq_boldness$rep"[1] /
          (VarCorr(Mod1)$"freq_boldness$colony"[1] +
             VarCorr(Mod1)$"freq_boldness$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq_boldness$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq_boldness$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony    #####")
  print(quantile(posterior_colony /
                   (posterior_colony +posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep    #####")
  print(quantile(posterior_rep /
                   (posterior_colony + posterior_rep+ posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI residual    #####")
  print(quantile(posterior_residual /
                   (posterior_colony + posterior_rep+ posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()

#Pen
freq_boldness <- subset(freq1, pop=="P")
freq_boldness$week
weeks_P <- c(rep(252, 6), rep(261, 5), rep(268,3))

pdf("LMER_bold_week_Pen.pdf") 
sink("LMER_bold_week_Pen.txt")
for(i in seq(1:5)) {
  if(i == 1) dependVar <- freq_boldness$Mean_sec_to_enter
  if(i == 2) dependVar <- freq_boldness$sec_to_maxWorkers
  if(i == 3) dependVar <- freq_boldness$MeanAnts_crossed_junction
  if(i == 4) dependVar <- freq_boldness$MaxAnts_crossed_junction
  if(i == 5) dependVar <- freq_boldness$MeanAnts_depositing_pheromons
  
  if(i == 1) dataFile <- "#####     Mean_sec_to_enter      #####"
  if(i == 2) dataFile <- "#####     sec_to_maxWorkers     #####"
  if(i == 3) dataFile <- "#####     MeanAnts_crossed_junction     #####"
  if(i == 4) dataFile <- "#####     MaxAnts_crossed_junction     #####"
  if(i == 5) dataFile <- "#####     MeanAnts_depositing_pheromons     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~   weeks_P + (1 | freq_boldness$colony)  +(1 | freq_boldness$rep)  , REML = T) 
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  print("#####     summary     #####"); cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance  col   #####")
  print(VarCorr(Mod1)$"freq_boldness$colony"[1] /
          (VarCorr(Mod1)$"freq_boldness$colony"[1] +
             VarCorr(Mod1)$"freq_boldness$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance  rep   #####")
  print(VarCorr(Mod1)$"freq_boldness$rep"[1] /
          (VarCorr(Mod1)$"freq_boldness$colony"[1] +
             VarCorr(Mod1)$"freq_boldness$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq_boldness$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq_boldness$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony    #####")
  print(quantile(posterior_colony /
                   (posterior_colony +posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep    #####")
  print(quantile(posterior_rep /
                   (posterior_colony + posterior_rep+ posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI residual    #####")
  print(quantile(posterior_residual /
                   (posterior_colony + posterior_rep+ posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


## LMEM Exploratory ####
freq1 <- read.csv("Exploratory_.csv", sep = ";",header=TRUE)
freq <- subset(freq1, test_mean_4=="x" & pop=="K" & cons=="no") #K
freq$week

#Kue
freq1 <- read.csv("Exploratory_20221222.csv", sep = ",",header=TRUE)
freq <- subset(freq1, pop=="K")
weeks_K <- c(rep(252, 3), rep(261, 6), rep(268,4))

pdf("LM_explo_week_Kue.pdf")
sink("LM_explo_week_Kue.txt")
for(i in seq(1:4)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_unt_in_arena_first
  if(i == 4) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_unt_in_arena_first     #####"
  if(i == 4) dataFile <- "#####     s_until_in_arena_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_K  + (1  | freq$colony) + (1  | freq$rep)  , REML = T, data=freq) 
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony +  posterior_rep+ posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


#Pen
freq <- subset(freq1, pop=="P") 
weeks_P <- c(rep(252, 6), rep(261, 5), rep(268,3))

pdf("LM_explo_week_Pen.pdf")
sink("LM_explo_week_Pen.txt")
for(i in seq(1:4)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_unt_in_arena_first
  if(i == 4) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_unt_in_arena_first     #####"
  if(i == 4) dataFile <- "#####     s_until_in_arena_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_P + (1  | freq$colony) + (1  | freq$rep)  , REML = T, data=freq) 
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony +  posterior_rep+ posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


## LMEM Foraging Honey ####
freq1 <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)

#Kue
freq <- subset(freq1, test_mean_drinking_4=="x" & pop=="K")
weeks_K <- c(rep(252, 3), rep(261, 6), rep(268,4))

pdf("LM_ForHon_K_bef_all.pdf")
sink("LM_ForHon_K_bef_all.txt")
for(i in seq(1:4)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_first_Contact_honey  
  if(i == 4) dependVar <- freq$s_until_on_honey_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_first_Contact_honey     #####"
  if(i == 4) dataFile <- "#####     s_until_on_honey_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_K  + (1  | freq$colony)  + (1  | freq$rep) , REML = T, data=freq)
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


#Pen
freq <- subset(freq1, test_mean_drinking_4=="x" & pop=="P")
weeks_P  <- c(rep(252, 6), rep(261, 5), rep(268,3))

pdf("LM_ForHoney_P_bef_all.pdf")
sink("LM_ForHoney_P_bef_all.txt")
for(i in seq(1:4)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_first_Contact_honey
  if(i == 4) dependVar <- freq$s_until_on_honey_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_first_Contact_honey     #####"
  if(i == 4) dataFile <- "#####     s_until_on_honey_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_P  + (1  | freq$colony)  + (1  | freq$rep) , REML = T, data=freq)
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


## LMEM Foraging Arena ####
freq1 <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)

#Kue
freq <- subset(freq1, test_mean_arena_4=="x" & pop=="K")
weeks_K <- c(rep(252, 3), rep(261, 6), rep(268,4))


pdf("LM_ForArena_K_bef_all.pdf")
sink("LM_ForArena_K_bef_all.txt")
for(i in seq(1:4)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_first_in_arena
  if(i == 4) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_first_in_arena     #####"
  if(i == 4) dataFile <- "#####     s_until_in_arena_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_K  + (1  | freq$colony)  + (1  | freq$rep) , REML = T, data=freq)
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


#Pen
freq1 <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)
freq <- subset(freq1, test_mean_arena_4=="x" & pop=="P")
weeks_P <- c(rep(252, 7), rep(261, 5), rep(268,3))

pdf("LM_ForArena_P_bef_all.pdf")
sink("LM_ForArena_P_bef_all.txt")
for(i in seq(1:4)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_first_in_arena
  if(i == 4) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_first_in_arena     #####"
  if(i == 4) dataFile <- "#####     s_until_in_arena_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_P + (1  | freq$colony)  + (1  | freq$rep) , REML = T, data=freq)
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony +  posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


## LMEM Risk Honey ####
freq1 <- read.csv("Risk_20221222.csv", sep = ",", header=TRUE)

#Kue
freq <- subset(freq1,  pop=="K" & ant_location_help=="honey")
weeks_K <- c(rep(252, 3), rep(261, 6), rep(268,4))

pdf("LM_RiskHoney_K_bef_all.pdf")
sink("LM_RiskHoney_K_bef_all.txt")
for(i in seq(1:3)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_in_arena_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_K  + (1  | freq$colony) + (1  | freq$rep) , REML = T, data=freq)
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


#Pen
freq <- subset(freq1, pop=="P" & ant_location_help=="honey")
weeks_P <- c(rep(252, 5), rep(261, 5), rep(268,3))

pdf("LM_RiskHoney_P_bef_all.pdf")
sink("LM_RiskHoney_P_bef_all.txt")
for(i in seq(1:3)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_in_arena_max     #####"
  
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_P  + (1  | freq$colony) + (1  | freq$rep) , REML = T, data=freq)
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


## LMEM Risk Arena ####
freq1 <- read.csv("Risk_20221222.csv", sep = ",", header=TRUE)
freq <- subset(freq1, pop=="K" & ant_location_help=="in_arena")
weeks_K <- c(rep(252, 3), rep(261, 6), rep(268,4))

pdf("LM_RiskArena_K_bef_all.pdf")
sink("LM_RiskArena_K_bef_all.txt")
for(i in seq(1:3)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_in_arena_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_K  + (1  | freq$colony) + (1  | freq$rep) , REML = T, data=freq)
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()


#Pen
freq <- subset(freq1,  pop=="P" & ant_location_help=="in_arena")
weeks_P <- c(rep(252, 5), rep(261, 5), rep(268,3))

pdf("LM_RiskArena_P_bef_all.pdf")
sink("LM_RiskArena_P_bef_all.txt")
for(i in seq(1:3)) {
  if(i == 1) dependVar <- freq$mean
  if(i == 2) dependVar <- freq$max
  if(i == 3) dependVar <- freq$s_until_in_arena_max
  
  if(i == 1) dataFile <- "#####     mean workers      #####"
  if(i == 2) dataFile <- "#####     max workers    #####"
  if(i == 3) dataFile <- "#####     s_until_in_arena_max     #####"
  
  print(dataFile)
  Mod1 <- lmer(dependVar ~ weeks_P  + (1  | freq$colony) + (1  | freq$rep) , REML = T, data=freq) 
  simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
  plot(simulationOutput, sub="Mod1")
  cat("\n");  cat("\n")
  print(summary(Mod1))
  print(r.squaredGLMM(Mod1))
  cat("\n"); print("#####     variance col    #####")
  print(VarCorr(Mod1)$"freq$colony"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  cat("\n"); print("#####     variance rep    #####")
  print(VarCorr(Mod1)$"freq$rep"[1] /
          (VarCorr(Mod1)$"freq$colony"[1] +
             VarCorr(Mod1)$"freq$rep"[1] +
             attr(VarCorr(Mod1), "sc")^2))
  set.seed(1)
  simulated <- arm::sim(Mod1, n.sim = 1000)
  posterior_colony <- apply(simulated@ranef$"freq$colony"[ , , 1],1,var)
  posterior_rep <- apply(simulated@ranef$"freq$rep"[ , , 1],1,var)
  posterior_residual <- simulated@sigma^2
  cat("\n"); print("#####     Posteriors CI colony      #####")
  print(quantile(posterior_colony /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Posteriors CI rep      #####")
  print(quantile(posterior_rep /
                   (posterior_colony + posterior_rep + posterior_residual),
                 prob=c(0.025, 0.5, 0.975)))
  cat("\n"); print("#####     Coeff of Variatio Colony     #####")
  CVi <- sqrt(posterior_colony) / summary(Mod1)$coefficients[1]
  print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
  cat("\n");  cat("\n"); cat("\n");  cat("\n")
}
sink()
dev.off()



# LM Plots ####
## LM Boldness ####
setwd("")
freq_boldness <- read.csv("boldness_20221222.csv", sep = ",",header=TRUE)

Ants_over_bridge <- ggplot(freq_boldness, aes(y = MeanAnts_crossed_junction, x = weeks_days), col="black")+ 
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Average no. of ants crossing bridge")+
  coord_cartesian(ylim=c(0,10), xlim=c(250, 270))+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,10,2))+
  xlab("Day number in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ ##OR Set1 OR Dark2
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")
Ants_over_bridge

MeanSec2Enter <- ggplot(freq_boldness, aes(y = Mean_sec_to_enter, x = weeks_days), col="black")+ 
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until first ants crossed bridge")+
  coord_cartesian(ylim=c(0,2000), xlim=c(250, 270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Day number in the year")+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")
MeanSec2Enter

#freq_boldness$sec_to_maxWorkers
Sec2Max <- ggplot(freq_boldness, aes(y = sec_to_maxWorkers, x = weeks_days), col="black")+#))+ 
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until max. ants crossed bridge")+
  coord_cartesian(ylim=c(0,2000), xlim=c(250, 270))+
  xlab("Day number in the year")+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
Sec2Max


Pheromons <- ggplot(freq_boldness, aes(y = MeanAnts_depositing_pheromons, x = weeks_days), col="black")+#
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Number of ants depositing pheromons")+
  coord_cartesian(ylim=c(0,0.5), xlim=c(250, 270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Day number in the year")+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(), 
        legend.position = "none")#
Pheromons

pdf("Boldness_KP_all.pdf", width=11, height=11)
ggarrange(Ants_over_bridge, MeanSec2Enter,
          Sec2Max, Pheromons,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2) 
dev.off()


## LM Exploratory ####
freq_exploratory <- read.csv("Exploratory_20221222.csv", sep = ",",header=TRUE)
weeksNew <- c(rep(252, 9), rep(261, 11), rep(268,7))

Mean_exp <- ggplot(freq_exploratory, aes(y = mean, x = weeksNew), col="black")+
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Mean no. of ants in arena")+
  coord_cartesian(ylim=c(0,10), xlim=c(250,270))+
  geom_smooth(method = "lm", se=F)+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,10,2))+
  xlab("Days in the year")+
  scale_color_brewer(palette="Set2")+ #
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")
Mean_exp

#freq_exploratory$colony
Max_exp <- ggplot(freq_exploratory, aes(y = max, x = weeksNew), col="black")+#))+ 
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Max. no. of ants in arena")+
  coord_cartesian(ylim=c(0,35), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,35,5))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ ##OR Set1 OR Dark2
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
Max_exp

SFirst_exp <- ggplot(freq_exploratory, aes(y = s_unt_in_arena_first, x = weeksNew), col="black")+
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until first ants in arena")+
  coord_cartesian(ylim=c(0,2000), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ ##
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
SFirst_exp

SMax_exp <- ggplot(freq_exploratory, aes(y = s_until_in_arena_max, x = weeksNew), col="black")+#
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until max. ants in arena")+
  coord_cartesian(ylim=c(0,2000), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ #
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
SMax_exp

pdf("Expl_KPbef_all.pdf", width=11, height=11)
ggarrange(Mean_exp, Max_exp, SFirst_exp, SMax_exp,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2) 
dev.off()


## LM Foraging Honey ####
freq1fh <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)
freq_foraging_honey <- subset(freq1fh, test_mean_drinking_4=="x")
weeksNewFor <- c(rep(252, 9), rep(261, 11), rep(268,7))

#FHon = Foraging Honey
Mean_FHon <- ggplot(freq_foraging_honey, aes(y = mean, x = weeksNewFor), col="black")+
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Mean n of workers in arena")+
  coord_cartesian(ylim=c(0,30), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")#
Mean_FHon


Max_FHon <- ggplot(freq_foraging_honey, aes(y = max, x = weeksNewFor), col="black")+#))+ 
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Max n of workers in arena")+
  coord_cartesian(ylim=c(0,40), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,40,10))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")
Max_FHon

#freq_foraging_honey$s_until_first_Contact_honey
SFirst_FHon <- ggplot(freq_foraging_honey, aes(y = s_until_first_Contact_honey, x = weeksNewFor), col="black")+
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("S until first workers feeding honey")+
  coord_cartesian(ylim=c(0,1000), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ #
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")#
SFirst_FHon

SMax_FHon <- ggplot(freq_foraging_honey, aes(y = s_until_in_arena_max, x = weeksNewFor), col="black")+  
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until max workers in arena")+
  coord_cartesian(ylim=c(0,1000), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")
SMax_FHon


pdf("For_Honey_KP_bef_all.pdf", width=11, height=11)
ggarrange(Mean_FHon, Max_FHon, SFirst_FHon, SMax_FHon, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2) 
dev.off()



## LM Foraging Arena ####
freq1fh <- read.csv("Foraging_20221222.csv", sep = ",",header=TRUE)
freq_foraging_arena <- subset(freq1fh, test_mean_arena_4=="x")
weeksNewForA <- c(rep(252, 10), rep(261, 11), rep(268,7))

#Fare = Foraging arena
Mean_FAre <- ggplot(freq_foraging_arena, aes(y = mean, x = weeksNewForA), col="black")+#
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Mean n of workers in arena")+
  coord_cartesian(ylim=c(0,15), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,15,5))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
Mean_FAre


Max_FAre <- ggplot(freq_foraging_arena, aes(y = max, x = weeksNewForA), col="black")+#
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Max n of workers in arena")+
  coord_cartesian(ylim=c(0,40), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,40,10))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
Max_FAre


SFirst_FAre <- ggplot(freq_foraging_arena, aes(y = s_until_first_in_arena, x = weeksNewForA), col="black")+#))+ 
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until first workers feeding honey")+
  scale_x_continuous(breaks=c(252,261,268))+
  coord_cartesian(ylim=c(0,450), xlim=c(250,270))+
  scale_y_continuous(breaks=seq(0,450,50))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")
SFirst_FAre

SMax_FAre <- ggplot(freq_foraging_arena, aes(y = s_until_in_arena_max, x = weeksNewForA), col="black")+ 
  geom_jitter(size=3, width = 0.5, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until max workers in arena")+
  coord_cartesian(ylim=c(0,1000), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")#
SMax_FAre


pdf("For_Arena_KP_bef_all.pdf", width=11, height=11)
ggarrange(Mean_FAre, Max_FAre, SFirst_FAre, SMax_FAre,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2) 
dev.off()



## LM Risk Honey ####
setwd("")
freq1fh <- read.csv("Risk_20221222.csv", sep = ",",header=TRUE)
freq_risk_honey <- subset(freq1fh, ant_location_help=="honey")
weeksNewRisk <- c(rep(252, 8), rep(261, 11), rep(268,7))

#RHon = Risk Honey
Mean_RHon <- ggplot(freq_risk_honey, aes(y = mean, x = weeksNewRisk), col="black")+
  geom_jitter(size=3, width = 0.8, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Mean no. of ants in arena")+
  coord_cartesian(ylim=c(0,15), xlim=c(250,270))+
  scale_y_continuous(breaks=seq(0,15,5))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(),
        legend.position = "none")
Mean_RHon


Max_RHon <- ggplot(freq_risk_honey, aes(y = max, x = weeksNewRisk), col="black")+
  geom_jitter(size=3, width = 0.8, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Max. no. of ants in arena")+
  coord_cartesian(ylim=c(0,25), xlim=c(250,270))+
  scale_y_continuous(breaks=seq(0,25,5))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+

  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
Max_RHon


SMax_RHon <- ggplot(freq_risk_honey, aes(y = s_until_on_honey_max, x = weeksNewRisk), col="black")+#
  geom_jitter(size=3, width = 0.8, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until max. ants in arena")+
  coord_cartesian(ylim=c(0,750), xlim=c(250,270))+
  scale_y_continuous(breaks=seq(0,750,250))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
SMax_RHon

library(ggpubr)
pdf("Risk_Honey_KP_bef_all.pdf", width=11, height=11)
ggarrange(Mean_RHon, Max_RHon, SMax_RHon, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2) 
dev.off()


## LM Risk Arena ####
freq1fh <- read.csv("Risk_20221222.csv", sep = ",",header=TRUE)
freq_risk_arena <- subset(freq1fh, ant_location_help=="in_arena")
weeksNewRisk <- c(rep(252, 8), rep(261, 11), rep(268, 7))

#RAre = Risk Arena 
Mean_RAre <- ggplot(freq_risk_arena, aes(y = mean, x = weeksNewRisk), col="black")+#
  geom_jitter(size=3, width = 0.8, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Mean no. of ants in arena")+
  coord_cartesian(ylim=c(0,15), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,15,5))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
Mean_RAre


Max_RAre <- ggplot(freq_risk_arena, aes(y = max, x = weeksNewRisk), col="black")+ #
  geom_jitter(size=3, width = 0.8, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Max. no. of ants in arena")+
  coord_cartesian(ylim=c(0,25), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  scale_y_continuous(breaks=seq(0,25,5))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")
Max_RAre


SMax_RAre <- ggplot(freq_risk_arena, aes(y = s_until_in_arena_max, x = weeksNewRisk), col="black")+
  geom_jitter(size=3, width = 0.8, height = 0.001)+
  facet_grid(~pop1)+
  ylab("Sec until max. ants in arena")+
  coord_cartesian(ylim=c(0,1000), xlim=c(250,270))+
  scale_x_continuous(breaks=c(252,261,268))+
  xlab("Days in the year")+
  geom_smooth(method = "lm", se=F)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  theme(plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank(),
        legend.position = "none")#
SMax_RAre


pdf("Risk_Arena_KPsep_bef_all.pdf", width=11, height=11)
ggarrange(Mean_RAre, Max_RAre, #
          SMax_RAre, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2) 
dev.off()

# Fig 2 complete ####
pdf("Fig2.pdf", height = 19, width = 12)  #
ggarrange(
  ggarrange(Ants_over_bridge + rremove("x.text") + rremove("xlab"),  Pheromons+ rremove("x.text") + rremove("xlab"),
            MeanSec2Enter  + rremove("x.text") + rremove("xlab"), Sec2Max  + rremove("x.text") + rremove("xlab"),
            labels = c("A", "B", "C", "D"), label.x = 0,
            ncol = 4, nrow = 1, align = "h"),
  ggarrange(Mean_exp+ rremove("x.text") + rremove("xlab"), Max_exp+ rremove("x.text") + rremove("xlab"), SFirst_exp+ rremove("x.text") + rremove("xlab"), SMax_exp+ rremove("x.text") + rremove("xlab"),
            labels = c("E", "F", "G", "H"),
            ncol = 4, nrow = 1, align = "h"),
  ggarrange(Mean_FHon+ rremove("x.text") + rremove("xlab"), Max_FHon+ rremove("x.text") + rremove("xlab"), SFirst_FHon+ rremove("x.text") + rremove("xlab"), SMax_FHon+ rremove("x.text") + rremove("xlab"), 
            labels = c("I", "J", "K", "L"),
            ncol = 4, nrow = 1, align = "h"),
  ggarrange(Mean_FAre+ rremove("x.text") + rremove("xlab"), Max_FAre+ rremove("x.text") + rremove("xlab"), SFirst_FAre+ rremove("x.text") + rremove("xlab"), SMax_FAre+ rremove("x.text") + rremove("xlab"),
            labels = c("M", "N", "O", "P"),
            ncol = 4, nrow = 1, align = "h"),
  ggarrange(Mean_RHon+ rremove("x.text") + rremove("xlab"), Max_RHon+ rremove("x.text") + rremove("xlab"), SMax_RHon+ rremove("x.text") + rremove("xlab"), 
            labels = c("Q", "R", "S"),
            ncol = 4, nrow = 1),
  ggarrange(Mean_RAre, Max_RAre, SMax_RAre,
            labels = c("T", "U", "V"),
            ncol = 4, nrow = 1, align = "h"),
  nrow = 6,  ncol=1, align = "v")
dev.off()



# Increasing temperatures - difference in number of workers remaining in arena? ####
setwd("")
Risk_ <- read.csv("Risk_temp_20221222.csv", header = TRUE,sep = ",")

# Fig 4 ####
Fig4 <- ggplot(Risk_, aes(x=Risk_$temperatur_1, y=Risk_$inds_tot_bef_leaving))+
  geom_jitter(size=3, height = 0.11, width = 0.11)+
  geom_smooth(method = "lm", se=F, col="black")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~")), size=4, label.x = 25)+
  stat_cor(size=4, label.y = 20)+
  ylab("No. of ants feeding on honey at specific temperature")+
  coord_cartesian(ylim=c(0,22), xlim=c(25,50))+
  scale_y_continuous(breaks=seq(0,23,1))+
  xlab("Temperature [°C]")+
  theme_bw()+
  facet_grid(~ pop_1 )+
  theme(legend.position = "top",
        plot.title = element_text(size=17),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12, color="black"),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12, color="black"),
        panel.grid.minor = element_blank(),  #
        panel.grid.major = element_blank())
Fig4

pdf("Fig4_20221222.pdf", width = 10)
Fig4
dev.off()

#Analysing the data
Risk_$pop_1
Risk_ <- subset(Risk, test=="x" & ants=="honey")

Risk_ <- as_tibble(Risk_)
Risk$pop_1 <- as.factor(Risk$pop_1)
levels(Risk$pop_1)
c1 <- c(1, -1)
mat <- cbind(c1)
contrasts(Risk$pop_1) <- mat


mod1 <- lmer(inds_tot_bef_leaving ~ pop_1 + (1 | temperatur_1), data=Risk_)
simulationOutput <- simulateResiduals(fittedModel = mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
plot(simulationOutput, sub="mod1")
#summary(mod1)

#Data transformation using the natural log
mod2 <- lmer(log(inds_tot_bef_leaving+1) ~ pop_1 + (1 | temperatur_1), data=Risk_)
simulationOutput <- simulateResiduals(fittedModel = mod2, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
plot(simulationOutput, sub="mod2")
summary(mod2)


effect_plot(model = mod2, pred = pop_1, 
            plot.points = TRUE, jitter = 0.3, point.alpha = 0.2) +
  xlab("Population") + 
  ylab("Inds leaving") + 
  scale_x_discrete(labels = c('K','P'))


Risk_K <- subset(Risk_, pop_1=="K")
Risk_P <- subset(Risk_, pop_1=="P")
mean(Risk_K$inds_tot_bef_leaving, na.rm = T) # 2.756757
mean(Risk_P$inds_tot_bef_leaving, na.rm = T) # 6.671642

summary(lm(inds_tot_bef_leaving ~ temperatur_1, data=Risk_K))
summary(lm(inds_tot_bef_leaving ~ temperatur_1, data=Risk_P))

ModK <- lm(inds_tot_bef_leaving ~ temperatur_1, data=Risk_K)
simulationOutput <- simulateResiduals(fittedModel = ModK, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
plot(simulationOutput, sub="ModK")

ModP <- lm(inds_tot_bef_leaving ~ temperatur_1, data=Risk_P)
simulationOutput <- simulateResiduals(fittedModel = ModP, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
plot(simulationOutput, sub="ModP")


pdf("Risk_temp_Dharma.pdf")
sink("Risk_Temp_LMEM.txt")
Mod1 <- lmer(log(inds_tot_bef_leaving+1) ~ pop_1 + (1 | temperatur_1), data=Risk_)
simulationOutput <- simulateResiduals(fittedModel = Mod1, n=500, seed=(as.integer(runif(1, min=1, max=10000))))
plot(simulationOutput, sub="Mod1")
print(summary(Mod1))
print(r.squaredGLMM(Mod1))
cat("\n"); print("#####     variance temp    #####")
print(VarCorr(Mod1)$"Risk_$temperatur_1"[1] /
        (VarCorr(Mod1)$"Risk_$temperatur_1"[1] +
           attr(VarCorr(Mod1), "sc")^2))
set.seed(1)
simulated <- arm::sim(Mod1, n.sim = 1000)
posterior_pop <- apply(simulated@ranef$temperatur_1[ , , 1],1,var)
posterior_residual <- simulated@sigma^2
cat("\n"); print("#####     Posteriors CI colony      #####")
print(quantile(posterior_pop /
                 (posterior_pop +  posterior_residual),
               prob=c(0.025, 0.5, 0.975)))
cat("\n"); print("#####     Coeff of Variatio Colony     #####")
CVi <- sqrt(posterior_pop) / summary(Mod1)$coefficients[1]
print(quantile(CVi,prob=c(0.025, 0.5, 0.975)))
dev.off()
sink()

#