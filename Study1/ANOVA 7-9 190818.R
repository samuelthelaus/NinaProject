
library(ggplot2)

#### Analysis 7-9 ####

#### Number 1 ####

# t-test comparing end play and beginning play
meansEndBegListen <- aggregate(log_ball ~ Segment + Subject, ANOVA_6, mean)

# Prepare - BegPlay
EndBegListenM <- subset(meansEndBegListen, Segment == 'EndListen' | Segment == 'BegListen')
EndBegListenM$Segment <- droplevels(EndBegListenM$Segment)
levels(EndBegListenM$Segment)

t.test(EndBegListenM$log_ball ~ EndBegListenM$Segment, var.equal=T)

# Effect size
round((mean(meansEndBegListen$log_ball[meansEndBegListen$Segment == 'EndListen']) - 
         mean(meansEndBegListen$log_ball[meansEndBegListen$Segment == 'BegListen']))/
        sqrt(
          (sd(meansEndBegListen$log_ball[meansEndBegListen$Segment == 'EndListen'])^2 +
             sd(meansEndBegListen$log_ball[meansEndBegListen$Segment == 'BegListen'])^2)/
            2), 3)


#### Number 2 ####

ANOVA_7$log_ball <- log(ANOVA_7$BallDist)

meansSubject7 <- aggregate(log_ball ~ Bin + Subject, ANOVA_7, mean)

model_7_1 <- aov(log_ball ~ Bin + Error(Subject), data = meansSubject7)
summary(model_7_1)

sum(ANOVA_int$Length[ANOVA_int$Segment == 'Listen'] > 1000)/
  sum(ANOVA_int$Segment == 'Listen')

# Line plot of segments
SumANOVA_bin7 <- data.frame(Bin = factor(c(1, 2, 3, 4), levels = c(1, 2, 3, 4),
                                           labels = c('0-120', '121-240', '241-360', '361-480')),
                              log_ball = c(mean(meansSubject7$log_ball[meansSubject7$Bin == '0-120']),
                                           mean(meansSubject7$log_ball[meansSubject7$Bin == '121-240']),
                                           mean(meansSubject7$log_ball[meansSubject7$Bin == '241-360']),
                                           mean(meansSubject7$log_ball[meansSubject7$Bin == '361-480'])),
                              se = c(se(meansSubject7$log_ball[meansSubject7$Bin == '0-120']),
                                     se(meansSubject7$log_ball[meansSubject7$Bin == '121-240']),
                                     se(meansSubject7$log_ball[meansSubject7$Bin == '241-360']),
                                     se(meansSubject7$log_ball[meansSubject7$Bin == '361-480'])))

ggplot(SumANOVA_bin7, aes(Bin, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) +
  scale_y_continuous(limits = c(3.4, 5)) +
  labs(x = 'Bin', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_ANOVA_7.png', width = 6, height = 4.5)


# Based on graph, check difference between bin 0-120 and 361-480 to see if significant
EndListen14M <- subset(meansSubject7, Bin == '0-120' | Bin == '361-480')
EndListen14M$Bin <- droplevels(EndListen14M$Bin)

t.test(EndListen14M$log_ball ~ EndListen14M$Bin, var.equal=T)


#### Number 3 ####

ANOVA_8$log_ball <- log(ANOVA_8$BallDist)

meansSubject8 <- aggregate(log_ball ~ Bin + Subject, ANOVA_8, mean)

model_8_1 <- aov(log_ball ~ Bin + Error(Subject), data = meansSubject8)
summary(model_8_1)

sum(ANOVA_int$Length[ANOVA_int$Segment == 'Prepare'] > 500)/
  sum(ANOVA_int$Segment == 'Prepare')

# Line plot of segments
SumANOVA_bin8 <- data.frame(Bin = factor(c(1, 2, 3, 4), levels = c(1, 2, 3, 4),
                                         labels = c('0-120', '121-240', '241-360', '361-480')),
                            log_ball = c(mean(meansSubject8$log_ball[meansSubject8$Bin == '0-120']),
                                         mean(meansSubject8$log_ball[meansSubject8$Bin == '121-240']),
                                         mean(meansSubject8$log_ball[meansSubject8$Bin == '241-360']),
                                         mean(meansSubject8$log_ball[meansSubject8$Bin == '361-480'])),
                            se = c(se(meansSubject8$log_ball[meansSubject8$Bin == '0-120']),
                                   se(meansSubject8$log_ball[meansSubject8$Bin == '121-240']),
                                   se(meansSubject8$log_ball[meansSubject8$Bin == '241-360']),
                                   se(meansSubject8$log_ball[meansSubject8$Bin == '361-480'])))

ggplot(SumANOVA_bin8, aes(Bin, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) +
  scale_y_continuous(limits = c(3.4, 5)) +
  labs(x = 'Bin', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_ANOVA_8.png', width = 6, height = 4.5)


# Based on graph, check difference between bin 0-120 and 361-480 to see if significant
PrePlay14M <- subset(meansSubject8, Bin == '0-120' | Bin == '361-480')
PrePlay14M$Bin <- droplevels(PrePlay14M$Bin)

t.test(PrePlay14M$log_ball ~ PrePlay14M$Bin, var.equal=T)


#### Number 4 ####

# Plot with bins for end listen and pre-play
SumANOVA_bin78 <- rbind(SumANOVA_bin7, SumANOVA_bin8)
SumANOVA_bin78$Bin <- factor(1:8, levels = 1:8,
                             labels = rep(c('L0-120', 'L121-240', 'L241-360', 'L361-480',
                                            'PP0-120', 'PP121-240', 'PP241-360', 'PP361-480')))

ggplot(SumANOVA_bin78, aes(Bin, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) +
  scale_y_continuous(limits = c(3.4, 5)) +
  labs(x = 'Bin', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_ANOVA_78.png', width = 6, height = 4.5)


#### Number 5 ####

ANOVA_9$log_ball <- log(ANOVA_9$BallDist)

meansSubject9 <- aggregate(log_ball ~ Bin + Subject, ANOVA_9, mean)

model_9_1 <- aov(log_ball ~ Bin + Error(Subject), data = meansSubject9)
summary(model_9_1)

sum(ANOVA_int$Length[ANOVA_int$Segment == 'Listen'] > 1440)/
  sum(ANOVA_int$Segment == 'Listen')

# Line plot of segments

SumANOVA_bin9 <- data.frame(Bin = factor(1:12, levels = 1:12,
                                  labels = c('0-120', '121-240', '241-360', '361-480',
                                             '481-600', '601-720', '721-840', '841-960',
                                             '961-1080', '1081-1200', '1201-1320', '1321-1440')))

x <- data.frame()
for(i in levels(meansSubject9$Bin)) {
  
  x <- rbind(x, data.frame(log_ball = mean(meansSubject9$log_ball[meansSubject9$Bin == i]),
                  se = se(meansSubject9$log_ball[meansSubject9$Bin == i])))
  
}

SumANOVA_bin9 <- cbind(SumANOVA_bin9, x)


ggplot(SumANOVA_bin9, aes(Bin, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) +
  scale_y_continuous(limits = c(3.4, 5)) +
  labs(x = 'Bin', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_ANOVA_9.png', width = 6, height = 4.5)


# Based on graph, check difference between bin 0-120 and 361-480 to see if significant
Listen112M <- subset(meansSubject9, Bin == '0-120' | Bin == '1321-1440')
Listen112M$Bin <- droplevels(Listen112M$Bin)

t.test(Listen112M$log_ball ~ Listen112M$Bin, var.equal=T)






