
library(doBy)
library(ggplot2)
library(plyr)

se = function(x) {
  sd(x)/sqrt(length(x))
}


#### ANOVA 4 Script ####

ANOVA_4$log_ball <- log(ANOVA_4$BallDist)

meansSubject3 <- aggregate(log_ball ~ Segment + Subject, ANOVA_4, mean)

model_4_1 <- aov(log_ball ~ Segment + Error(Subject), data = meansSubject3)
summary(model_4_1)

save_desc_3 <- data.frame()
for(i in levels(ANOVA_4$Segment)) {
  mean_seg <- mean(ANOVA_4$log_ball[ANOVA_4$Segment == i])
  sd_seg <- sd(ANOVA_4$log_ball[ANOVA_4$Segment == i])
  
  save_desc_3 <- rbind(save_desc_3, 
                       data.frame(Segment = i, Mean = mean_seg, SD = sd_seg))
  
  print_1 <- sprintf('Segment: %s, Mean = %.2f, SD = %.2f', i, mean_seg, sd_seg)
  
  print(print_1)
}


# Prepare - BegPlay
PrepareBegPlayM <- subset(meansSubject3, Segment == 'Prepare' | Segment == 'BegPlay')
PrepareBegPlayM$Segment <- droplevels(PrepareBegPlayM$Segment)
levels(PrepareBegPlayM$Segment)

t.test(PrepareBegPlayM$log_ball ~ PrepareBegPlayM$Segment, var.equal=T)

# Effect size
round((mean(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare']) - 
         mean(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay']))/
        sqrt(
          (sd(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare'])^2 +
             sd(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay'])^2)/
            2), 3)


# Prepare - EndListen
PrepareEndListenM <- subset(meansSubject3, Segment == 'Prepare' | Segment == 'EndListen')
PrepareEndListenM$Segment <- droplevels(PrepareEndListenM$Segment)
levels(PrepareEndListenM$Segment)

t.test(PrepareEndListenM$log_ball ~ PrepareEndListenM$Segment, var.equal=T)

round((mean(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare']) - 
         mean(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen']))/
        sqrt(
          (sd(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare'])^2 +
             sd(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen'])^2)/
            2), 3)


# BegPlay - EndListen
BegPlayEndListenM <- subset(meansSubject3, Segment == 'BegPlay' | Segment == 'EndListen')
BegPlayEndListenM$Segment <- droplevels(BegPlayEndListenM$Segment)
levels(BegPlayEndListenM$Segment)

t.test(BegPlayEndListenM$log_ball ~ BegPlayEndListenM$Segment, var.equal=T)

# Effect Size
round((mean(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay']) - 
         mean(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen']))/
        sqrt(
          (sd(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay'])^2 +
             sd(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen'])^2)/
            2), 3)


# Descriptives
desc_means_3 <- data.frame(Segment = 'X', N = 2, Mean = 3, SD = 4, Min = 8, Max = 9, Skew = 11)
for(i in levels(ANOVA_4$Segment)) {
  
  x <- cbind(i, as.data.frame(psych::describe(ANOVA_4$log_ball[ANOVA_4$Segment == i]))[, c(2:4, 8, 9, 11)])
  colnames(x) <- colnames(desc_means_3)
  
  desc_means_3 <- rbind(desc_means_3, x)
  
}

desc_means_3 <- desc_means_3[2:nrow(desc_means_3), ]
write.csv(desc_means_3, 'desc_means_3.csv', row.names = F)


# Line plot of segments
SumANOVA_seg4 <- data.frame(Segment = factor(c(1, 2, 3), levels = c(1, 2, 3),
                                             labels = c('End Listen', 'Pre-play', 'Beg Play')),
                            log_ball = c(mean(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen']),
                                         mean(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare']),
                                         mean(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay'])),
                            se = c(se(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen']),
                                   se(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare']),
                                   se(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay'])))

ggplot(SumANOVA_seg4, aes(Segment, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) + 
  scale_y_continuous(limits = c(3.4, 5.0)) + 
  labs(x = 'Segment type', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_ANOVA_4.png', width = 6, height = 4.5)

# Frequencies of Segments
summary(ANOVA_4$Segment)


#### ANOVA 5 Script ####

ANOVA_5$log_ball <- log(ANOVA_5$BallDist)

meansSubject4 <- aggregate(log_ball ~ Segment + Subject, ANOVA_5, mean)

model_5_1 <- aov(log_ball ~ Segment + Error(Subject), data = meansSubject4)
summary(model_5_1)

save_desc_4 <- data.frame()
for(i in levels(ANOVA_5$Segment)) {
  mean_seg <- mean(ANOVA_5$log_ball[ANOVA_5$Segment == i])
  sd_seg <- sd(ANOVA_5$log_ball[ANOVA_5$Segment == i])
  
  save_desc_4 <- rbind(save_desc_4, 
                       data.frame(Segment = i, Mean = mean_seg, SD = sd_seg))
  
  print_1 <- sprintf('Segment: %s, Mean = %.2f, SD = %.2f', i, mean_seg, sd_seg)
  
  print(print_1)
}


# Wait - EndPlay
WaitEndPlayM <- subset(meansSubject4, Segment == 'Wait' | Segment == 'EndPlay')
WaitEndPlayM$Segment <- droplevels(WaitEndPlayM$Segment)
levels(WaitEndPlayM$Segment)

t.test(WaitEndPlayM$log_ball ~ WaitEndPlayM$Segment, var.equal=T)

# Effect size
round((mean(meansSubject4$log_ball[meansSubject4$Segment == 'Wait']) - 
         mean(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay']))/
        sqrt(
          (sd(meansSubject4$log_ball[meansSubject4$Segment == 'Wait'])^2 +
             sd(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay'])^2)/
            2), 3)


# Wait - BegListen
WaitBegListenM <- subset(meansSubject4, Segment == 'Wait' | Segment == 'BegListen')
WaitBegListenM$Segment <- droplevels(WaitBegListenM$Segment)
levels(WaitBegListenM$Segment)

t.test(WaitBegListenM$log_ball ~ WaitBegListenM$Segment, var.equal=T)

round((mean(meansSubject4$log_ball[meansSubject4$Segment == 'Wait']) - 
         mean(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen']))/
        sqrt(
          (sd(meansSubject4$log_ball[meansSubject4$Segment == 'Wait'])^2 +
             sd(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen'])^2)/
            2), 3)


# EndPlay - BegListen
EndPlayBegListenM <- subset(meansSubject4, Segment == 'EndPlay' | Segment == 'BegListen')
EndPlayBegListenM$Segment <- droplevels(EndPlayBegListenM$Segment)
levels(EndPlayBegListenM$Segment)

t.test(EndPlayBegListenM$log_ball ~ EndPlayBegListenM$Segment, var.equal=T)

# Effect Size
round((mean(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay']) - 
         mean(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen']))/
        sqrt(
          (sd(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay'])^2 +
             sd(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen'])^2)/
            2), 3)


# Descriptives
desc_means_4 <- data.frame(Segment = 'X', N = 2, Mean = 3, SD = 4, Min = 8, Max = 9, Skew = 11)
for(i in levels(ANOVA_5$Segment)) {
  
  x <- cbind(i, as.data.frame(psych::describe(ANOVA_5$log_ball[ANOVA_5$Segment == i]))[, c(2:4, 8, 9, 11)])
  colnames(x) <- colnames(desc_means_4)
  
  desc_means_4 <- rbind(desc_means_4, x)
  
}

desc_means_4 <- desc_means_4[2:nrow(desc_means_4), ]
write.csv(desc_means_4, 'desc_means_4.csv', row.names = F)


# Line plot of segments
SumANOVA_seg5 <- data.frame(Segment = factor(c(1, 2, 3), levels = c(1, 2, 3),
                                             labels = c('End Play', 'Pre-listen', 'Beg Listen')),
                            log_ball = c(mean(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay']),
                                         mean(meansSubject4$log_ball[meansSubject4$Segment == 'Wait']),
                                         mean(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen'])),
                            se = c(se(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay']),
                                   se(meansSubject4$log_ball[meansSubject4$Segment == 'Wait']),
                                   se(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen'])))

ggplot(SumANOVA_seg5, aes(Segment, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) + 
  scale_y_continuous(limits = c(3.4, 5.0)) + 
  labs(x = 'Segment type', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_ANOVA_5.png', width = 6, height = 4.5)

# Frequencies of Segments
summary(ANOVA_5$Segment)


#### ANOVA Plot BIG ####

SumANOVA_seg6 <- data.frame(Segment = factor(c(1, 2, 3, 4, 5, 6), levels = c(1, 2, 3, 4, 5, 6),
                                             labels = c('End Play', 'Pre-listen', 'Beg Listen',
                                                        'End Listen', 'Pre-play', 'Beg Play')),
                            log_ball = c(mean(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay']),
                                         mean(meansSubject4$log_ball[meansSubject4$Segment == 'Wait']),
                                         mean(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen']),
                                         mean(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen']),
                                         mean(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare']),
                                         mean(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay'])),
                            se = c(se(meansSubject4$log_ball[meansSubject4$Segment == 'EndPlay']),
                                   se(meansSubject4$log_ball[meansSubject4$Segment == 'Wait']),
                                   se(meansSubject4$log_ball[meansSubject4$Segment == 'BegListen']),
                                   se(meansSubject3$log_ball[meansSubject3$Segment == 'EndListen']),
                                   se(meansSubject3$log_ball[meansSubject3$Segment == 'Prepare']),
                                   se(meansSubject3$log_ball[meansSubject3$Segment == 'BegPlay'])))

ggplot(SumANOVA_seg6, aes(Segment, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) + 
  scale_y_continuous(limits = c(3.4, 5.0)) + 
  labs(x = 'Segment type', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_ANOVA_6.png', width = 6, height = 4.5)


ANOVA_6 <- rbind(ANOVA_4, ANOVA_5)

# Desc full
desc_means_5 <- data.frame(Segment = 'X', N = 2, Mean = 3, SD = 4, Min = 8, Max = 9, Skew = 11)
for(i in levels(ANOVA_6$Segment)) {
  
  x <- cbind(i, as.data.frame(psych::describe(ANOVA_6$log_ball[ANOVA_6$Segment == i]))[, c(2:4, 8, 9, 11)])
  colnames(x) <- colnames(desc_means_5)
  
  desc_means_5 <- rbind(desc_means_5, x)
  
}

desc_means_5 <- desc_means_5[2:nrow(desc_means_5), ]
write.csv(desc_means_5, 'desc_means_5.csv', row.names = F)

