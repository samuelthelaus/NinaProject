
library(doBy)
library(ggplot2)
library(plyr)

se = function(x) {
  sd(x)/sqrt(length(x))
}

ANOVA_2_duet$log_ball <- log(ANOVA_2_duet$BallDist)

meansSubject2 <- aggregate(log_ball ~ Segment + Familiarity + Subject, ANOVA_2_duet, mean)

model_2_1 <- aov(log_ball ~ Segment + Familiarity + Error(Subject), data = meansSubject2)
summary(model_2_1)

save_desc_2 <- data.frame()
for(i in levels(ANOVA_2_duet$Segment)) {
  mean_seg <- mean(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == i])
  sd_seg <- sd(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == i])
  
  save_desc_2 <- rbind(save_desc_2, 
                       data.frame(Segment = i, Mean = mean_seg, SD = sd_seg))
  
  print_1 <- sprintf('Segment: %s, Mean = %.2f, SD = %.2f', i, mean_seg, sd_seg)
  
  print(print_1)
}


# Play - Listen
PlayListenM <- subset(meansSubject2, Segment == 'Play' | Segment == 'Listen')
PlayListenM$Segment <- droplevels(PlayListenM$Segment)
levels(PlayListenM$Segment)

t.test(PlayListenM$log_ball ~ PlayListenM$Segment, var.equal=T)

# Effect size
round((mean(meansSubject2$log_ball[meansSubject2$Segment == 'Play']) - 
         mean(meansSubject2$log_ball[meansSubject2$Segment == 'Listen']))/
        sqrt(
          (sd(meansSubject2$log_ball[meansSubject2$Segment == 'Play'])^2 +
             sd(meansSubject2$log_ball[meansSubject2$Segment == 'Listen'])^2)/
            2), 3)


# Play - Wait
PlayWaitM <- subset(meansSubject2, Segment == 'Play' | Segment == 'Wait')
PlayWaitM$Segment <- droplevels(PlayWaitM$Segment)
levels(PlayWaitM$Segment)

t.test(PlayWaitM$log_ball ~ PlayWaitM$Segment, var.equal=T)

round((mean(meansSubject2$log_ball[meansSubject2$Segment == 'Play']) - 
         mean(meansSubject2$log_ball[meansSubject2$Segment == 'Wait']))/
        sqrt(
          (sd(meansSubject2$log_ball[meansSubject2$Segment == 'Play'])^2 +
             sd(meansSubject2$log_ball[meansSubject2$Segment == 'Wait'])^2)/
            2), 3)


# Listen - Prepare
ListenPrepareM <- subset(meansSubject2, Segment == 'Listen' | Segment == 'Prepare')
ListenPrepareM$Segment <- droplevels(ListenPrepareM$Segment)
levels(ListenPrepareM$Segment)

t.test(ListenPrepareM$log_ball ~ ListenPrepareM$Segment, var.equal=T)

# Effect Size
round((mean(meansSubject2$log_ball[meansSubject2$Segment == 'Prepare']) - 
         mean(meansSubject2$log_ball[meansSubject2$Segment == 'Listen']))/
        sqrt(
          (sd(meansSubject2$log_ball[meansSubject2$Segment == 'Prepare'])^2 +
             sd(meansSubject2$log_ball[meansSubject2$Segment == 'Listen'])^2)/
            2), 3)


# Prepare - Wait
PrepareWaitM <- subset(meansSubject2, Segment == 'Prepare' | Segment == 'Wait')
PrepareWaitM$Segment <- droplevels(PrepareWaitM$Segment)
levels(PrepareWaitM$Segment)

t.test(PrepareWaitM$log_ball ~ PrepareWaitM$Segment, var.equal=T)

# Effect Size
round((mean(meansSubject2$log_ball[meansSubject2$Segment == 'Prepare']) - 
         mean(meansSubject2$log_ball[meansSubject2$Segment == 'Wait']))/
        sqrt(
          (sd(meansSubject2$log_ball[meansSubject2$Segment == 'Prepare'])^2 +
             sd(meansSubject2$log_ball[meansSubject2$Segment == 'Wait'])^2)/
            2), 3)

# Descriptives
desc_means_2 <- data.frame(Segment = 'X', N = 2, Mean = 3, SD = 4, Min = 8, Max = 9, Skew = 11)
for(i in levels(ANOVA_2_duet$Segment)) {
  
  x <- cbind(i, as.data.frame(psych::describe(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == i]))[, c(2:4, 8, 9, 11)])
  colnames(x) <- colnames(desc_means_2)
  
  desc_means_2 <- rbind(desc_means_2, x)
  
}

desc_means_2 <- desc_means_2[2:nrow(desc_means_2), ]
write.csv(desc_means_2, 'desc_means_2.csv', row.names = F)


# Line plot of segments
SumANOVA_seg <- data.frame(Segment = factor(c(1, 2, 3, 4), levels = c(1, 2, 3, 4),
                                            labels = c('Play', 'Pre-Listen', 'Listen', 'Pre-Play')),
                           log_ball = c(mean(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Play']),
                                        mean(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Wait']),
                                        mean(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Listen']),
                                        mean(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Prepare'])),
                           se = c(se(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Play']),
                                  se(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Wait']),
                                  se(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Listen']),
                                  se(ANOVA_2_duet$log_ball[ANOVA_2_duet$Segment == 'Prepare'])))

ggplot(SumANOVA_seg, aes(Segment, log_ball, group = 1)) + 
  geom_point(size = .75) + 
  geom_line() + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se), 
                width = 0.15) + 
  scale_y_continuous(limits = c(3.4, 5.0)) + 
  labs(x = 'Segment type', y = 'Mean log ball distance') +
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .5)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('line_graph_anova_2.png', width = 6, height = 4.5)

# Frequencies of Segments
summary(ANOVA_2_duet$Segment)
