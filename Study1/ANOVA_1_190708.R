
library(doBy)
library(ggplot2)
library(plyr)
library(Rmisc)

se = function(x) {
  sd(x)/sqrt(length(x))
}

ANOVA_1$log_ball <- log(ANOVA_1$BallDist)

# Get means for each Segment for each participant
meansSubject1 <- aggregate(log_ball ~ Segment + Order + Subject, ANOVA_1, mean)
meansSegments1 <- aggregate(log_ball ~ Segment, ANOVA_1, mean)

# Run anova on participant means
model_1_1 <- aov(log_ball ~ Segment * Order + Error(Subject), data = meansSubject1)
summary(model_1_1)

# End Listen vs Beg
t.test(meansSubject1$log_ball[meansSubject1$Segment == 'Listen'] ~ 
         meansSubject1$Order[meansSubject1$Segment == 'Listen'],
       var.equal=T)

# End Play vs Beg
t.test(meansSubject1$log_ball[meansSubject1$Segment == 'Play'] ~ 
         meansSubject1$Order[meansSubject1$Segment == 'Play'],
       var.equal=T)

save_desc_1 <- data.frame()
for(i in levels(meansSubject1$Segment)) {
  mean_seg <- mean(meansSubject1$log_ball[meansSubject1$Segment == i])
  sd_seg <- sd(meansSubject1$log_ball[meansSubject1$Segment == i])
  
  save_desc_1 <- rbind(save_desc_1, 
                       data.frame(Segment = i, Mean = mean_seg, SD = sd_seg))
  
  print_1 <- sprintf('Segment: %s, Mean = %.2f, SD = %.2f', i, mean_seg, sd_seg)
  
  print(print_1)
}

save_desc_1_ord <- data.frame()
for(i in levels(meansSubject1$Order)) {
  mean_seg <- mean(meansSubject1$log_ball[meansSubject1$Order == i])
  sd_seg <- sd(meansSubject1$log_ball[meansSubject1$Order == i])
  
  save_desc_1_ord <- rbind(save_desc_1_ord, 
                           data.frame(Order = i, Mean = mean_seg, SD = sd_seg))
  
  print_1 <- sprintf('Order: %s, Mean = %.2f, SD = %.2f', i, mean_seg, sd_seg)
  
  print(print_1)
}

for(i in levels(meansSubject1$Order)) {
  for(j in levels(meansSubject1$Segment)) {
    
    mean_seg <- mean(meansSubject1$log_ball[meansSubject1$Order == i &
                                              meansSubject1$Segment == j])
    sd_seg <- sd(meansSubject1$log_ball[meansSubject1$Order == i &
                                          meansSubject1$Segment == j])
    
    print_1 <- sprintf('Order: %s, Segment: %s, Mean = %.2f, SD = %.2f', i, j, mean_seg, sd_seg)
    
    print(print_1)
  }
}

# Plot interaction
SumANOVA_2 <- summarySE(meansSubject1, measurevar = 'log_ball', groupvars = c('Segment', 'Order'))

ggplot(SumANOVA_2, aes(Order, log_ball)) + 
  geom_point(aes(color = Segment), size = .75) + 
  geom_line(aes(color = Segment, group = Segment)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = log_ball - se, ymax = log_ball + se, color = Segment), 
                width = 0.15) + 
  scale_y_continuous(limits = c(3.4, 5.0)) +
  scale_x_discrete(breaks=c('First', 'Last'),
                   labels=c('Beginning', 'End')) +
  labs(x = 'Segment part', y = 'Mean log ball distance') +
  scale_color_manual(name = 'Segment Type', values = c('#002E75', '#FF1831'),
                     labels = c('Play', 'Listen')) +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('interaction_plot_anova_1.png', dpi = 800)


# Effect size play/listen
round((mean(meansSubject1$log_ball[meansSubject1$Segment == 'Play']) - 
         mean(meansSubject1$log_ball[meansSubject1$Segment == 'Listen']))/
        sqrt(
          (sd(meansSubject1$log_ball[meansSubject1$Segment == 'Play'])^2 +
             sd(meansSubject1$log_ball[meansSubject1$Segment == 'Listen'])^2)/
            2), 3)


# Effect size beginning/end
round((mean(meansSubject1$log_ball[meansSubject1$Order == 'First']) - 
         mean(meansSubject1$log_ball[meansSubject1$Order == 'Last']))/
        sqrt(
          (sd(meansSubject1$log_ball[meansSubject1$Order == 'First'])^2 +
             sd(meansSubject1$log_ball[meansSubject1$Order == 'Last'])^2)/
            2), 3)

# Frequencies of Segment and Order
summary(ANOVA_1$Segment)
summary(ANOVA_1$Order)
