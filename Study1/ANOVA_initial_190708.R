
my_data <- ANOVA_int
my_data <- my_data[my_data$Trial != '1' & my_data$Trial != '3', ]

# Drop None segments
my_data_drop <- my_data[my_data$Segment != 'None', ]
my_data_drop$Segment <- droplevels(my_data_drop$Segment)

# Check distribution of BallDistance for each segment
png('first_anova_dist.png')
par(mfrow = c(4, 3))
for(i in levels(my_data_drop$Segment)) {
  
  hist(my_data_drop$BallDist[my_data_drop$Segment == i], main = i, xlab = 'Ball Distance')
  
}
par(mfrow = c(1, 1))
dev.off()

# Remove values less than or equal to 0, and log-transform ball dist
my_data_drop <- my_data_drop[my_data_drop$BallDist > 0, ]
my_data_drop$log_ball <- log(my_data_drop$BallDist)

library(doBy)
library(ggplot2)
library(plyr)

se = function(x) {
  sd(x)/sqrt(length(x))
}

pmeans <- summaryBy(log_ball ~ Segment + Subject, data = my_data_drop, keep.names = T, FUN = c(mean, sd))
meanssum <- summaryBy(log_ball.mean ~ Segment, data = pmeans, keep.names = T, FUN = c(mean, sd))

colnames(meanssum) <- c('Segment', 'Mean', 'SD')
meanssum$Segment <- factor(c('Control', 'Listen', 'Play', 'Pre-play',
                             'Pre-listen', 'Overlap', 'Pause'),
                           levels = c('Control', 'Listen', 'Play', 'Pre-play',
                                      'Pre-listen', 'Overlap', 'Pause'),
                           labels = c('Control', 'Listen', 'Play', 'Pre-play',
                                      'Pre-listen', 'Overlap', 'Pause'))

ggplot(meanssum, aes(x = Segment, y = Mean)) +
  geom_bar(stat = 'identity', position = 'dodge', width = .8, 
           col = 'black', fill = 'white') +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(.6), width = .1) +
  labs(x = 'Segment', y = 'Mean log ball distance') +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  theme_classic()


ggsave('anova_plot_first.png', width=5, height=3, dpi=800)


means <- aggregate(log_ball ~ Segment + Subject, my_data_drop, mean) # Get means for each segment for each participant
means2 <- aggregate(log_ball ~ Segment, my_data_drop, mean)

# Descriptives
desc_means_1 <- data.frame(Segment = 'X', N = 2, Mean = 3, SD = 4, Min = 8, Max = 9, Skew = 11)
for(i in levels(my_data_drop$Segment)) {
  
  x <- cbind(i, as.data.frame(psych::describe(my_data_drop$log_ball[my_data_drop$Segment == i]))[, c(2:4, 8, 9, 11)])
  colnames(x) <- colnames(desc_means_1)
  
  desc_means_1 <- rbind(desc_means_1, x)
  
}

desc_means_1 <- desc_means_1[2:nrow(desc_means_1), ]
write.csv(desc_means_1, 'desc_means_1.csv', row.names = F)

for(i in levels(my_data_drop$Segment)) {
  
  x <- sprintf('%s: %.2f', i, mean(my_data_drop$Length[my_data_drop$Segment == i]))
  print(x)
  x <- sprintf('%s: %.2f', i, sd(my_data_drop$Length[my_data_drop$Segment == i]))
  print(x)
  
}

mod7_seg <- aov(log_ball ~ Segment + Error(Subject), data = means)
summary(mod7_seg)
