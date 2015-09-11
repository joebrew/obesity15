
#####
# SOURCE HELPERS
#####
setwd(public)
source('helpers.R')

###############################################################################

#####
# DEFINE KOURTNEY's 9 SCHOOLS OF INTEREST
##### 
k9 <- c('ALACHUA ELEMENTARY',
        'CARING & SHARING LEARNING',
        'DUVAL, CHARLES W. ELEMENTARY',
        'IDYLWILD ELEMENTARY',
        'LAKE FOREST ELEMENTARY',
        'METCALFE, W.A. ELEMENTARY',
        'RAWLINGS, MARJORIE K. ELEMENTARY',
        'TERWILLIGER, MYRA ELEMENTARY',
        'WILLIAMS, JOSEPH ELEMENTARY')

#####
# GET DISTRIBUTIONS
#####

# ALL SCHOOLS
temp <- get_ts(dist = TRUE, race_bi ='non_white')

# SPECIFIC SCHOOLS
temp <- get_ts(dist = TRUE, school = k9[3], grade = 3)

years <- as.numeric(gsub('year_', '', names(temp)))
colors <- colorRampPalette(c('darkblue', 'darkred'))(length(years))

for (i in 1:length(years)){
  hist(temp[[paste0('year_', years[i])]], freq = FALSE, 
       border = 'white',
       ylim = c(0, 0.04),
       xlim = c(0, 100),
       main = NA,
       xlab = 'Percentile BMI for age')
  
  for (j in which(years <= years[i])){
    lines(density(temp[[paste0('year_', years[j])]]),
          col = adjustcolor('black', alpha.f = 0.6))
  }
  
  lines(density(temp[[paste0('year_', years[i])]]),
        col = 'red',
        lwd = 2)
  title(main = years[i])
  Sys.sleep(1)
}

ggplot() + 
  ylim(0, 0.03) +
  geom_density(aes(temp[[paste0('year_', years[1])]]), 
               fill = colors[1],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[2])]]), 
               fill = colors[2],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[3])]]), 
               fill = colors[3],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[4])]]), 
               fill = colors[4],
               alpha = 0.5) +
  geom_density(aes(temp[[paste0('year_', years[5])]]), 
               fill = colors[5],
               alpha = 0.5)

#####
# FREE REDUCED LUNCH
#####
schools <- screen %>%
  group_by(school) %>%
  summarise(Z = mean(Z),
            bmi_percentile = mean(bmi_percentile),
            n = n(),
            free_reduced = length(which(lunch == 'free')),
            non_white = length(which(race_bi == 'non_white')))

schools$p_free <- schools$free_reduced / schools$n * 100
schools$p_non_white <- schools$non_white / schools$n * 100

plot(schools$p_free, schools$bmi_percentile)
g <- ggplot(data = schools[which(schools$bmi_percentile >= 40),], aes(x = p_non_white, 
                                                                      y = bmi_percentile))

g + 
  geom_jitter(alpha = 0.6, color = 'darkgreen') +
  #geom_smooth() + 
  geom_smooth(color = 'red')


#####
# PLOT EACH OF 9 SCHOOLS
#####
for (i in 1:length(k9)){
  
  # Everyone
  all_schools <- get_ts()
  all_schools$school <- 'all'
  
  this_school <- get_ts(school = k9[i])
  this_school$school <- k9[i]
  
  temp <- rbind(all_schools, this_school)
  
  g <- ggplot(data = temp, aes(x = year, y = bmi_percentile, group = school, color = school))
  g1 <- g + geom_line() +
    # geom_smooth() +
    ylim(50, 100) +
    ggtitle('All students')
  
  # Free/reduced lunch only
  all_schools <- get_ts(lunch = 'free')
  all_schools$school <- 'all'
  
  this_school <- get_ts(school = k9[i], lunch = 'free')
  this_school$school <- k9[i]
  
  temp <- rbind(all_schools, this_school)
  
  g <- ggplot(data = temp, aes(x = year, y = bmi_percentile, group = school, color = school))
  g2 <- g + geom_line() +
    # geom_smooth() +
    ylim(50, 100) +
    ggtitle('Only free/reduced lunch students')
  
  # Nonwhite only
  all_schools <- get_ts(race_bi = 'non_white')
  all_schools$school <- 'all'
  
  this_school <- get_ts(school = k9[i], race_bi = 'non_white')
  this_school$school <- k9[i]
  
  temp <- rbind(all_schools, this_school)
  
  g <- ggplot(data = temp, aes(x = year, y = bmi_percentile, group = school, color = school))
  g3 <- g + geom_line() +
    # geom_smooth() +
    ylim(50, 100) +
    ggtitle('Only non-white students')
  
  multiplot(g1, g2, g3)
}


g <- ggplot(data = screen[which(!is.na(screen$lunch)),],
            aes(x = factor(year), y = Z))

g + geom_jitter(alpha = 0.2) +
  geom_violin(fill = 'orange', alpha = 0.3, color = NA) +
  facet_grid(lunch ~ race_bi)

#### 
g <- ggplot(data = screen[which(screen$year != 2015 & !is.na(screen$lunch)),])
g + geom_density(aes(x = Z, y = ..density.., group = lunch, fill = lunch),
                 alpha = 0.3) +
  facet_grid( year ~ .)

# Get by year, school, grade, lunch
temp <- screen %>%
  filter(year != 2015) %>%
  group_by(year, lunch) %>%
  summarise(n = n(),
            normal = length(which(cat == 'normal')),
            overweight = length(which(cat == 'overweight')),
            obese = length(which(cat == 'obese')),
            Z = mean(Z))

# Get percentages
columns <- c('normal', 'overweight', 'obese')
for (j in 1:length(columns)){
  temp[,paste0(columns[j], '_p')] <- 
    temp[,columns[j]] / temp$n * 100
}


# Reshape
temp_gathered <- gather(temp, key = key, value = value, normal_p:obese_p)

g <- ggplot(data = temp_gathered,
            aes(x = year, y = Z, group = lunch, color = lunch))
g + geom_line()


# Get by year
temp <- screen %>%
  filter(year != 2015) %>%
  group_by(year, Grade) %>%
  summarise(n = n(),
            normal = length(which(cat == 'normal')),
            overweight = length(which(cat == 'overweight')),
            obese = length(which(cat == 'obese')))

# Get percentages
columns <- c('normal', 'overweight', 'obese')
for (j in 1:length(columns)){
  temp[,paste0(columns[j], '_p')] <- 
    temp[,columns[j]] / temp$n * 100
}

# Keep only 6th graders
temp6 <- temp[which(temp$Grade == 6),]

# Reshape
temp6_gathered <- gather(temp6, key = key, value = value, normal_p:obese_p)

# Relevel key so that obese is on the bottom
temp6_gathered$key <- factor(temp6_gathered$key,
                             levels = c('obese_p', 'overweight_p', 'normal_p'))

temp6_gathered <- arrange(temp6_gathered, (key))

# Get a version of temp6_gathered using n()
temp6_gathered_n <- gather(temp6, key = key, value = value, normal:obese)
temp6_gathered_n <- arrange(temp6_gathered_n, (key))

# Plot ###############################3


library(reshape2)

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

g <- ggplot(temp6_gathered)
g1 <- g +
  aes(x = year, y = value, group = key, colour = key) +
  #geom_line(lwd = 1, alpha = 0.5) +
  geom_area(aes(group = key, fill = key), color = NA, position = 'stack') +
  theme_bw() +
  xlab('Year') + ylab('Percentage') +
  ggtitle('6th grade adiposity over time (Relative)') +
  theme(axis.text.x = element_text(size = 7, angle = 90),
        #legend.position = 'none',
        axis.title = element_text(size = 8),
        plot.title = element_text(size = rel(0.8))) +
  scale_fill_manual(values = c('darkred', 'orange', 'beige'),
                    guide = guide_legend(reverse=TRUE))

g <- ggplot(temp6_gathered_n)
g2 <- g +
  aes(x = year, y = value, group = key, colour = key) +
  #geom_line(lwd = 1, alpha = 0.5) +
  geom_area(aes(group = key, fill = key), color = NA, position = 'stack') +
  theme_bw() +
  xlab('Year') + ylab('Students') +
  ggtitle('6th grade adiposity over time (Absolute)') +
  theme(axis.text.x = element_text(size = 7, angle = 90),
        #legend.position = 'none',
        axis.title = element_text(size = 8),
        plot.title = element_text(size = rel(0.8))) +
  scale_fill_manual(values = c('darkred', 'orange', 'beige'),
                    guide = guide_legend(reverse=TRUE))



screen <- screen[which(screen$year != 2015),]
g3 <- ggplot(data = screen, aes(x = factor(year), y = Z)) +
  geom_jitter(color = 'orange', alpha = 0.2) +
  geom_violin(fill = 'blue', alpha = 0.3) +
  xlab('Year') +
  ylab('Z-score (0 = average)') +
  ggtitle('Distribution by year')

g4 <- ggplot(data = screen) + 
  aes(x = Health_Screening_Date, y = Z) + 
  geom_point(color = 'orange', alpha = 0.2) + 
  geom_smooth() + 
  xlab('Date of screening') +
  ylab('Z-score') +
  ggtitle('Smoothed trend')

multiplot(g1, g2, g3, g4, cols = 2)