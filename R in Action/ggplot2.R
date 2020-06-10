# basic scatterplot
library(ggplot2)
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = 'Automobile Data', x = 'Weight', y = 'Miles Per Gallon')

# scatter plot with regression line/band
library(ggplot2)
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point(pch = 17, color = 'blue', size = 2) +
  geom_smooth(method = 'lm', color = 'red', linetype = 2) +
  labs(title = 'Automobile Data', x = 'Weight', y = 'Miles Per Gallon')

# introduction to facets and groups
library(ggplot2)
mtcars$cyl <- factor(mtcars$cyl) # make sure the grouping/facet variables are factors
mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c('Automatic', 'Manual'))
mtcars$vs <- factor(mtcars$vs, levels = c(0, 1), labels = c('V-Engine', 'Straight Engine'))
ggplot(data = mtcars, aes(x = hp, y = mpg, shape = cyl, color = cyl)) +
  geom_point(size = 3) +
  facet_grid(am ~ vs) +
  labs(title = 'Automobie Data by Engine Type', x = 'Horsepower', y = 'Miles Per Gallon')

# basic histogram
data(singer, package = 'lattice')
ggplot(singer, aes(x = height)) +
  geom_histogram()

# basic box plot
ggplot(singer, aes(x = voice.part, y = height)) +
  geom_boxplot()

# box plots on top of dot plots with jitter and rug plot
data(Salaries, package = 'carData')
ggplot(Salaries, aes(x = rank, y = salary)) + 
  geom_boxplot(notch = TRUE, fill = 'cornflowerblue', color = 'black') +
  geom_rug(sides = 'l', color = 'black') +
  geom_point(alpha = 0.5, color = 'blue', position = 'jitter')

# violin plot with boxplot
library(ggplot2)
data(singer, package = 'lattice')
ggplot(singer, aes(x = voice.part, y = height)) +
  geom_violin(fill = 'lightblue') +
  geom_boxplot(fill = 'lightgreen', width = .2)

# grouped density plot
# to color by a variable, do color or fill = variable inside aes(). To color a standard color, do it outside of aes()
data(Salaries, package = 'carData')
ggplot(Salaries, aes(x = salary, fill = rank)) +
  geom_density(alpha = .3)

# stacked vs dodge vs fill bar charts
ggplot(Salaries, aes(x = rank, fill = sex)) +
  geom_bar(position = 'stack') +
  labs(title = 'position = stack')
ggplot(Salaries, aes(x = rank, fill = sex)) +
  geom_bar(position = 'dodge') +
  labs(title = 'position = dodge')
ggplot(Salaries, aes(x = rank, fill = sex)) +
  geom_bar(position = 'fill') +
  labs(title = 'position = fill')

# faceting histogram plots
data(singer, package = 'lattice')
library(ggplot2)
ggplot(data = singer, aes(x = height)) +
  geom_histogram() +
  facet_wrap(~voice.part, nrow = 4) # separate plots for each level of voice.part arranged into 4 rows

# combining faceting and grouping for scatterplots
ggplot(Salaries, aes(x = yrs.since.phd, y = salary, color = rank, shape = rank)) +
  geom_point() +
  facet_grid(.~sex) # separate plots for each level of sex arranged as a single row

# faceting multiple density plots into a single column
data(singer, package = 'lattice')
library(ggplot2)
ggplot(singer, aes(x = height, fill = voice.part)) +
  geom_density() +
  facet_grid(voice.part~.)

# adding smooth lines to scatterplots
data(Salaries, package = 'carData')
library(ggplot2)
ggplot(Salaries, aes(x = yrs.since.phd, y = salary)) +
  geom_point() +
  geom_smooth(method = 'loess')

# because we saw a quadratic bend, let's try grouping by sex and adding a quadratic line for both sexes
ggplot(Salaries, aes(x = yrs.since.phd, y = salary, color = sex, shape = sex, linetype = sex)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE)

# tweaking axes and scales (Notice how scale_x_discrete is used to adjust our categorical x-axis)
ggplot(data = Salaries, aes(x = rank, y = salary, fill = sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('AsstProf', 'AssocProf', 'Prof'),
                   labels = c('Assistant\nProfessor',
                              'Associate\nProfessor',
                              'Full\nProfessor')) +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000),
                     labels = c('$50k', '$100k', '$150k', '$200k')) +
  labs(title = 'Faculty Salary by Rank and Sex', x = '', y = '')

# tweaking the legend
ggplot(data = Salaries, aes(x = rank, y = salary, fill = sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('AsstProf', 'AssocProf', 'Prof'),
                   labels = c('Assistant\nProfessor',
                              'Associate\nProfessor',
                              'Full\nProfessor')) +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000),
                     labels = c('$50k', '$100k', '$150k', '$200k')) +
  labs(title = 'Faculty Salary by Rank and Sex', x = '', y = '', fill = 'gender') + # fill = gender changed the legend title
  theme(legend.position = c(0.1, 0.8)) # change the position of the legend

# bubble chart (This is an example of scale being continuous)
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point(shape = 21, color = 'black', fill = 'cornsilk') +
  labs(x = 'Weight', y = 'Miles Per Gallon', title = 'Bubble Chart', size = 'Engine\nDisplacement') # see how we changed legend title

# Example of discrete scale
ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary, color = rank)) +
  geom_point(size = 2) +
  scale_color_manual(values = c('orange', 'olivedrab', 'navy'))
# or if you're bad with colors
ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary, color = rank)) +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Paired') # other options: Set1, Set2, Set3, Pastel1, Pastel2, Dark2, Accent

# working with manually-created themes
mytheme <- theme(plot.title = element_text(face = 'bold.italic', size = 14, color = 'brown'),
                 axis.title = element_text(face = 'bold.italic', size = 10, color = 'brown'),
                 axis.text = element_text(face = 'bold', size = 9, color = 'darkblue'),
                 panel.background = element_rect(fill = 'white', color = 'darkblue'),
                 panel.grid.major.y = element_line(color = 'grey', linetype = 1),
                 panel.grid.minor.y = element_line(color = 'grey', linetype = 2),
                 panel.grid.minor.x = element_blank(),
                 legend.position = 'top')
ggplot(Salaries, aes(x = rank, y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = 'Salary by Rank and Sex', x = 'Rank', y = 'Salary') +
  mytheme

# making a dashboard/grid is actually really easy (Not as good as Tableau though and not interactive)  
p1 <- ggplot(Salaries, aes(x = rank, y = salary)) + 
  geom_boxplot(notch = TRUE, fill = 'cornflowerblue', color = 'black') +
  geom_rug(sides = 'l', color = 'black') +
  geom_point(alpha = 0.5, color = 'blue', position = 'jitter')  
p2 <- ggplot(Salaries, aes(x = rank, y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = 'Salary by Rank and Sex', x = 'Rank', y = 'Salary') +
  mytheme  
p3 <- ggplot(Salaries, aes(x = yrs.since.phd, y = salary, color = sex, shape = sex, linetype = sex)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE)
p4 <- ggplot(Salaries, aes(x = yrs.since.phd, y = salary, color = rank, shape = rank)) +
  geom_point() +
  facet_grid(.~sex) # separate plots for each level of sex arranged as a single row  
library(gridExtra)  
grid.arrange(p1, p2, p3, p4, ncol = 2)  
  
  

























