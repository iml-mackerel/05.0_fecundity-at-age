#Required package
packages = c('dplyr', 'ggplot2', 'cowplot', 'magrittr','lubridate','tidyverse','cowplot','ggridges','broom','ggeffects','performance','readxl' )
invisible(lapply(packages, function(x) {if (!require(x, character.only = T)) {install.packages(x);require(x)}}))

# load Pelletier fecundity data
mackerel_fecundity_pelletier <- read_excel("C:/Users/BoudreauMA/Desktop/Maquereau/fecundity/mackerel_fecundity_pelletier.xlsx")

# look at data
glimpse(mackerel_fecundity_pelletier)

# new variables
mackerel_fecundity_pelletier %<>% mutate(mass_somatic = `Total weight` - `ovaires weight`, GSI = `ovaires weight` / mass_somatic * 100)
mackerel_fecundity_pelletier$mass_gonad <- mackerel_fecundity_pelletier$`ovaires weight`
mackerel_fecundity_pelletier$day <- mackerel_fecundity_pelletier$`Day of capture`
mackerel_fecundity_pelletier$age <- as.numeric(mackerel_fecundity_pelletier$AgeF)
mackerel_fecundity_pelletier$fecundity <- mackerel_fecundity_pelletier$fecondity
mackerel_fecundity_pelletier$length <- mackerel_fecundity_pelletier$Length

# Verify categories of maturity in data base
unique(mackerel_fecundity_pelletier$Maturity) # only maturity category 5

# figures to see if any outliers
p1 <- mackerel_fecundity_pelletier %>% ggplot(aes(Length, fecundity)) + geom_point() + theme_minimal()
p2 <- mackerel_fecundity_pelletier %>% ggplot(aes(mass_somatic, fecundity)) + geom_point()+ theme_minimal()
p3 <- mackerel_fecundity_pelletier %>% dplyr::filter(!is.na(age)) %>%  ggplot(aes(age, fecundity)) + geom_point()+ theme_minimal()
p4 <- mackerel_fecundity_pelletier %>% ggplot(aes(`ovaires weight`, fecundity)) + geom_point()+ theme_minimal()
p5 <- mackerel_fecundity_pelletier %>% ggplot(aes(day, fecundity)) + geom_point()+ theme_minimal()
p6 <- mackerel_fecundity_pelletier %>% ggplot(aes(GSI, fecundity)) + geom_point()+ theme_minimal()

plot_grid(p1, p2, p3, p4, p5, p6, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 12)

# only 1 outlier with gonad weight > 300 g

# figures in log scales
f1 <- mackerel_fecundity_pelletier %>% ggplot(aes(log(Length), log(fecundity))) + geom_point() + theme_minimal()
f2 <- mackerel_fecundity_pelletier %>% ggplot(aes(log(mass_somatic), log(fecundity))) + geom_point()+ theme_minimal()
f3 <- mackerel_fecundity_pelletier %>% dplyr::filter(!is.na(age)) %>%  ggplot(aes(log(AgeF), log(fecundity))) + geom_point()+ theme_minimal()
f4 <- mackerel_fecundity_pelletier %>% ggplot(aes(log(`ovaires weight`), log(fecundity))) + geom_point()+ theme_minimal()
f5 <- mackerel_fecundity_pelletier %>% ggplot(aes(log(day), log(fecundity))) + geom_point()+ theme_minimal()
f6 <- mackerel_fecundity_pelletier %>% ggplot(aes(log(GSI), log(fecundity))) + geom_point()+ theme_minimal()

plot_grid(f1, f2, f3, f4, f5, f6, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 12)

# Run linear models on each variable seperately as per Pelletier 1986 and compare
# original analyses by Pelletier 1986
m1 <- lm(log10(fecondity) ~ log10(Length), data = mackerel_fecundity_pelletier)
m2 <- lm(log10(fecondity) ~ log10(`Total weight`), data = mackerel_fecundity_pelletier)
m3 <- lm(log10(fecondity) ~ log10(AgeF), data = mackerel_fecundity_pelletier)
m4 <- lm(log10(fecondity) ~ log10(`ovaires weight`), data = mackerel_fecundity_pelletier)
m5 <- lm(log10(fecondity) ~ log10(`Day of capture`), data = mackerel_fecundity_pelletier)
m6 <- lm(log10(fecondity) ~ log10(GSI), data = mackerel_fecundity_pelletier)

compare_performance(m1,m2,m3,m4,m5,m6, rank = T)

m1 <- lm(log(fecondity) ~ log(Length), data = mackerel_fecundity_pelletier)
m2 <- lm(log(fecondity) ~ log(`Total weight`), data = mackerel_fecundity_pelletier)
m3 <- lm(log(fecondity) ~ log(AgeF), data = mackerel_fecundity_pelletier)
m4 <- lm(log(fecondity) ~ log(`ovaires weight`), data = mackerel_fecundity_pelletier)
m5 <- lm(log(fecondity) ~ log(`Day of capture`), data = mackerel_fecundity_pelletier)
m6 <- lm(log(fecondity) ~ log(GSI), data = mackerel_fecundity_pelletier)

compare_performance(m1,m2,m3,m4,m5,m6, rank = T)
plot(mackerel_fecundity_pelletier$AgeF, mackerel_fecundity_pelletier$`ovaires weight`)
plot(mackerel_fecundity_pelletier$AgeF, mackerel_fecundity_pelletier$GSI)

# Produce figure to see relationship between fecundity and age/gonad weight
str(mackerel_fecundity_pelletier)

p1 <- mackerel_fecundity_pelletier %>% ggplot(aes(age, fecundity)) + geom_point() + theme_minimal()
p2 <- mackerel_fecundity_pelletier %>% ggplot(aes(age, log(fecundity))) + geom_point() + theme_minimal()

p3 <- mackerel_fecundity_pelletier %>% ggplot(aes(mass_somatic, fecundity)) + geom_point()+ theme_minimal()
p4 <- mackerel_fecundity_pelletier %>% ggplot(aes(mass_somatic, log(fecundity))) + geom_point()+ theme_minimal()
p5 <- mackerel_fecundity_pelletier %>% ggplot(aes(log(mass_somatic), log(fecundity))) + geom_point()+ theme_minimal()

plot_grid(p1, p2, p3, p4, p5, labels = c('A', 'B', 'C', 'D', 'E'), label_size = 12)

#Create plus group with mackerel of 10 years old and +
mackerel_fecundity_pelletier$age.group <- ifelse(mackerel_fecundity_pelletier$AgeF < 10, mackerel_fecundity_pelletier$AgeF, 10)

# Remove gonad weight of more than 300g and NA in age group
mackerel_fecundity_pelletier <- mackerel_fecundity_pelletier %>% filter(mass_gonad < 300)
mackerel_fecundity_pelletier <- mackerel_fecundity_pelletier %>% drop_na(age.group)

# Test different linear model
#Null model
m0 <- lm(fecundity ~ 1, data = mackerel_fecundity_pelletier)
m0.1 <- lm(log(fecundity) ~ 1, data = mackerel_fecundity_pelletier)

# Models with only gonad weight and log fecundity 
m1 <- lm(fecundity ~ GSI, data = mackerel_fecundity_pelletier)
m1.1 <- lm(log(fecundity) ~ GSI, data = mackerel_fecundity_pelletier)
m1.2 <- lm(log(fecundity) ~ log(GSI), data = mackerel_fecundity_pelletier)

# Models with only age group and log transformation of fecundity
m2 <- lm(fecundity ~ age.group, data = mackerel_fecundity_pelletier)
m2.1 <- lm(log(fecundity) ~ age.group, data = mackerel_fecundity_pelletier)
m2.2 <- lm(log(fecundity) ~ log(age.group), data = mackerel_fecundity_pelletier)

# Models with age group + gonad weight and log transformation of fecundity and gonad weight
m3 <- lm(fecundity ~ GSI + age.group, data = mackerel_fecundity_pelletier)
m3.1 <- lm(log(fecundity) ~ GSI + age.group, data = mackerel_fecundity_pelletier)
m3.2 <- lm(log(fecundity) ~ log(GSI) + log(age.group), data = mackerel_fecundity_pelletier)

# Models with age group + gonad weight and log transformation of fecundity and gonad weight + interaction
m4 <- lm(fecundity ~ GSI + age.group + GSI*age.group, data = mackerel_fecundity_pelletier)
m4.1 <- lm(log(fecundity) ~ GSI + age.group + mass_gonad*age.group, data = mackerel_fecundity_pelletier)
m4.2 <- lm(log(fecundity) ~ log(GSI) + log(age.group) + log(GSI)*log(age.group), data = mackerel_fecundity_pelletier)

#Compare performance of each model
compare_performance(m1,m2,m3,m4, rank = T)
compare_performance(m0.1,m1.1,m2.1,m3.1,m4.1, rank = T)
compare_performance(m1.2,m2.2,m3.2,m4.2, rank = T)

#Test with lmRob to give less weight to outliers fecundity values
library(robust)
m5<-lmRob(log(fecundity) ~ log(GSI) + log(age.group), data = mackerel_fecundity_pelletier)

#let see diagnostic graph for :

# m3 (fecundity ~ gsi + age.group))
summary(m3) 
check_model(m3)  
d3<-ggeffect(m3)
plot(d3)

# m3.1 (log(fecundity) ~ gsi + age.group)
summary(m3.1)
check_model(m3.1) 
d3.1<-ggeffect(m3.1)
plot(d3.1)

# m3.2 (log(fecundity) ~ log(gsi) + log(age.group))
summary(m3.2) 
plot(m3.2) 
d4.1<-ggeffect(m3.2)
plot(d4.1)

# m5 (log(fecundity) ~ log(gsi) + log(age.group)) with lmRob
summary(m5) 
plot(m5, which = 1:5,
     caption = c("Standardized residuals vs. Robust Distances",
                 "Normal Q-Q vs. Residuals", "Response vs. Fitted Values",
                 "Residuals vs. Fitted Values" , "Sqrt of abs(Residuals) vs. Fitted Values"),
     panel = if(add.smooth) panel.smooth else points,
     sub.caption = deparse(x$call), main = "",
     compute.MD = TRUE,
     ask = prod(par("mfcol")) < length(which) && dev.interactive(),
     id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75,
     label.pos = c(4,2), qqline = TRUE, add.smooth = getOption("add.smooth"), p=0.025)
d4.1<-ggeffect(m5)
plot(d4.1)

#Selection of m5 because good residuals and give less weight to lower fecundity values

#### Testing data ####
# Load mackerel bio data from Oracle 
mackerel_bio<-read.csv("C:/Users/BoudreauMA/Desktop/Maquereau/Data/bio.csv", header = TRUE, sep=",")
mackerel_bio <- mackerel_bio %>% dplyr::select(year, agef,sex,matur, wgonad, weight)
mackerel_bio %<>% transmute(year = as.numeric(year), maturity = as.numeric(matur),sex=sex, mass_gonad = wgonad*1000, age = as.numeric(agef), weight = weight*1000)

# subset data with female and maturity category 5
mackerel_bio  %<>%   dplyr::filter(!is.na(mass_gonad),!is.na(age), sex == "F", maturity == "5") # n = 5408

# Remove age outliers and create age.group 1 to 10+ 
mackerel_bio<-subset(mackerel_bio, age  > 0 & age < 30 & weight > 0)
mackerel_bio$age.group <- ifelse(mackerel_bio$age < 10, mackerel_bio$age, 10)
unique(mackerel_bio$age.group)

#Check GSI outliers 
mackerel_bio$GSI <- mackerel_bio$mass_gonad/mackerel_bio$weight*100

# Boxplot of GSI in relation to age.group
ggplot(mackerel_bio, aes(x = as.factor(age.group), y = GSI)) +
  geom_boxplot() +
  xlab("Age") +
  ylab("GSI") +
  theme_minimal()

# Boxplot of GSI in relation to year
ggplot(mackerel_bio, aes(x = as.factor(year), y = GSI)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("GSI") +
  theme_minimal()

# Compare pelletier data with current data
mackerel_bio_graph <- mackerel_bio %>% dplyr::select(year,mass_gonad, age.group, GSI) %>% mutate(DATA = paste("BIO"))
mackerel_fecundity_pelletier_graph <- mackerel_fecundity_pelletier %>% dplyr::select(year,mass_gonad, age.group, GSI) %>% mutate(DATA = paste("REF"))
mackerel_bio_graph <- rbind(mackerel_bio_graph, mackerel_fecundity_pelletier_graph)

# Compare gonad weight by age group
ggplot(mackerel_bio_graph, aes(x = as.factor(age.group), y = mass_gonad, col=DATA)) +
  geom_boxplot() +
  xlab("Age") +
  ylab("Gonad weight (g)")

# Compare GSI by age group
ggplot(mackerel_bio_graph, aes(x = as.factor(age.group), y = GSI, col=DATA)) +
  geom_boxplot() +
  xlab("Age") +
  ylab("GSI")

ggplot(mackerel_bio_graph, aes(x = as.factor(age.group), y = GSI, col=DATA)) +
  geom_point() +
  xlab("Age") +
  ylab("GSI")

ggplot(mackerel_bio_graph, aes(x = as.factor(year), y = GSI, col=DATA)) +
  geom_point() +
  xlab("Year") +
  ylab("GSI")

# loop to select value lower than 99th percentile and higher than 1th percentile for each age group 
# to account for fish that have spawned or high GSI outliers
mackerel_bio_new<-NULL
df2 <- NULL
df2.1 <- NULL

for (i in unique(mackerel_bio$age.group)) {
  
  data.sub <- subset(mackerel_bio, age.group == i)

  High.quantile <- as.numeric(quantile(data.sub$GSI,.99))
  Low.quantile <- as.numeric(quantile(data.sub$GSI,.01))
    
  data.sub$outliers<- ifelse(data.sub$GSI > Low.quantile & data.sub$GSI < High.quantile, FALSE, TRUE)
    
  df1.1 <- data.sub
    
  data.new <- subset(data.sub, outliers == FALSE)
    
  df2.1 <- rbind(df2.1, df1.1)
  
  mackerel_bio_new <- rbind(mackerel_bio_new, data.new)
    
}

# Visualize time series of outliers for all age group aggregated 
p<-ggplot(df2.1, aes(x = year, y = GSI, col = outliers)) + 
  geom_point() +
  #ggtitle(paste("Age", unique(df2.1$age.group), sep = " ")) +
  xlab("Year") + 
  ylab("GSI")

print(p)

# Visualize time series of outliers for all age group separated 
for (i in unique(df2.1$age.group)) {
  
  data.age <- subset(df2.1, age.group == i)
  
  p<-ggplot(data.age, aes(x = year, y = GSI, col = outliers)) + 
  geom_point() +
  ggtitle(paste("Age", unique(data.age$age.group), sep = " ")) +
  xlab("Year") + 
  ylab("GSI")

print(p)

}

#### Simulation to get acceptable number of fish to estimate GSI
# The next couple of lines runs a simulation 1000 times for each age class
# The simulation consist of taking a sample of 1 to 100 fish for each age class and then calculate the error
data.S1.average <- mackerel_bio_new %>% group_by(age.group) %>%
  summarize(Mean.GSI = mean(GSI, na.rm = TRUE))

df4 <- NULL

for (a in 1:1000) {
  
  sim.run <- a
  
  df3 <- NULL
  
  for (b in unique(data.S1.average$age.group)) {
    
    data.S1.age <- subset(mackerel_bio_new, age.group == b)
    
    df2 <- NULL
    
    for (c in 1:100) {
      
      # Sample 1 to 100 fish for each step
      data.S1.sample <- sample_n(data.S1.age, c, replace = TRUE)
      
      # Keep only age group and GSI of the sampled fish 
      data.S1.sample <- data.S1.sample %>% dplyr::select(age.group, GSI)
      
      # Calculate mean weight of fish sampled
      Average.GSI.Sampled <- sum(data.S1.sample$GSI)/nrow(data.S1.sample)
      
      # Join together the average weight at age of the whole subset of data with the individual weight of fish sampled
      test <- left_join(data.S1.sample, data.S1.average, by = "age.group")
      
      # Estimate absolute relative error
      ARE <- (Average.GSI.Sampled-unique(test$Mean.GSI))/unique(test$Mean.GSI)
      
      # Get the number of fish sampled
      nb.fish <- as.numeric(nrow(data.S1.sample))
      
      # Get the age.group of the fish sampled
      age.fish <- as.numeric(unique(data.S1.sample$age.group))
      
      # Data frame of the current run (i.e. i = 2 fish)
      df1 <- data.frame(age.fish, nb.fish, ARE)
      
      # Compiled the result of the current run with those of previous runs
      df2 <- rbind(df2,df1)
      
    }
    
    df3 <- rbind(df3, df2)
    
  }
  
  df3$sim.run <- as.numeric(sim.run)
  df4 <- rbind(df4, df3)
  
}

write.csv(df4, "C:/Users/BoudreauMA/Desktop/Maquereau/Data/Nb.fish.simulation.FecAtAge.csv", row.names = FALSE)


# for each age class and each number of fish sampled, estimate the mean error value ± IC95 
# Import simulation data.set
sim.data <- read.csv("C:/Users/BoudreauMA/Desktop/Maquereau/Data/Nb.fish.simulation.FecAtAge.csv", header = TRUE, sep = ",")

df5 <- NULL

for (i in unique(sim.data$age.fish)) {
  
  sub.data <- subset(sim.data, age.fish == i)
  
  df6 <- NULL
  
  for (j in unique(sub.data$nb.fish)) {
    
    df2 <- subset(sub.data, nb.fish == j)
    
    mean.value <- mean(df2$ARE, na.rm=TRUE)
    
    sd.value <- sd(df2$ARE, na.rm = TRUE)
    
    IC.low <- mean.value - (1.96*sd.value)
    
    IC.up <- mean.value + (1.96*sd.value)
    
    nb.fish <- unique(df2$nb.fish)
    
    age <- unique(df2$age.fish)
    
    df3 <- data.frame(age, nb.fish, mean.value, IC.low, IC.up)
    
    df6 <- rbind(df6, df3) 
    
  }
  
  df5 <- rbind(df5, df6) 
  
  
} 

pd <- position_dodge(0.1) # move them .05 to the left and right

for (i in unique(df5$age)) {
  
  age.data <- subset(df5, age == i)
  
  p <- ggplot(age.data, aes(x = nb.fish, y = mean.value)) +
    geom_line(size = 1.5) +
    geom_errorbar(size = 0.5, aes(ymin=IC.low, ymax=IC.up), width=1.5, position=pd) +
    geom_line(position=pd, size=1) +
    geom_point(position=pd, size=1) +
    geom_hline(yintercept=0.1, linetype="dashed", color = "red") +
    geom_hline(yintercept=-0.1, linetype="dashed", color = "red") +
    theme_minimal(base_size = 14) + 
    ylab("Mean relative error ± IC 95%") + 
    xlab("Number of fish sampled") +
    scale_x_continuous(breaks = seq(0,100,5)) +
    ggtitle(paste("Age", unique(age.data$age), sep = " "))
  
  print(p)
  
}

####Predict fecundity at age ####
# with new sets of data without outliers 
summary(m5)
mackerel_bio_new <- mackerel_bio_new %>% mutate(lGSI=log(GSI),lAGE=log(age.group))
mackerel_bio_new$fit <- 10.94794 + 0.62927*mackerel_bio_new$lGSI + 0.39968*mackerel_bio_new$lAGE
mackerel_bio_new$fecundity <- exp(mackerel_bio_new$fit)

summary(m3.2)
mackerel_bio_new$fit2 <- 10.70832 + 0.71935*mackerel_bio_new$lGSI + 0.38652*mackerel_bio_new$lAGE
mackerel_bio_new$fecundity2 <- exp(mackerel_bio_new$fit2)

# Pelletier (observed - blue points) vs Oracle (predicted - boxplot) fecundity in relation to age.group
p <- ggplot() +
  geom_boxplot(data= mackerel_bio_new, aes(x = as.factor(age.group), y = fecundity)) +
  geom_boxplot(data= mackerel_bio_new, aes(x = as.factor(age.group), y = fecundity2), col = "red", alpha = 0.5) +
  geom_point(data = mackerel_fecundity_pelletier, aes(x = as.factor(age.group), y = fecundity), col = "blue", shape=1, size = 2) +
  xlab("Age") +
  ylab("Fecundity (number of eggs)") +
  theme_minimal(base_size = 14)
p

# Pelletier (observed - black points) vs Oracle (predicted - red points) fecundity in relation to gonad weight
p <- ggplot(data = mackerel_bio_new, aes(x = GSI, y = fecundity)) +
  geom_point(col = "red", shape = 1) +
  xlab("GSI") +
  ylab("Fecundity (number of eggs)") +
  theme_minimal(base_size = 14)
p

p <- p + geom_point(data = mackerel_bio_new, aes(x = GSI, y = fecundity2), col = "blue", shape = 1)


p <- p + geom_point(data = mackerel_fecundity_pelletier, aes(x = GSI, y = fecundity), col = "black", shape=1)

p 

# For each combinations of Year and age, summarize mean fecundity, IC low and high, and nb of fish
Average.data <- mackerel_bio_new %>% group_by(year, age.group) %>%
  dplyr::summarize(nb.fish = n(), mean.value = mean(fecundity), sd.value = sd(fecundity))

Average.data <- as_tibble(Average.data)
Average.data <- Average.data %>% complete(year, age.group)

Average.data$IC.low <- Average.data$mean.value - (1.96*Average.data$sd.value)

Average.data$IC.up <- Average.data$mean.value + (1.96*Average.data$sd.value)

# Fill NA gap for year-age.group combination with knnImputation function from DMwR2 package
library(DMwR2)
test.data <- Average.data %>% dplyr::select(year,age.group, mean.value)

#Must be done for each age.group to be sure that the nearest neighboring value is not from another age class
# Function that fills in all NA values using the k Nearest Neighbours of each case with NA values. 
# By default it uses the values of the neighbors and obtains an weighted (by the distance to the case) average of their values to fill in the unknowns. If meth='median' it uses the median/most frequent value, instead.
Average.data.imp <- NULL
for (i in unique(test.data$age.group)) {
  data.age <- subset(test.data, age.group == i)
  Imp.data <- knnImputation(test.data, k = 5, scale = T, meth = "weighAvg",
                            distData = NULL)
  
  Average.data.imp <- rbind(Average.data.imp, Imp.data)
  
}

# graph of the average weight-at-age time series and NA gaps filled
S2<-ggplot(Average.data.imp, aes(x=year, y=mean.value, col=as.factor(age.group))) + 
  geom_line(size=1) + 
  #scale_x_continuous(breaks = seq(1970, 2022, 5)) +
  xlab("Year") +
  ylab("Average fecundity (number of eggs)") +
  scale_x_continuous(breaks = seq(1970, 2022, 5)) +
  labs(col="Age") +
  #ylim(50,850) +
  #ggtitle("4TVWX + gillnets") +
  theme_minimal(base_size = 14)
S2

S2<-ggplot(Average.data.imp, aes(x=year, y=mean.value, col=as.factor(age.group))) + 
  geom_point(size=2) + 
  geom_smooth(method = "gam") +
  #scale_x_continuous(breaks = seq(1970, 2022, 5)) +
  xlab("Year") +
  ylab("Average fecundity (number of eggs)") +
  scale_x_continuous(breaks = seq(1970, 2022, 10)) +
  labs(col="Age") +
  #ylim(50,850) +
  #ggtitle("4TVWX + gillnets") +
  theme_minimal(base_size = 14)
S2

# Heat map kind of graph to show the number of fish for each combination of year and age
g5.3<-ggplot(Average.data, aes(x = year, y = as.factor(age.group), fill = nb.fish)) +
  ylab("Age") +
  geom_tile() +
  #scale_x_continuous(breaks = seq(1970, 2022, 5)) +
  scale_fill_gradient(low = "white", high = "red") + 
  theme_minimal(base_size = 14) +
  labs(fill = "Nb fish") +
  #theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Number of fish") +
  xlab("") +
  geom_text(aes(label = nb.fish), color = "black", size = 3)

cowplot::plot_grid(g5.3, S2, align = "v", nrow = 2, rel_heights = c(1, 2))


