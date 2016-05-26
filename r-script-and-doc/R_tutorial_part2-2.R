library(rms)
library(ggplot2)
library(car)
library(dplyr)
library(stargazer)

# data from day 1
alldata <- read.csv("alldata.csv")

## RESEARCH QUESTION 3: cog1 ~ dx (t-test)

## Wow we have a result!!
## But one of my favorite things output R is that the statistical output can be saved as an object!! 
my.t.result <- t.test(data=alldata, cog1 ~ dx) # saves to output to my.t.result

print(my.t.result)       ## prints to output to the console
my.t.result$statistic    ## gets us the t statistic!
my.t.result$parameter    ## the degrees of freedom
my.t.result$p.value      ## gets us the p-value

round(my.t.result$statistic,2) ## we can these numbers using the "round" function

## let's put these three together into something we might want to report in our paper
my.t.results.txt = paste0('t(',
                        round(my.t.result$parameter,1),
                        ') = ',
                        round(my.t.result$statistic,2), ', p = ',
                        round(my.t.result$p.value, 7))

# make it fancier:
## first - let's deal with the NA's we don't want to plot - 
## let's remove them from the plotting dataset
data.toplot <-filter(alldata, !is.na(cog1), !is.na(dx))

## even fancier - let's add a title, and annotation and label the axes
ggplot(data.toplot, aes(y=cog1,x=dx)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(alpha=0.5) +
  labs(title="Effect of Diagnosis on Cog Score #1",
       y = "Cognitive Test 1",
       x = "Diagnosis") +
  annotate("text", label = my.t.results.txt, x = 1, y = 21) +
  theme_bw()


## NOW LET's save our plot!!!
## Note: we can start by using the "Export" button in the plots tab..
ggsave('figure1_ttestresults.pdf', width = 5, height = 5)

## Let's make a diagnosis by cognition table
my.stats.table <- summarise(alldata,
                            "Mean" = mean(cog1, na.rm = T),
                            "St Dev" = sd(cog1, na.rm = T))

stargazer(my.stats.table, summary=F, type="text")

my.stats.table <- alldata %>% 
                  group_by(dx) %>%
                  summarise("Mean" = mean(cog1, na.rm = T),
                            "St Dev" = sd(cog1, na.rm = T))

stargazer(my.stats.table, summary=F, type="text")
stargazer(my.stats.table, summary=F, type="html",out="my.stats.table.html")


## RESEARCH AIM 4: total_behaviour_score ~ age (linear regression)
# calculate a composite variable by combining multiple variables
# note new variables can be made easily

# this is the base package way
alldata$totalcog1 <- (alldata$cog1 + alldata$cog3)/alldata$cog2

# using dplyr's mutate verb
alldata <- mutate(alldata, 
                  totalcog = cog1 + cog3 / cog2)

# simple linear regression (two ways: base package and rms)

lm.base <- lm(data=alldata, totalcog ~ age)
lm.rms <- ols(data=alldata, totalcog ~ age)

# compare outputs

lm.base
summary(lm.base)
anova(lm.base)

stargazer(lm.base, type="text")
stargazer(lm.base, type="html", out="lm.base.html")

# to make the most out of rms package functionality, we need to store summary
# stats using the datadist() function. That way, when we call summary() on an
# ols() object (we just made one called "lm.rms") it will give us useful info. 

dd.alldata <- datadist(alldata)
options(datadist="dd.alldata")

lm.rms
summary(lm.rms)
anova(lm.rms)

# visualize results using ggplot2

ggplot(data=alldata, aes(y=totalcog, x=age)) + 
	geom_point() + 
	geom_smooth(method=lm)

ggsave("lm.plot.pdf",  width = 5, height = 5)
