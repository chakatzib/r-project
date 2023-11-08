library(tidyverse)
library(Metrics)
library(caTools)
dataset=read.csv('Movie_project.csv')

# explore
head(dataset)
glimpse(dataset)
names(dataset)
summary(dataset)
unique(dataset$rating)
str(dataset)

# missing data 
colSums(is.na(dataset))
# inpute missing data

# score
png("density_score.png")
ggplot(data=dataset, aes(score))+
  geom_density()
dev.off()
score_mean=mean(dataset$score, na.rm=TRUE)
dataset$score=ifelse(is.na(dataset$score), score_mean, dataset$score)

ggplot(data = dataset, aes(budget))+
  geom_density()

# votes
png("density_votes")
ggplot(data=dataset, aes(votes))+
  geom_density()
dev.off()
votes_mean=mean(dataset$votes, na.rm=TRUE)
dataset$votes=ifelse(is.na(dataset$votes), votes_mean, dataset$votes)

# budget
png("density_budget")
ggplot(data=dataset, aes(budget))+
  geom_density()
dev.off()
budget_mean=mean(dataset$budget, na.rm=TRUE)
dataset$budget=ifelse(is.na(dataset$budget),budget_mean, dataset$budget)

# gross
png("density_gross")
ggplot(data=dataset, aes(gross))+
  geom_density()
dev.off()
gross_median=median(dataset$gross, na.rm=TRUE)
dataset$gross=ifelse(is.na(dataset$gross), gross_median, dataset$gross)

# runtime
png("density_runtime")
ggplot(data=dataset, aes(runtime))+
  geom_density()
dev.off()
runtime_mean=mean(dataset$runtime, na.rm=TRUE)
dataset$runtime=ifelse(is.na(dataset$runtime), runtime_mean, dataset$runtime)

colSums(is.na(dataset))


data_new = dataset[c(12,13)]
set.seed(123)
split = sample.split(data_new$gross, SplitRatio = 2/3)
training_set = subset(data_new, split == TRUE)
test_set = subset(data_new, split == FALSE)

regressor = lm(formula = gross~budget, training_set)
summary(regressor)

y_pred = predict(regressor, newdata = test_set)
result = data.frame(test_set$gross, y_pred)
head(result)

ggplot(data = dataset,
       aes(x = test_set$budget,
           y = test_set$gross),
       color = 'blue')+
  geom_point()+
  geom_line(aes(x = test_set$budget,
                y = y_pred),
            color = 'red')
ggplot()+geom_point()



ggplot()+geom_point(aes(x=test_set$budget,
                        y=test_set$gross),
                    color = 'blue')+
  geom_line(aes(x=test_set$budget,
                y = y_pred),
            color='red')+
  xlab('Budget')+ylab('Gross')


#accuracy checks
mae(test_set$gross, y_pred)
mse(test_set$gross, y_pred)
rmse(test_set$gross, y_pred)
#predict
new=data.frame(budget=1000000)
predict(regressor,newdata=new)

