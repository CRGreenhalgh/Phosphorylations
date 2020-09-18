###############
## Phosp
################

## Libraries
library(tidyverse)
library(caret)
library(DataExplorer)

## Read Files
phos_train <- read_csv("train.csv")
phos_test <- read_csv("test.csv")

phos_data <- bind_rows(train = phos_train, test = phos_test, .id = "Set")

## Check classes
lapply(phos_data, class)
phos_data$Amino.Acid <- as.factor(phos_data$Amino.Acid)
class(phos_data$Amino.Acid)

###############################
## Exploratory Data Analysis ##
###############################

## get summary, plot missing and correlations
summary(phos_data)
plot_missing(phos_data)
plot_correlation(phos_data, type = "continuous",
                 cor_args = list(use = "pairwise.complete.obs"))

## Get rid of NAs and build linear model to test for collinearity
phos_noNA <- phos_train %>% filter(!is.na(Consensus))
phos_lm <- glm(Response ~., data = phos_noNA, family = "binomial")
car::vif(phos_lm)

## Get rid of every collinear variable except SVM
phos_temp <- phos_data %>% select(c(Set, Amino.Acid, Iupred.score, SVM, normalization, Response, SiteNum))
phos_temp_train <- phos_temp %>% filter(!is.na(Response))

## New missing and correlation plots
plot_missing(phos_temp)
plot_correlation(phos_temp, type = "continuous",
                 cor_args = list(use = "pairwise.complete.obs"))

ggplot(data = phos_temp_train, mapping = aes(x = SVM, y = Response, color = Amino.Acid)) + 
  geom_point() + geom_jitter(width = .9, height = .3) + geom_smooth()
