###############################
# Antibiotics Associated Diarrhea Project
# Full R Script for Objectives
###############################

# Load libraries
library(dplyr)
library(ggplot2)
library(skimr)
library(psych)
library(car)
library(ggpubr)

# Load data (already loaded as AAD)
# load("AAD.RData")

###############################
# 1. Data Reading
###############################
# Inspect structure
str(AAD)
skim(AAD)

###############################
# 2. Descriptive Statistics
###############################
# Summary of continuous variables
summary(AAD[, c("D1.Shannon.diversity", "D6.Shannon.diversity", 
                "D1.Chao1.diversity", "D6.Chao1.diversity", "D1.D6.Jaccard.distance")])

# Frequency table for categorical variables
table(AAD$Antibiotic.class)
table(AAD$Outcome)

# Correlation
cor(AAD$D1.Shannon.diversity, AAD$D6.Shannon.diversity, use="complete.obs")
cor(AAD$D1.Chao1.diversity, AAD$D6.Chao1.diversity, use="complete.obs")

###############################
# 3. Graphics
###############################
# Bar chart of Outcome
ggplot(AAD, aes(x=Outcome)) +
  geom_bar(fill="steelblue") +
  theme_minimal() +
  ggtitle("Outcome Distribution")

# Bar chart mean Outcome per Antibiotic class (using proportion)
ggplot(AAD, aes(x=Antibiotic.class, fill=Outcome)) +
  geom_bar(position="fill") +
  theme_minimal() +
  ggtitle("Outcome by Antibiotic Class (Proportion)")

# Histogram of Chao diversity
ggplot(AAD, aes(x=D1.Chao1.diversity)) +
  geom_histogram(bins=30, fill="skyblue", color="black") +
  ggtitle("Histogram of D1 Chao Diversity")

ggplot(AAD, aes(x=D6.Chao1.diversity)) +
  geom_histogram(bins=30, fill="salmon", color="black") +
  ggtitle("Histogram of D6 Chao Diversity")

# Scatterplot with regression lines
ggplot(AAD, aes(x=D1.Chao1.diversity, y=D6.Chao1.diversity, color=Antibiotic.class)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  theme_minimal() +
  ggtitle("Scatterplot with Regression Lines by Antibiotic")

# Boxplot of Jaccard distance
ggplot(AAD, aes(y=D1.D6.Jaccard.distance)) +
  geom_boxplot(fill="lightgreen") +
  theme_minimal() +
  ggtitle("Boxplot of Jaccard Distance")

ggplot(AAD, aes(x=Antibiotic.class, y=D1.D6.Jaccard.distance, fill=Antibiotic.class)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Jaccard Distance per Antibiotic Class")

###############################
# 4. Outlier Detection
###############################
boxplot(AAD$D1.Chao1.diversity, main="Outlier Detection: D1 Chao")
boxplot(AAD$D6.Chao1.diversity, main="Outlier Detection: D6 Chao")
boxplot(AAD$D1.Shannon.diversity, main="Outlier Detection: D1 Shannon")
boxplot(AAD$D6.Shannon.diversity, main="Outlier Detection: D6 Shannon")

###############################
# 5. Normality & Homoscedasticity
###############################
# Normality tests
shapiro.test(AAD$D1.Chao1.diversity)
shapiro.test(AAD$D6.Chao1.diversity)

# QQ plots
qqnorm(AAD$D1.Chao1.diversity); qqline(AAD$D1.Chao1.diversity)
qqnorm(AAD$D6.Chao1.diversity); qqline(AAD$D6.Chao1.diversity)

# Homoscedasticity: Levene's Test
leveneTest(D1.Chao1.diversity ~ Antibiotic.class, data=AAD)
leveneTest(D6.Chao1.diversity ~ Antibiotic.class, data=AAD)

###############################
# 6. Statistical Inference
###############################
# Confidence intervals for Chao & Shannon by Antibiotic
by(AAD$D1.Chao1.diversity, AAD$Antibiotic.class, function(x) t.test(x, conf.level=0.90)$conf.int)
by(AAD$D1.Chao1.diversity, AAD$Antibiotic.class, function(x) t.test(x, conf.level=0.95)$conf.int)
by(AAD$D1.Chao1.diversity, AAD$Antibiotic.class, function(x) t.test(x, conf.level=0.99)$conf.int)

###############################
# 7. Hypothesis Testing
###############################
# Paired test: Day 1 vs Day 6 Shannon
with(AAD, t.test(D1.Shannon.diversity, D6.Shannon.diversity, paired=TRUE))

# Chao difference between PBL vs FQN
subset_data <- AAD %>% filter(Antibiotic.class %in% c("PBL","FQN"))
t.test(D1.Chao1.diversity ~ Antibiotic.class, data=subset_data, var.equal=FALSE)

# ANOVA across antibiotics
aov_res <- aov(D1.Chao1.diversity ~ Antibiotic.class, data=AAD)
summary(aov_res)
TukeyHSD(aov_res)

###############################
# 8. Linear Models
###############################
# Simple linear regression
lm_res <- lm(D6.Chao1.diversity ~ D1.Chao1.diversity + Antibiotic.class, data=AAD)
summary(lm_res)
confint(lm_res)

# Linear regression with repeated measures (using patient ID as random effect)
library(lme4)
lme_res <- lmer(D6.Chao1.diversity ~ D1.Chao1.diversity + Antibiotic.class + (1|Patient.ID), data=AAD)
summary(lme_res)
