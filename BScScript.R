#Codes Bachelor Thesis on SGMY
library(gvlma)
library(haven)
library(psych)
library(foreign)
library(tidyverse)
library(tidyr)
library(dplyr)
library(janitor)
library(broom)
library(vtree)
library (openxlsx)
library(car)
library(readxl)
library(writexl)
library (modelr)
library (ltm)
library(ggplot2)
library(Hmisc)
library(reshape)
library(ez)
library(dplyr)
library(lme4)
library(lmtest)

# PRE ANALYSE #

setwd("/Users/aleczirnheld/Desktop/University/Thesis/Dataset")

data <- read_excel("Data30112023.2.xlsx")

## delete unnecessary colums ## 
data$StartDate <- NULL
data$EndDate <- NULL
data$Status <- NULL
data$IPAddress <- NULL
data$`Duration (in seconds)` <- NULL
data$Finished <- NULL
data$RecordedDate <- NULL
#data$ResponseId <- NULL
data$DistributionChannel<- NULL
data$UserLanguage <- NULL
data$Q_RecaptchaScore <- NULL
data$ExternalReference <- NULL
data$Q_StraightliningQuestions <- NULL
data$Q_StraightliningCount <- NULL
data$Q_StraightliningPercentage <- NULL
data$id <- NULL
data$Q_UnansweredPercentage <- NULL
data$Q_UnansweredQuestions <- NULL

#delete unnecessary row
datagood <- data [-c (1),]

# Removing people who are CISHETALLO, <16, >27 and/or answered NO to the 
data1 <- datagood[datagood$Consent != "2.0", ]
data1 <- data1[data1$Age >= "16.0", ]
data1 <- data1[data1$Age <= "27.0", ]
data1 <- subset(data1, !(Gender == 1 & Sex == 2.0 & Sexuality == 1))
data1 <- subset(data1, !(Gender == 2 & Sex == 1.0 & Sexuality == 1))
data1$Progress <- as.numeric(as.character(data1$Progress))
ProgressNotDone <- data1[data1$Progress < 69, ]
data1 <- data1[data1$Progress >= 70, ]
data1 <- data1 %>%
  filter_all(any_vars(!is.na(.)))

#Examining Progress not Done
ProgressNotDone<- ProgressNotDone %>%
  filter_all(any_vars(!is.na(.)))


####### SASI #######
data2 <- data1
#Combine SASI Sexuality Scores
SASI <- data1[,c(19:23)]
#Turn SASI into numeric scores
SASI <- lapply(SASI, function(x) as.numeric(as.character(x)))
SASI=data.frame(SASI) 

#Calculate mean and Sum sasi scores
data2$SASI_mean <- rowMeans(SASI)
data2$SASI_sum <- rowSums(SASI)


#chronbach's Alpha for SASI
psych::alpha(SASI)
#raw_alpha= 0,88

####### SAGI #######

SAGI <- data1[,c(40:44)]

SAGI <- lapply(SAGI, function(x) as.numeric(as.character(x)))
SAGI=data.frame(SAGI) 

data2$SAGI_mean <- rowMeans(SAGI)
data2$SAGI_sum <- rowSums(SAGI)



#chronbach's Alpha for
psych::alpha(SAGI)
#rawalpha 


####### Victimisation #######
#Sum All Victimisation into final score from 0-6
Victim <- data1[,c(13:18)]
Victim <- lapply(Victim, function(x) as.numeric(as.character(x)))
Victim=data.frame(Victim) 

data2$Victim_sum <- rowSums(Victim)


#chronbach's Alpha for Victimisation
psych::alpha(Victim)
#raw_alpha: 0,66
#std alpha: 0,69


####### SPRide #######
SPride <-data1[,c(24:31)]
SPride <- lapply(SPride, function(x) as.numeric(as.character(x)))
SPride=data.frame(SPride)

data2$SPride_mean <- rowMeans(SPride)
data2$SPride_sum <- rowSums(SPride)


#chronbach's Alpha
psych::alpha(SPride)

####### GPride #######

GPride <-data1[,c(32:39)]
GPride <- lapply(GPride, function(x) as.numeric(as.character(x)))
GPride=data.frame(GPride)

data2$GPride_mean <- rowMeans(GPride)
data2$GPride_sum <- rowSums(GPride)

summary(GPride)


#chronbach's Alpha
psych::alpha(GPride)

####### SF_12 #######
#Rename SF-12 Column
names(data2)[names(data2)=="SF-12"] <- "SF_12"
# Convert 'SF_12' column to numeric
data2$SF_12 <- as.numeric(as.character(data2$SF_12))

####### SGM Self-Acceptance #######
# Function to mean the sum rows of two columns if both are filled, otherwise copy the value
mean_or_copy <- function(row) {
  if (!is.na(row["SASI_mean"]) && !is.na(row["SAGI_mean"])) {
    return((as.numeric(row["SASI_mean"]) + as.numeric(row["SAGI_mean"])) / 2)
  } else if (!is.na(row["SASI_mean"])) {
    return(as.numeric(row["SASI_mean"]))
  } else if (!is.na(row["SAGI_mean"])) {
    return(as.numeric(row["SAGI_mean"]))
  } else {
    return(NA)
  }
}

# Apply the function row-wise
SASISGM <- apply(data2, 1, mean_or_copy)
data2$SASISGM <- SASISGM


####### SGM Pride #######
# Function to sum rows of two columns if both are filled, otherwise copy the value
mean_or_copy2 <- function(row) {
  if (!is.na(row["SPride_mean"]) && !is.na(row["GPride_mean"])) {
    return((as.numeric(row["SPride_mean"]) + as.numeric(row["GPride_mean"])) / 2)
  } else if (!is.na(row["SPride_mean"])) {
    return(as.numeric(row["SPride_mean"]))
  } else if (!is.na(row["GPride_mean"])) {
    return(as.numeric(row["GPride_mean"]))
  } else {
    return(NA)
  }
}

# Apply the function row-wise
PrideSGM <- apply(data2, 1, mean_or_copy2)
data2$PrideSGM <- PrideSGM

####Creating Clean Dataset#####
CleanData <- data2 %>%
  dplyr::select(ResponseId, Age, Gender, Sexuality, Sex, Education, Country, SF_12, Victim_sum, SASI_mean, SASI_sum, SAGI_mean, SAGI_sum, SASISGM, PrideSGM, GPride_mean, GPride_sum, SPride_mean, SPride_sum, Vict_1, Vict_2, Vict_3, Vict_4, Vict_5, Vict_6)

CleanData <- CleanData %>%
  filter(!is.na(SASISGM) & !is.na(SF_12) & !is.na(Victim_sum) & !is.na(PrideSGM))

CleanData$Sex <- as.numeric(as.character(CleanData$Sex))

#Assigning groups to people #1 are ONLY gender minorities, #2 are only sexual minority, #3 are sexual AND gender minority.
CleanData <- CleanData %>%
  mutate(Group = ifelse(!is.na(SPride_mean) & !is.na(GPride_mean), 3,
                        ifelse(!is.na(SPride_mean), 2,
                               ifelse(!is.na(GPride_mean), 1, NA))))

table(CleanData$Group)

####DEMOGRAPHICS####
## Gender Columns ##
#1=Female, 2= Male, 3= Cis, 4 =Dyadic, 4 = Inter, 6 =Nonbindary, 7 = Questioning, 8 = trans, 9 = self-identify, 10 = not answer
Gender <- CleanData %>%
  separate_rows(Gender, sep = ",") %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n) * 100)

## Sexuality Columns ## 
#1 hetero, 2 asexual, 3bisexual, 4 gay, 5 lesbian, 6 pansexual, 7 questionning, 8 self identify, 9 not answer
Sexuality <- CleanData %>%
  separate_rows(Sexuality, sep = ",") %>%
  count(Sexuality) %>%
  mutate(Percentage = n / sum(n) * 100)

## Sex Columns ## 
Sex <- CleanData %>% count(Sex) %>% 
  mutate(percentage = n / sum(n)*100)

#COuntry , 122 = Netherlands, 137 =Poland, 187 = USA, 185 = UK, 31 = Canada, 61 = France
Country <- CleanData %>% count(Country) %>% 
  mutate(percentage = n / sum(n)*100)

## Age ##
CleanData$Age <- as.numeric(as.character(CleanData$Age))
sd(CleanData$Age, na.rm = TRUE) 
#2.723668
mean(CleanData$Age, na.rm = TRUE)
# 21.38211
summary(CleanData$Age)

## Education ## 
#1 Some primary school, 2 completed primary, 3 some secondary school, 4 completed secondary, 5 vocational or similar, 6 some university (no degree), 7 bachelors degree, 8 graduate or professional degree ((MA, MS, MBA, PhD, JD, MD, DDS etc.), 9 prefer not to say
Education <- CleanData %>% count(Education) %>% 
  mutate(percentage = n / sum(n)*100)

#Making Subgroups
OnlyVariablesSGMY <- CleanData[ , c("SF_12", "Victim_sum", "SASISGM", "PrideSGM", "Age")]
OnlyVariablesSM <- CleanData[ , c("SF_12", "Victim_sum", "SASI_mean", "SPride_mean", "Age")]
OnlyVariablesGM <- CleanData[ , c("SF_12", "Victim_sum","SAGI_mean", "GPride_mean", "Age")]
OnlyVariablesGM <- na.omit(OnlyVariablesGM)
OnlyVariablesSM <- na.omit(OnlyVariablesSM)
OnlyVariablesSGMY <- na.omit(OnlyVariablesSGMY)

#### DESCRIPTIVE DATA FOR ALL SCALES ####
#SASI S

CleanData %>% summarise(mean=mean(SASI_mean, na.rm = TRUE),
                   sd=sd(SASI_mean, na.rm = TRUE),
                   min=min(SASI_mean, na.rm = TRUE),
                   max=max(SASI_mean, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$SASI_mean)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(CleanData$SASI_mean, main = "SASI")
shapiro.test(CleanData$SASI_mean) #not normal

#SAGI

CleanData %>% summarise(mean=mean(SAGI_mean, na.rm = TRUE),
                   sd=sd(SAGI_mean, na.rm = TRUE),
                   min=min(SAGI_mean, na.rm = TRUE),
                   max=max(SAGI_mean, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$SAGI_mean)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(CleanData$SAGI_mean, main = "SAGI")
shapiro.test(CleanData$SAGI_mean) #Not normally distributed

#Self-Acceptance SGM
CleanData %>% summarise(mean=mean(SASISGM, na.rm = TRUE),
                    sd=sd(SASISGM, na.rm = TRUE),
                    min=min(SASISGM, na.rm = TRUE),
                    max=max(SASISGM, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$SASISGM)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(CleanData$SASISGM, main = "SASI SGM")
shapiro.test(CleanData$SASISGM) #Not normally distributed


#VICTIM SGM
CleanData %>% summarise(mean=mean(Victim_sum, na.rm = TRUE),
                    sd=sd(Victim_sum, na.rm = TRUE),
                    min=min(Victim_sum, na.rm = TRUE),
                    max=max(Victim_sum, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$Victim_sum)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

result <- CleanData %>%
  count(Victim_sum) %>%
  mutate(Percentage = n / sum(n) * 100)

hist(CleanData$Victim_sum, main = "Victimisation Score")
shapiro.test(CleanData$Victim_sum) #Not normally distributed

hist(Victim$Vict_1, main = "Victim")


#Trial - APA7 Conform Graphs
library(tidyverse)

CleanData %>%
  ggplot(aes(x = factor(Victim_sum), y = ..count..)) +
  geom_bar(width = 0.7, color = "black") +
  scale_y_continuous(expand = expansion(0), limits = c(0, 60)) +
  scale_x_discrete(breaks = c(0, 1, 2, 3, 4, 5, 6), labels = c(0, 1, 2, 3, 4, 5, 6)) +  # Specify x-axis breaks and labels
  labs(
    x = "Victimisation Score",
    y = "Frequency",
    title = "Victimisation"
  ) +
  geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, colour = "white") + 
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.20, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )


ggsave("VictimisationBarChart.png", width = 10, height = 7, dpi = 300)


# Reshape the data into long format
victim_long <- CleanData[, c("Vict_1", "Vict_2", "Vict_3", "Vict_4", "Vict_5", "Vict_6")]
victim_long <- gather(victim_long, key = "Variable", value = "Value", Vict_1:Vict_6)
str(victim_long$Value)



# Create histograms using ggplot2 with custom variable labels

library(dplyr)

victim_long <- victim_long %>%
  mutate(Variable = gsub("Vict_", "Item ", Variable))

ggplot(victim_long, aes(x = Value, fill = Variable)) +
  geom_bar(position = position_dodge(width = 0.5), alpha = 0.6) +
  geom_text(stat = "count", aes(label = ..count..), vjust = 1.2, color = "black") +
  scale_x_discrete(labels = c("No", "Yes")) +  # Use scale_x_discrete for discrete x-axis
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Histograms of Victimisation Items",
       x = "Presence of Victimisation") +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black", face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit = "cm"
    )
  )


ggsave("VictimisationItemsChart.png", width = 10, height = 7, dpi = 300)


#SFH SGM

CleanData %>% summarise(mean=mean(SF_12, na.rm = TRUE),
                    sd=sd(SF_12, na.rm = TRUE),
                    min=min(SF_12, na.rm = TRUE),
                    max=max(SF_12, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$SF_12)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(CleanData$SF_12, main = "Histogram of Scale Variable")
shapiro.test(CleanData$SF_12) #Not normally distributed


#SF_12 GM
OnlyVariablesGM %>% summarise(mean=mean(SF_12, na.rm = TRUE),
                        sd=sd(SF_12, na.rm = TRUE),
                        min=min(SF_12, na.rm = TRUE),
                        max=max(SF_12, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(OnlyVariablesGM$SF_12)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(OnlyVariablesGM$SF_12, main = "Histogram of Scale Variable")


#SF_12 SM
OnlyVariablesSM %>% summarise(mean=mean(SF_12, na.rm = TRUE),
                              sd=sd(SF_12, na.rm = TRUE),
                              min=min(SF_12, na.rm = TRUE),
                              max=max(SF_12, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(OnlyVariablesSM$SF_12)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(OnlyVariablesSM$SF_12, main = "Histogram of Scale Variable")

rcorr(OnlyVariablesSM$SF_12)

#Pride Sexuality
CleanData %>% summarise(mean=mean(SPride_mean, na.rm = TRUE),
                    sd=sd(SPride_mean, na.rm = TRUE),
                    min=min(SPride_mean, na.rm = TRUE),
                    max=max(SPride_mean, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$SPride_mean)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(CleanData$SPride_mean, main = "Sexuality Pride")
shapiro.test(CleanData$SPride_mean) #Marginally Not normally distributed


#Pride Gender
CleanData %>% summarise(mean=mean(GPride_mean, na.rm = TRUE),
                    sd=sd(GPride_mean, na.rm = TRUE),
                    min=min(GPride_mean, na.rm = TRUE),
                    max=max(GPride_mean, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$GPride_mean)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(CleanData$GPride_mean, main = "Gender Pride")
shapiro.test(CleanData$GPride_mean) #Normally distributed

#Pride SGM

CleanData %>% summarise(mean=mean(PrideSGM, na.rm = TRUE),
                    sd=sd(PrideSGM, na.rm = TRUE),
                    min=min(PrideSGM, na.rm = TRUE),
                    max=max(PrideSGM, na.rm = TRUE)) %>%
  mutate(
    n = sum(!is.na(CleanData$PrideSGM)),  # Calculate number of non-NA values
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,  # Calculate lower CI
    upper_ci = mean + qt(0.975, df = n - 1) * se   # Calculate upper CI
  )

hist(CleanData$PrideSGM, main = "SGM Pride")
shapiro.test(CleanData$PrideSGM) #Not normally distributed


library(ggcorrplot)
library(corrplot)

#SGMY

SGMY.cor = rcorr(as.matrix(OnlyVariablesSGMY), type = "spearman")

ggcorrplot(SGMY.cor$r)
corrplot(SGMY.cor$r)

#SM

SMY.cor = rcorr(as.matrix(OnlyVariablesSM), type = "spearman")
ggcorrplot(SMY.cor$r)
corrplot(SMY.cor$r)

#GM

GMY.cor = rcorr(as.matrix(OnlyVariablesGM), type = "spearman")
ggcorrplot(GMY.cor$r)
corrplot(GMY.cor$r)

#### Hypothesis 1 Testing ####
VictHealth <- CleanData %>% lm(SF_12 ~ Victim_sum, data = .)
summary(VictHealth) #Not Significant, p=0,8

##Linear relationships #VIOLATED
#Model1 -> NOT LINEAR
plot(VictHealth, which = 1)

## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,08, meaning that they are independent.
durbinWatsonTest(VictHealth)

## 3rd Normality of residuals #VIOLATED
plot(VictHealth, which = 2)

shapiro.test(resid(VictHealth)) #p-value = 4.075e-07, meaning that the residuals vary significantly from a normal distribution
hist(resid(VictHealth))

##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictHealth, which = 1) #Residuals vs. Fitted Values => SUPER VIOLATED
plot(VictHealth, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictHealth)
bptest(VictHealth)

library(Hmisc)
rcorr(CleanData$SF_12, CleanData$Victim_sum, type = "spearman")
cor.test(CleanData$SF_12, CleanData$Victim_sum, method = "spearman", exact = FALSE) #Not Significant

#### H2a - Evaluating Hypothesis of SF_12 and SASI + Victim for SGMY ####
VictSASI <- CleanData %>% lm(SF_12 ~ Victim_sum * SASISGM, data = .)
summary(VictSASI)
tidy(VictSASI)


#Assumptions

CleanData <- CleanData %>% 
  add_residuals(VictSASI)
CleanData <- CleanData %>% 
  add_predictions(VictSASI)

##Linear relationships #VIOLATED
plot(VictSASI, which = 1)

CleanData%>% 
  add_residuals(VictSASI) %>% 
  ggplot(aes(x = SASISGM, y = resid)) + 
  geom_point()


## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,01, meaning that they are not.
durbinWatsonTest(VictSASI)
vif(VictSASI) 
#VIF values greater than 5 or 10 could indicate multicollinearity issues among predictors. 
#Victimisation has VIF higher than 5 & 10


## 3rd Normality of residuals #Skewed slightly to the right
plot(VictSASI, which = 2) 

hist(resid(VictSASI)) #SKEWED SLIGHTLY to the right

CleanData %>% 
  add_residuals(VictSASI) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram()

shapiro.test(resid(VictSASI)) #p = 0,008, meaning that the residuals vary significantly from a normal distribution


##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictSASI, which = 1) #Residuals vs. Fitted Values 
plot(VictSASI, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictSASI)
bptest(VictSASI)


#### H2b - Evaluating Hypothesis for SGMY ####
VictPride <- CleanData %>% lm(SF_12 ~ Victim_sum + PrideSGM + Victim_sum:PrideSGM, data = .)
summary(VictPride)

VictPride %>%  tidy()


#Assumptions

CleanData <- CleanData %>% 
  add_residuals(VictPride)
CleanData <- CleanData %>% 
  add_predictions(VictPride)

##Linear relationships #VIOLATED
plot(VictPride, which = 1)

CleanData%>% 
  add_residuals(VictPride) %>% 
  ggplot(aes(x = SASISGM, y = resid)) + 
  geom_point()


## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,09, meaning that they are.-> victsum is >20
durbinWatsonTest(VictPride)
vif(VictPride) 
#VIF values greater than 5 or 10 could indicate multicollinearity issues among predictors. 
#Victimisation has VIF higher than 5 & 10


## 3rd Normality of residuals #Skewed slightly to the right
plot(VictPride, which = 2) 

hist(resid(VictPride)) #SKEWED SLIGHTLY to the right

CleanData %>% 
  add_residuals(VictPride) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram()

shapiro.test(resid(VictPride)) #p = 0,001, meaning that the residuals vary significantly from a normal distribution


##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictPride, which = 1) #Residuals vs. Fitted Values 
plot(VictPride, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictPride)#p=0,4
bptest(VictPride) #p=0,07


#### H2a - Evaluating SEXUALI MINTORIY Hypothesis of SF_12 and SASI + Victim for SGMY ####
VictSASIS <- CleanData %>% lm(SF_12 ~ Victim_sum * SASI_mean, data = .)
summary(VictSASIS)
tidy(VictSASIS)

#Assumptions

CleanData <- CleanData %>% 
  add_residuals(VictSASIS)
CleanData <- CleanData %>% 
  add_predictions(VictSASIS)

##Linear relationships #VIOLATED
plot(VictSASIS, which = 1)

CleanData%>% 
  add_residuals(VictSASIS) %>% 
  ggplot(aes(x = SASI_mean, y = resid)) + 
  geom_point()


## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,01, meaning that they are not.
durbinWatsonTest(VictSASIS)
vif(VictSASIS) 
#VIF values greater than 5 or 10 could indicate multicollinearity issues among predictors. 
#Victimisation has VIF higher than 5 & 10


## 3rd Normality of residuals #Skewed slightly to the right, but okay
plot(VictSASIS, which = 2) 

hist(resid(VictSASIS)) #SKEWED SLIGHTLY to the right

CleanData %>% 
  add_residuals(VictSASIS) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram()

shapiro.test(resid(VictSASIS)) #p = 0,152, meaning that the residuals DO NOT significantly from a normal distribution


##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictSASIS, which = 1) #Residuals vs. Fitted Values 
plot(VictSASIS, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictSASIS)
bptest(VictSASIS)


#### H2b - Evaluating Hypothesis Pride for SEXUALITY ####
VictPrideS <- CleanData %>% lm(SF_12 ~ Victim_sum + SPride_mean + Victim_sum:SPride_mean, data = .)
summary(VictPrideS)

VictPrideS %>%  tidy()


#Assumptions

CleanData <- CleanData %>% 
  add_residuals(VictPrideS)
CleanData <- CleanData %>% 
  add_predictions(VictPrideS)

##Linear relationships #VIOLATED
plot(VictPrideS, which = 1)

CleanData%>% 
  add_residuals(VictPrideS) %>% 
  ggplot(aes(x = SASI_mean, y = resid)) + 
  geom_point()


## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,09, meaning that they are.-> victsum is >20
durbinWatsonTest(VictPrideS)
vif(VictPrideS) 
#VIF values greater than 5 or 10 could indicate multicollinearity issues among predictors. 
#Victimisation has VIF higher than 5 & 10


## 3rd Normality of residuals #Skewed slightly to the left
plot(VictPrideS, which = 2) 

hist(resid(VictPrideS)) #SKEWED SLIGHTLY to the right

CleanData %>% 
  add_residuals(VictPrideS) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram()

shapiro.test(resid(VictPrideS)) #p = 0,002, meaning that the residuals vary significantly from a normal distribution


##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictPrideS, which = 1) #Residuals vs. Fitted Values 
plot(VictPrideS, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictPrideS)#p=0,66
bptest(VictPrideS) #p=0,07



#### H2a - Evaluating Gender MINTORIY Hypothesis of SF_12 and SASI + Victim for SGMY ####
VictSASIG <- CleanData %>% lm(SF_12 ~ Victim_sum * SAGI_mean, data = .)
summary(VictSASIG)
tidy(VictSASIG)

#Assumptions

CleanData <- CleanData %>% 
  add_residuals(VictSASIG)
CleanData <- CleanData %>% 
  add_predictions(VictSASIG)

##Linear relationships #VIOLATED
plot(VictSASIG, which = 1)

CleanData%>% 
  add_residuals(VictSASIG) %>% 
  ggplot(aes(x = SAGI_mean, y = resid)) + 
  geom_point()


## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,01, meaning that they are not.
durbinWatsonTest(VictSASIG)
vif(VictSASIG) 
#VIF values greater than 5 or 10 could indicate multicollinearity issues among predictors. 
#Victimisation has VIF higher than 5 & 10


## 3rd Normality of residuals #Skewed slightly to the right, but okay
plot(VictSASIG, which = 2) 

hist(resid(VictSASIG)) #SKEWED SLIGHTLY to the right

CleanData %>% 
  add_residuals(VictSASIG) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram()

shapiro.test(resid(VictSASIG)) #p = 0,152, meaning that the residuals DO NOT significantly from a normal distribution


##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictSASIG, which = 1) #Residuals vs. Fitted Values 
plot(VictSASIG, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictSASIG)
bptest(VictSASIG)


#### H2b - Evaluating Pride Hypothesis for Gender ####
VictPrideG <- CleanData %>% lm(SF_12 ~ Victim_sum + GPride_mean + Victim_sum:GPride_mean, data = .)
summary(VictPrideG)

VictPrideG %>%  tidy()


#Assumptions

CleanData <- CleanData %>% 
  add_residuals(VictPrideG)
CleanData <- CleanData %>% 
  add_predictions(VictPrideG)

##Linear relationships #VIOLATED
plot(VictPrideG, which = 1)

CleanData%>% 
  add_residuals(VictPrideG) %>% 
  ggplot(aes(x = GPride_mean, y = resid)) + 
  geom_point()


## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,09, meaning that they are.-> victsum is >20
durbinWatsonTest(VictPrideG)
vif(VictPrideG) 
#VIF values greater than 5 or 10 could indicate multicollinearity issues among predictors. 
#Victimisation has VIF higher than 5 & 10


## 3rd Normality of residuals #Skewed slightly to the left
plot(VictPrideG, which = 2) 

hist(resid(VictPrideG)) #SKEWED SLIGHTLY to the right

CleanData %>% 
  add_residuals(VictPrideG) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram()

shapiro.test(resid(VictPrideG)) #p = 0,002, meaning that the residuals vary significantly from a normal distribution


##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictPrideG, which = 1) #Residuals vs. Fitted Values 
plot(VictPrideG, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictPrideG)#p=0,7
bptest(VictPrideG) #p=0,06

#### Hypothesis 1 Testing = WITH Vitc1####
VictVerb <- CleanData %>% lm(SF_12 ~ factor(Vict_1), data = .)
summary(VictVerb) #Not Significant, p=0,8

##Linear relationships #VIOLATED
#Model1 -> NOT LINEAR
plot(VictVerb, which = 1)

## 2nd No Multicollinearity = independence tested using Durbin Watson Test. p = 0,08, meaning that they are independent.
durbinWatsonTest(VictVerb)

## 3rd Normality of residuals #VIOLATED
plot(VictVerb, which = 2)

shapiro.test(resid(VictVerb)) #p-value = 4.075e-07, meaning that the residuals vary significantly from a normal distribution
hist(resid(VictVerb))

##4th Homoscedasticity (Equal/Constant Variance) => NOT CONSTANT

# Residuals vs. Fitted plot
plot(VictVerb, which = 1) #Residuals vs. Fitted Values => SUPER VIOLATED
plot(VictVerb, which = 3) #using Square Root of Standardized Residuals => SUPER VIOLATED

ncvTest(VictVerb)
bptest(VictVerb)

library(Hmisc)
rcorr(CleanData$SF_12, CleanData$Vict_1, type = "spearman")

cor.test(CleanData$SF_12, CleanData$Vict_1, method = "spearman", exact = FALSE) #Not Significant

