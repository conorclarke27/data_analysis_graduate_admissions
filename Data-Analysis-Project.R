
################## Setting working directory ######################
setwd("C:/Users/conor/Desktop/DataAnalysisProject")
mydata <- read.csv("GraduateAdmissions.csv", header = TRUE)


################## Looking at the data ########################
dim(mydata)
which(is.na(Sex))

################# Visualisation of the Data #####################
# GRE Score
boxplot(mydata$GRE.Score, ylab = "GRE Score", main = "GRE Scores")
# TOEFL Score
boxplot(mydata$TOEFL.Score, ylab = "TOEFL Score", main = "TOEFL Scores")
# University Rating
boxplot(mydata$University.Rating, ylab = "University Rating", main = "University Rating")
uni_rating <- table(mydata$University.Rating)
barplot(uni_rating, ylim = c(0,140), xlab = "University Rating (out of 5)", ylab = "Frequency", main = "University Rating")
# Statement of Purpose
boxplot(mydata$SOP, ylab = "Statement of Purpose (out of 5)", main = "Statement of Purpose")
hist(mydata$SOP, xlab = "SOP(out of 5)", main = "Statement of Purpose")
# Letter of Recommendation
boxplot(mydata$LOR, ylab = "LOR(out of 5)", main = "Letter of Recommendation")
hist(mydata$LOR)
mydata$LOR[mydata$LOR == 10] <- median(mydata$LOR) ## Changing the outlier to the median of the data
boxplot(mydata$LOR, ylab = "LOR(out of 5)", main = "Letter of Recommendation")
hist(mydata$LOR)
# CGPA
boxplot(mydata$CGPA, ylab = "UnderGraduate GPA", main = "UnderGraduate GPA")
hist(mydata$CGPA, xlab = "CGPA", main = "Under Graduate GPA")
# Research
research <- table(mydata$Research)
barplot(research, ylab = "Frequency", ylim = c(0,250), names = c("No Research", "Research"), main = "Research Experience (either 0 or 1)")
# Chance of Admittance
boxplot(mydata$Chance.of.Admit, ylab = "Chance of Admit (ranging from 0 to 1)", ylim = c(0, 1), main = "Chance of Admit.")
hist(mydata$Chance.of.Admit, xlab = "Chance of Admit. (ranging 0 to 1)", main = "Chance of Admit.")
# Sex
sexes <- table(mydata$Sex)
barplot(sexes, ylab = "Frequency", ylim = c(0,250), main = "Sex")

##################### Descriptive Statistics ########################
# Mean for Data
mean(mydata$GRE.Score)
mean(mydata$TOEFL.Score)
mean(mydata$University.Rating)
mean(mydata$SOP)
mean(mydata$LOR)
mean(mydata$CGPA)
mean(mydata$Research)
mean(mydata$Chance.of.Admit)

# Median for Data
median(mydata$GRE.Score)
median(mydata$TOEFL.Score)
median(mydata$University.Rating)
median(mydata$SOP)
median(mydata$LOR)
median(mydata$CGPA)
median(mydata$Research)
median(mydata$Chance.of.Admit)

# Standard Deviation
sd(mydata$GRE.Score)
sd(mydata$TOEFL.Score)
sd(mydata$University.Rating)
sd(mydata$SOP)
sd(mydata$LOR)
sd(mydata$CGPA)
sd(mydata$Research)
sd(mydata$Chance.of.Admit)

# Interquartile Range
IQR(mydata$GRE.Score)
IQR(mydata$TOEFL.Score)
IQR(mydata$University.Rating)
IQR(mydata$SOP)
IQR(mydata$LOR)
IQR(mydata$CGPA)
IQR(mydata$Research)
IQR(mydata$Chance.of.Admit)


#################### Research Questions ##########################
pairs(~GRE.Score+TOEFL.Score+SOP+LOR+CGPA+Chance.of.Admit, data = mydata)


boxplot(split(mydata$Chance.of.Admit, mydata$University.Rating), main = "Chance of Admittance by University Rating")
boxplot(split(mydata$Chance.of.Admit, mydata$Research), main = "Chance of Admittance by Research")
boxplot(split(mydata$Chance.of.Admit, mydata$Sex), main = "Chance of Admittance by Sex")
boxplot(split(mydata$Chance.of.Admit, mydata$SOP), main = "Chance of Admittance by SOP")
boxplot(split(mydata$Chance.of.Admit, mydata$LOR), main = "Chance of Admittance by LOR")

########## Correlation R Code with Chance of Admittance ##########
cor(mydata$Chance.of.Admit, mydata$GRE.Score)
cor(mydata$Chance.of.Admit, mydata$TOEFL.Score)
cor(mydata$Chance.of.Admit, mydata$CGPA)

admit_greScores<-lm(Chance.of.Admit~GRE.Score, data = mydata)
par(mfrow=c(2,1))
plot(admit_greScores, which=1:2)

summary(admit_greScores)

admit_toeflScores<-lm(Chance.of.Admit~TOEFL.Score, data = mydata)
par(mfrow=c(2,1))
plot(admit_toeflScores, which=1:2)

summary(admit_toeflScores)

admit_cgpa<-lm(Chance.of.Admit~CGPA, data = mydata)
par(mfrow=c(2,1))
plot(admit_cgpa, which=1:2)

summary(admit_cgpa)


#################### Hypothesis Tests #########################

boxplot(mydata$Chance.of.Admit~mydata$Sex, ylim = c(0,1), main = "Chance of Admittance by Sex")
t.test(mydata$Chance.of.Admit~mydata$Sex)

boxplot(mydata$Chance.of.Admit~mydata$Research, main = "Chance of Admittance by Research")
t.test(mydata$Chance.of.Admit~mydata$Research)

t.test(mydata$Chance.of.Admit,mydata$GRE.Score)



