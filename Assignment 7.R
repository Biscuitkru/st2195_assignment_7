library(ggplot2)

titanic <- read.csv("titanic.csv")
str(titanic)

titanic$Survived <- factor(titanic$Survived, levels = c(0,1), labels = c("No","Yes"))
titanic$Pclass <- factor(titanic$Pclass, levels = c(1,2,3), labels = c("1st","2nd","3rd"))

# 1. Generate a series of bar charts to describe the: (a) gender, (b) ticket class, 
# and (c) survival of the passengers onboard.

# a) Bar chart for Gender
ggplot(data = titanic, aes(x=Sex, fill=Sex)) + geom_bar() +
  labs(title="Passenger's Gender")

# b) Bar Chart for Ticket Class
ggplot(titanic, aes(x=Pclass, fill=Pclass)) + geom_bar() +
  ggtitle("Passenger's Ticket class") +
  xlab("Ticket class") +
  theme(legend.position="none")
## Note: xlab changes the title for the x axis, ylab changes the y axis title
## Theme is able to change the option of inputting legends or not
# b) Alternative way using dplyr for Gender bar chart(Assuming i've not factorised Pclass yet)
library(dplyr)
titanic %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=Pclass, fill=Pclass)) + 
  geom_bar() + 
  ggtitle("Ticket class of the passengers onboard") + 
  xlab("ticket class") +
  theme(legend.position="none")

# c) Bar Chart for survival
ggplot(titanic, aes(x=Survived, fill=Survived)) + geom_bar() +
  ggtitle("Survival count of the passengers onboard") +
  theme(legend.position = "none")

# 2. Generate a histogram for the passengers' age. 
# Furthermore, describe the passengers' age using the following two boxplots: 
# (i) age per ticket class, and (ii) age based on survival.

# (a) Histogram for the passengers age
ggplot(titanic, aes(x=Age)) +
  geom_histogram(fill="darkred", bins=10, na.rm=TRUE) +
  ggtitle("Age distribution of the passengers")
ggplot(titanic, aes(x=Age)) +
  geom_density(na.rm=TRUE) +
  ggtitle("Age distribution of the passengers")

## For both histogram and density
ggplot(titanic, aes(x=Age, y=..density..)) + 
  geom_histogram(fill="steelblue", bins=10, na.rm=TRUE) + 
  geom_density(na.rm=TRUE) +
  ggtitle("Distribution of age of the passengers onboard")

# (i) Boxplot for Age per ticket class
ggplot(titanic, aes(x=Pclass, y=Age, fill=Pclass)) +
  geom_boxplot(na.rm=TRUE) +
  ggtitle("Age of the passengers") + 
  xlab("Ticket Class") +
  theme(legend.position="none")

# (ii) Boxplot for Age based on survival
ggplot(titanic, aes(x=Survived, y=Age, fill=Survived)) +
  geom_boxplot(na.rm=TRUE) +
  ggtitle("Age of the passengers") +
  theme(legend.position="none")

# 3. Generate a a)histogram for the travel fare
#               b)table showing the number of people who did not pay 
# - you may want to check on Google why a handful of people was on board for free! 

# 3a. Histogram for Ticket Fare
ggplot(titanic, aes(x=Fare, y=..density..)) +
  geom_histogram(fill="steelblue", bins=30, na.rm=TRUE) +
  ggtitle("Ticket fare of the passengers")

# 3b. Table for Paid and Unpaid
paid <- table(titanic$Fare != 0)
names(paid) <- c("Didn't pay", "Paid")
paid

# 4. A chart to describe the family size per ticket class
titanic$family_size <- titanic$SibSp +titanic$Parch + 1
table(titanic$family_size)

ggplot(titanic, aes(x=family_size, y=..density.., fill=Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(Pclass ~ ., scales="free") +
  ggtitle("Family size per ticket class")

ggplot(titanic, aes(x=family_size, y=..density.., fill = Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(~ Pclass)  + 
  ggtitle("Family size per ticket class")

# 5. A series of stacked bar charts to show how survival differs for 
# a) different genders, b) different ticket classes

# a
ggplot(titanic, aes(fill=Survived, x=Sex)) +
  geom_bar(position="stack") +
  ggtitle("Survival by Gender") +
  xlab("Gender") +
  guides(fill=guide_legend("Survived"))
## Females have a better survival rate as compared to males

# b
x <- ggplot(titanic, aes(fill=Survived, x=Pclass)) +
  geom_bar(position = "stack") +
  ggtitle("Survival by Ticket Class") +
  xlab("Ticket Class") +
  guides(fill=guide_legend("Survived"))

# b with data labels for survived
x + geom_text(aes(label=..count..),
              stat="count",
              size=3,
              position="stack",
              vjust=2)

# b with data represented as totals
totals <- titanic %>% group_by(Pclass) %>% summarise(total=n())
x + geom_text(data=totals,
              aes(x=Pclass, y=total, label=total, fill=NULL),
              size = 3,
              nudge_y = 30)

# Cleaner representation of b
ggplot(titanic, aes(fill=Survived, x=Pclass)) + 
  geom_bar(position='dodge') +
  ggtitle("Survival by ticket class") + 
  xlab("ticket class") +
  geom_text(
    aes(label=..count..),
    stat="count",
    size=3,
    vjust=1.5, 
    position=position_dodge(width=0.9))

# 6. A violin chart describing how survival related to age and gender

ggplot(titanic, aes(x=Sex, y=Age, fill=Survived)) + 
  geom_violin(na.rm=TRUE, adjust=0.5)

# 7. A violin chart describing the survival rate related to age and ticket class

ggplot(titanic, aes(fill=Survived, x=Pclass, y=Age)) +
  geom_violin(na.rm=TRUE, adjust=0.5)
