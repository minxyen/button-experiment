# E-Commerce Button Color Experiment (A/B Testing)

# Read Data
library("tidyverse")
test.table <- read_csv("data/test_table.csv")
user.table <- read_csv("data/user_table.csv")
head(test.table)
head(user.table)

# Data Verification
print(paste("[test.table] num of unique users:", 
            nrow(unique(select(test.table, user_id)))))

print(paste("[user.table] num of unique users:", 
            nrow(unique(select(user.table, user_id)))))

print(paste("[in both tables] num of unique uesrs:", 
            nrow(unique(inner_join(select(test.table, user_id),
                                   select(user.table, user_id))))))

# treatment group and control group are balanced?
print(paste("[treatment] number: ", sum(test.table$test == 1)))
print(paste("[control] number: ", sum(test.table$test == 0)))


# Check the experiment entity -> per purchase
test.table %>% count(user_id, test, name='counts')

# Join tables 
test.data <- left_join(test.table, user.table, by = "user_id")
head(test.data)

# correct each variable type
test.data$age <- as.integer(test.data$age)

# children: 0 - 14, youth: 15 - 24, adult: 25 - 64, senior: 65 
test.data$age_group <- cut(test.data$age, c(0, 14, 24, 64, Inf),
                           c("childen", "youth", "adult", "senior"),
                           include.lowest=TRUE)
test.data$age_group <- as.factor(test.data$age_group)

test.data$date <- as.Date(test.data$date, format = "%Y/%m/%d")

for (i in c(3,4,6,7,9,10)){
    test.data[, i] <- as.factor(test.data[[i]])
}

head(test.data)

# Descriptive Statistic:
summary(test.data)

test.data %>%
    group_by(test) %>%
    summarize(mean_purchase_amount = mean(purchase_amount))


test.data %>%
    group_by(device) %>%
    summarize(mean_purchase_amount = mean(purchase_amount))

test.data %>%
    group_by(gender) %>%
    summarize(mean_purchase_amount = mean(purchase_amount))

test.data %>%
    group_by(service) %>%
    summarize(mean_purchase_amount = mean(purchase_amount))

test.data %>%
    group_by(age_group) %>%
    summarize(mean_purchase_amount = mean(purchase_amount))

# Counrty - Test Cross Effect
test.data %>%
    group_by(country, test) %>%
    summarize(mean_purchase_amount = mean(purchase_amount))


# Data Analysis
# t test. Welch Two Sample t-test (獨立的樣本t檢定)
t.test(test.data[test.data$test==1, ]$purchase_amount,
    test.data[test.data$test==0, ]$purchase_amount,
    alternative = 'greater')

# Analysis of Variance, ANOVA
# model 01
aov.model <- aov(purchase_amount ~ 
                 test + country + device + gender + service + age_group,
                 test.data)
summary(aov.model)

# model 02
aov.model <- aov(purchase_amount ~ 
                     test*country + test*device + test*service, 
                 test.data)
summary(aov.model)

# model 03
interaction.model <- aov(purchase_amount ~ 
                         test*country + device + service,
                         test.data)
summary(interaction.model)

# Tukey Test
TukeyHSD(interaction.model, "test")
TukeyHSD(interaction.model, "country") 


# Result Presentation - Data Viz
# Treatment Group v.s. Control Group
daily.purchase <- test.data %>%
    group_by(date, test) %>%
    summarise(purcahse_amount = mean(purchase_amount))

# Method 1)  Time Series - differences per day
ggplot(daily.purchase, aes(x=date, y=purcahse_amount, color=test)) +
    geom_line() +
    geom_point() +
    ylim(c(30,50)) +
    labs(x="Date", y="Purcahse Amount", 
         title = "Time Series Plot of Purchase Amount: Treatment versus Control") +
    theme_bw()

# Method 2) Density Plot on both groups
ggplot(test.data, aes(x=purchase_amount, fill=test, color=test)) +
    geom_density(alpha = 0.3) +
    labs(x="Purchase Amount", y="Density", 
         title="Density Plot of Purchase Amount: Treatment v.s. Control") +
    theme_bw()

# County & Test Cross Effect

# simply country
ggplot(test.data, aes(x=country, y=purchase_amount)) +
    geom_boxplot()

# country - test cross effect
ggplot(test.data, aes(x=country, y=purchase_amount, color=test)) +
    geom_boxplot() +
    labs(x="Country", y="Purchase Amount",
         title = "Country:Test Cross Effect") +
    theme_bw()

